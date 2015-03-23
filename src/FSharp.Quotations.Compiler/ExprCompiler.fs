namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System
open System.Reflection
open System.Reflection.Emit

module ExprCompiler =

  type VariableInfo =
    | Arg of int
    | Local of LocalBuilder
    | Field of FieldInfo

  type VariableEnv = (string * Type * VariableInfo) list

  let inline emitLoadInteger< ^TInteger when ^TInteger : (static member op_Explicit: ^TInteger -> int) > (value: obj) (gen: ILGeneratorWrapper) =
    match int (unbox< ^TInteger > value) with
    | -1 -> gen.Emit(Ldc_I4_M1)
    | 0 -> gen.Emit(Ldc_I4_0)
    | 1 -> gen.Emit(Ldc_I4_1)
    | 2 -> gen.Emit(Ldc_I4_2)
    | 3 -> gen.Emit(Ldc_I4_3)
    | 4 -> gen.Emit(Ldc_I4_4)
    | 5 -> gen.Emit(Ldc_I4_5)
    | 6 -> gen.Emit(Ldc_I4_6)
    | 7 -> gen.Emit(Ldc_I4_7)
    | 8 -> gen.Emit(Ldc_I4_8)
    | i when 9 <= i && i <= 127 ->
        gen.Emit(Ldc_I4_S i)
    | i ->
        gen.Emit(Ldc_I4 i)

  type ICompiledType<'T> =
    abstract member ExecuteCompiledCode: unit -> 'T

  let fsharpFuncType argType retType =
    typedefof<FSharpFunc<_, _>>.MakeGenericType([|argType; retType|])

  let compile (expr: Expr<'T>) : 'T =
    let asm =
      AppDomain.CurrentDomain.DefineDynamicAssembly(
        AssemblyName("CompiledAssembly"),
        AssemblyBuilderAccess.Run)
    let parentMod = ModuleBuilderWrapper.Create(asm, "CompiledModule")
    let typ = parentMod.DefineType("CompiledType", TypeAttributes.Public, typeof<obj>, [typeof<ICompiledType<'T>>])
    let m = typ.DefineOverrideMethod(typeof<ICompiledType<'T>>, "ExecuteCompiledCode", MethodAttributes.Public, typeof<'T>, [])

    let mutable gen = m.GetILGenerator()

    let stack = CompileStack()
    stack.Push(CompileTarget expr)

    let mutable lambdaCount = 0

    let varEnv: VariableEnv ref = ref []

    try
      while stack.Count <> 0 do
        match stack.Pop() with
        | RestoreGen g -> gen.Close(); gen <- g
        | Assumed f ->
            if stack.Count = 0 then
              f (False, gen)
            else
              match stack.Pop() with
              | Assumption a -> f (a, gen)
              | other ->
                  f (False, gen)
                  stack.Push(other)
        | Assumption _ -> () // do nothing
        | Compiling f -> f gen
        | CompilingIfThenElse (falseLabel, ifEndLabel, cond, truePart, falsePart) ->
            match cond, truePart, falsePart with
            | NotYet cond, _, _ ->
                stack.Push(CompilingIfThenElse (falseLabel, ifEndLabel, Done, truePart, falsePart))
                stack.Push(CompileTarget cond)
            | Done, NotYet truePart, _ ->
                gen.Emit(Brfalse falseLabel)
                stack.Push(CompilingIfThenElse (falseLabel, ifEndLabel, Done, Done, falsePart))
                stack.Push(CompileTarget truePart)
            | Done, Done, NotYet falsePart ->
                gen.Emit(Br ifEndLabel)
                gen.MarkLabel(falseLabel)
                stack.Push(CompilingIfThenElse (falseLabel, ifEndLabel, Done, Done, Done))
                stack.Push(CompileTarget falsePart)
            | Done, Done, Done ->
                gen.MarkLabel(ifEndLabel)
        | CompileTarget target ->
            match target with
            | Sequential (e1, e2) ->
                stack.Push(CompileTarget e2)
                stack.Push(Assumption IfSequential)
                stack.Push(CompileTarget e1)
            | IfThenElse (cond, truePart, falsePart) ->
                stack.Push(CompilingIfThenElse (gen.DefineLabel(), gen.DefineLabel(), NotYet cond, NotYet truePart, NotYet falsePart))
            | TryWith (body, _, _, e, exnHandler) ->
                let res = gen.DeclareLocal(body.Type)
                gen.BeginExceptionBlock() |> ignore
                stack.Push(Compiling (fun gen -> gen.Emit(Stloc res); gen.EndExceptionBlock(); gen.Emit(Ldloc res)))
                stack.Push(CompileTarget exnHandler)
                stack.Push(Compiling (fun gen -> gen.Emit(Stloc res); gen.BeginCatchBlock(e.Type)))
                stack.Push(CompileTarget body)
            | TryFinally (body, handler) ->
                let res = gen.DeclareLocal(body.Type)
                gen.BeginExceptionBlock() |> ignore
                stack.Push(Compiling (fun gen -> gen.EndExceptionBlock(); gen.Emit(Ldloc res)))
                stack.Push(CompileTarget handler)
                stack.Push(Compiling (fun gen -> gen.Emit(Stloc res); gen.BeginFinallyBlock()))
                stack.Push(CompileTarget body)
            | Let (var, expr, body) ->
                stack.Push(Compiling (fun _ ->
                  varEnv := (!varEnv).Tail
                ))
                stack.Push(CompileTarget body)
                let local = gen.DeclareLocal(var.Type)
                stack.Push(Compiling (fun gen -> gen.Emit(Stloc local)))
                stack.Push(Compiling (fun _ ->
                  varEnv := (var.Name, var.Type, Local local)::(!varEnv)
                ))
                stack.Push(CompileTarget expr)
            | LetRecursive (varAndExprList, body) ->
                stack.Push(Compiling (fun _ ->
                  varEnv := (!varEnv) |> Seq.skip varAndExprList.Length |> Seq.toList
                ))
                stack.Push(CompileTarget body)
                for var, expr in varAndExprList do
                  let local = gen.DeclareLocal(var.Type)
                  stack.Push(Compiling (fun gen -> gen.Emit(Stloc local)))
                  stack.Push(Compiling (fun _ ->
                    varEnv := (var.Name, var.Type, Local local)::(!varEnv)
                  ))
                  stack.Push(CompileTarget expr)
            | Lambda (var, body) ->
                let baseType = fsharpFuncType var.Type body.Type
                let baseCtor =
                  baseType.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], null)
                let lambda =
                  parentMod.DefineType("lambda" + (string lambdaCount), TypeAttributes.Public, baseType, [])
                lambdaCount <- lambdaCount + 1

                let needVarInfos =
                  !varEnv
                  |> List.fold (fun acc (n, t, info) -> if List.forall (fun (n2, _, _) -> n <> n2) acc then (n, t, info)::acc else acc) []
                  |> List.rev
                let varTypes = needVarInfos |> List.map (fun (_, t, _) -> t)
                let ctor = lambda.DefineConstructor(MethodAttributes.Public, varTypes)
                let ctorGen = ctor.GetILGenerator()
                ctorGen.Emit(Ldarg_0)
                ctorGen.Emit(Call (Ctor baseCtor))
                let fields =
                  List.map (fun (name, typ, _) -> lambda.DefineField(name, typ, FieldAttributes.Private)) needVarInfos
                for i, field in List.zip [1..fields.Length] fields do
                  ctorGen.Emit(Ldarg_0)
                  ctorGen.Emit(Ldarg i)
                  ctorGen.Emit(Stfld field)
                ctorGen.Emit(Ret)
                ctorGen.Close()

                let invoke =
                  lambda.DefineOverrideMethod(baseType, "Invoke", MethodAttributes.Public, body.Type, [ var.Type ])
                let invokeGen = invoke.GetILGenerator()
                let orgVarEnv = !varEnv
                stack.Push(Compiling (fun _ ->
                  varEnv := orgVarEnv
                ))
                stack.Push(Compiling (fun gen ->
                  for _, _, info in needVarInfos do
                    match info with
                    | Arg i -> gen.Emit(Ldarg i)
                    | Local local -> gen.Emit(Ldloc local)
                    | Field fi -> gen.Emit(Ldfld fi)
                  gen.Emit(Newobj ctor.RawBuilder)
                ))
                stack.Push(RestoreGen gen)
                gen <- invokeGen
                stack.Push(Compiling (fun gen -> gen.Emit(Ret); lambda.CreateType() |> ignore))
                stack.Push(CompileTarget body)
                let newVarEnv =
                  List.zip needVarInfos fields
                  |> List.map (fun ((name, typ, _), fi) -> (name, typ, Field fi))
                stack.Push(Compiling (fun _gen ->
                  varEnv := (var.Name, var.Type, Arg 1)::newVarEnv
                ))
            | Application (fExpr, argExpr) ->
                stack.Push(Compiling (fun gen ->
                  if stack.Count = 0 then
                    gen.Emit(Tailcall)
                  gen.Emit(Callvirt (Method (fExpr.Type.GetMethod("Invoke"))))
                ))
                stack.Push(CompileTarget argExpr)
                stack.Push(CompileTarget fExpr)
            | Call (None, mi, argsExprs) ->
                MethodCallEmitter.emit (mi, argsExprs) stack
            | Call (Some recv, mi, argsExprs) ->
                MethodCallEmitter.emit (mi, recv::argsExprs) stack
            | PropertyGet (Some recv, pi, argsExprs) ->
                MethodCallEmitter.emit (pi.GetMethod, recv::argsExprs) stack
            | PropertySet (Some recv, pi, argsExprs, expr) ->
                MethodCallEmitter.emit (pi.SetMethod, recv::(argsExprs @ [expr])) stack
            | FieldGet (None, fi) ->
                gen.Emit(Ldsfld fi)
            | NewObject (ctor, argsExprs) ->
                stack.Push(Compiling (fun gen ->
                  gen.Emit(Newobj ctor)
                ))
                argsExprs |> List.rev |> List.iter (fun argExpr -> stack.Push(CompileTarget argExpr))
            | NewArray (typ, elems) ->
                let count = elems.Length
                emitLoadInteger<int> count gen
                gen.Emit(Newarr typ)

                for e, i in List.zip elems [0..count - 1] |> List.rev do
                  stack.Push(Compiling (fun gen ->
                    gen.Emit(Stelem typ)
                  ))
                  stack.Push(CompileTarget e)
                  stack.Push(Compiling (fun gen ->
                    gen.Emit(Dup)
                    emitLoadInteger<int> i gen
                  ))
            | Value (null, _) ->
                stack.Push(Assumed (function IfSequential, _gen -> () | _, gen -> gen.Emit(Ldnull)))
            | Value (value, typ) ->
                if typ = typeof<int> then
                  emitLoadInteger<int> value gen
                elif typ = typeof<bool> then
                  emitLoadInteger<int> (if unbox<bool> value then 1 else 0) gen
                elif typ = typeof<string> then
                  gen.Emit(Ldstr (unbox<string> value))
                else
                  gen.Close()
                  failwithf "unsupported value type: %A" typ
            | Var v ->
                match List.pick (fun (n, _, info) -> if n = v.Name then Some info else None) !varEnv with
                | Arg 0 -> gen.Emit(Ldarg_0)
                | Arg 1 -> gen.Emit(Ldarg_1)
                | Arg 2 -> gen.Emit(Ldarg_2)
                | Arg 3 -> gen.Emit(Ldarg_3)
                | Arg idx -> gen.Emit(Ldarg idx)
                | Local local -> gen.Emit(Ldloc local)
                | Field fi -> gen.Emit(Ldarg_0); gen.Emit(Ldfld fi)
            | VarSet (v, expr) ->
                stack.Push(Compiling (fun gen ->
                  match List.pick (fun (n, _, info) -> if n = v.Name then Some info else None) !varEnv with
                  | Arg idx -> gen.Emit(Starg idx)
                  | Local local -> gen.Emit(Stloc local)
                  | Field fi -> gen.Emit(Ldarg_0); gen.Emit(Stfld fi)
                ))
                stack.Push(CompileTarget expr)
            | TypeTest (expr, typ) ->
                if typ = typeof<int> then
                  stack.Push(Compiling (fun gen ->
                    gen.Emit(Call (Method MethodCallEmitter.typeTestGenericInt32MethodInfo))
                  ))
                else
                  stack.Push(Compiling (fun gen ->
                    gen.Emit(Isinst typ)
                  ))
                stack.Push(CompileTarget expr)
            | Coerce (expr, typ) ->
                if typ = typeof<obj> then
                  if expr.Type.IsValueType then
                    stack.Push(Compiling (fun gen ->
                      gen.Emit(Box expr.Type)
                    ))
                elif expr.Type.IsValueType then
                  stack.Push(Compiling (fun gen ->
                    gen.Emit(Box expr.Type)
                    gen.Emit(Unbox_Any typ)
                  ))
                stack.Push(CompileTarget expr)
            | expr ->
                gen.Close()
                failwithf "unsupported expr: %A" expr

      gen.Emit(Ret)
    finally
      // clean up all gen
      while stack.Count <> 0 do
        match stack.Pop() with
        | RestoreGen g -> g.Close()
        | _ -> ()
      gen.Close()

    let x = Activator.CreateInstance(typ.CreateType()) :?> ICompiledType<'T>
    x.ExecuteCompiledCode()