namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System
open System.Reflection
open System.Reflection.Emit

module ExprCompiler =

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
    stack.Push(Compiling (fun gen -> gen.Emit(Ret)))
    stack.Push(Assumption IfRet)
    stack.Push(CompileTarget expr)

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
        | CompileTarget target ->
            match target with
            | Sequential (e1, e2) ->
                stack.Push(CompileTarget e2)
                stack.Push(Assumption IfSequential)
                stack.Push(CompileTarget e1)
            | IfThenElse (cond, truePart, falsePart) ->
                let falseLabel = gen.DefineLabel()
                let ifEndLabel = gen.DefineLabel()

                let assumptionOpt =
                  if stack.Count <> 0 then
                    match stack.Pop() with
                    | Assumption _ as assumption -> Some assumption
                    | other -> stack.Push(other); None
                  else
                    None
                match assumptionOpt with
                | Some a ->
                    stack.Push(Compiling (fun gen ->
                      gen.MarkLabel(ifEndLabel)
                    ))
                    stack.Push(a)
                | None ->
                    stack.Push(Compiling (fun gen ->
                      gen.MarkLabel(ifEndLabel)
                    ))
                stack.Push(CompileTarget falsePart)
                match assumptionOpt with
                | Some a ->
                    stack.Push(Compiling (fun gen ->
                      gen.Emit(Br ifEndLabel)
                      gen.MarkLabel(falseLabel)
                    ))
                    stack.Push(a)
                | None ->
                    stack.Push(Compiling (fun gen ->
                      gen.Emit(Br ifEndLabel)
                      gen.MarkLabel(falseLabel)
                    ))
                stack.Push(CompileTarget truePart)
                stack.Push(Compiling (fun gen ->
                  gen.Emit(Brfalse falseLabel)
                ))
                stack.Push(CompileTarget cond)
            | Lambda (var, TryWith (body, _, _, e, exnHandler)) when var.Type = typeof<unit> ->
                gen <- LambdaEmitter.emit parentMod (gen, varEnv, var, body.Type) (Compiling (fun gen ->
                  let res = gen.DeclareLocal("$res", body.Type)
                  let label = gen.BeginExceptionBlock()
                  stack.Push(Compiling (fun gen ->
                    gen.Emit(ILOpCode.stloc res "$res")
                    gen.Emit(Leave label)
                    gen.EndExceptionBlock()
                    gen.Emit(ILOpCode.ldloc res "$res")))
                  stack.Push(Compiling (fun _ ->
                    varEnv := (!varEnv).Tail
                  ))
                  stack.Push(CompileTarget exnHandler)
                  let local = gen.DeclareLocal(e.Name, e.Type)
                  stack.Push(Compiling (fun gen -> gen.Emit(ILOpCode.stloc local e.Name)))
                  stack.Push(Compiling (fun _ ->
                    varEnv := (e.Name, e.Type, Local (local, e.Name)) :: (!varEnv)
                  ))
                  stack.Push(Compiling (fun gen -> gen.Emit(ILOpCode.stloc res "$res"); gen.Emit(Leave label); gen.BeginCatchBlock(e.Type)))
                  stack.Push(CompileTarget body)
                )) stack
            | TryWith _ as tryWithExpr ->
                stack.Push(CompileTarget (Expr.Application(Expr.Lambda(Var("unitVar", typeof<unit>), tryWithExpr), <@ () @>)))
            | Lambda (var, TryFinally (body, handler)) when var.Type = typeof<unit> ->
                gen <- LambdaEmitter.emit parentMod (gen, varEnv, var, body.Type) (Compiling (fun gen ->
                  let res = gen.DeclareLocal("$res", body.Type)
                  let label = gen.BeginExceptionBlock()
                  stack.Push(Compiling (fun gen -> gen.Emit(Endfinally); gen.EndExceptionBlock(); gen.Emit(ILOpCode.ldloc res "$res")))
                  stack.Push(CompileTarget handler)
                  stack.Push(Compiling (fun gen -> gen.Emit(ILOpCode.stloc res "$res"); gen.Emit(Leave label); gen.BeginFinallyBlock()))
                  stack.Push(CompileTarget body)
                )) stack
            | TryFinally _ as tryFinallyExpr ->
                stack.Push(CompileTarget (Expr.Application(Expr.Lambda(Var("unitVar", typeof<unit>), tryFinallyExpr), <@ () @>)))
            | Let (var, expr, body) ->
                stack.Push(Compiling (fun _ ->
                  varEnv := (!varEnv).Tail
                ))
                stack.Push(CompileTarget body)
                let local = gen.DeclareLocal(var.Name, var.Type)
                stack.Push(Compiling (fun gen -> gen.Emit(ILOpCode.stloc local var.Name)))
                stack.Push(Compiling (fun _ ->
                  varEnv := (var.Name, var.Type, Local (local, var.Name))::(!varEnv)
                ))
                stack.Push(CompileTarget expr)
            | LetRecursive (varAndExprList, body) ->
                stack.Push(Compiling (fun _ ->
                  varEnv := (!varEnv) |> Seq.skip varAndExprList.Length |> Seq.toList
                ))
                stack.Push(CompileTarget body)
                for var, expr in varAndExprList do
                  let local = gen.DeclareLocal(var.Name, var.Type)
                  stack.Push(Compiling (fun gen -> gen.Emit(ILOpCode.stloc local var.Name)))
                  stack.Push(Compiling (fun _ ->
                    varEnv := (var.Name, var.Type, Local (local, var.Name))::(!varEnv)
                  ))
                  stack.Push(CompileTarget expr)
            | Lambda (var, body) ->
                gen <- LambdaEmitter.emit parentMod (gen, varEnv, var, body.Type) (CompileTarget body) stack
            | Application (fExpr, argExpr) ->
                MethodCallEmitter.emit (fExpr.Type.GetMethod("Invoke"), [fExpr; argExpr]) stack
            | Call (None, mi, argsExprs) ->
                MethodCallEmitter.emit (mi, argsExprs) stack
            | Call (Some recv, mi, argsExprs) ->
                MethodCallEmitter.emit (mi, recv::argsExprs) stack
            | PropertyGet (None, pi, argsExprs) ->
                MethodCallEmitter.emit (pi.GetMethod, argsExprs) stack
            | PropertyGet (Some recv, pi, argsExprs) ->
                MethodCallEmitter.emit (pi.GetMethod, recv::argsExprs) stack
            | PropertySet (None, pi, argsExprs, expr) ->
                MethodCallEmitter.emit (pi.SetMethod, (argsExprs @ [expr])) stack
            | PropertySet (Some recv, pi, argsExprs, expr) ->
                MethodCallEmitter.emit (pi.SetMethod, recv::(argsExprs @ [expr])) stack
            | FieldSet (None, fi, expr) ->
                stack.Push(Compiling (fun gen ->
                  gen.Emit(Stfld fi)
                ))
                stack.Push(CompileTarget expr)
            | FieldSet (Some recv, fi, expr) ->
                stack.Push(Compiling (fun gen ->
                  gen.Emit(Stfld fi)
                ))
                stack.Push(CompileTarget expr)
                stack.Push(CompileTarget recv)
            | FieldGet (None, fi) ->
                gen.Emit(Ldsfld fi)
            | FieldGet (Some recv, fi) ->
                stack.Push(Compiling (fun gen ->
                  gen.Emit(Ldfld fi)
                ))
                stack.Push(CompileTarget recv)
            | TupleGet (expr, idx) when idx < 7 ->
                let pi = expr.Type.GetProperty("Item" + string (idx + 1))
                MethodCallEmitter.emit (pi.GetMethod, [expr]) stack
            | TupleGet (expr, idx) ->
                let restCount = idx / 7 - 1
                let itemN = idx % 7 + 1
                let pi = expr.Type.GetProperty("Rest")
                let itemPi: PropertyInfo ref = ref null
                stack.Push(Assumed (function
                                    | IfRet, gen -> gen.Emit(Tailcall); gen.Emit(Call (Method (!itemPi).GetMethod))
                                    | _, gen -> gen.Emit(Call (Method (!itemPi).GetMethod))))
                stack.Push(Compiling (fun gen ->
                  gen.Emit(Call (Method pi.GetMethod))
                  let typ = ref pi.PropertyType
                  for _ in 1..restCount do
                    let pi = (!typ).GetProperty("Rest")
                    gen.Emit(Call (Method pi.GetMethod))
                    typ := pi.PropertyType
                  itemPi := (!typ).GetProperty("Item" + string itemN)
                ))
                stack.Push(CompileTarget expr)
            | NewTuple (elems) ->
                TupleEmitter.emit elems stack
            | NewUnionCase (case, argsExprs) ->
                let typ = case.DeclaringType
                match case.GetFields() with
                | [||] ->
                    let pi = typ.GetProperty(case.Name, typ)
                    MethodCallEmitter.emit (pi.GetMethod, argsExprs) stack
                | _fields ->
                    let mi =
                      match typ.GetMethod(case.Name) with
                      | null -> typ.GetMethod("New" + case.Name)
                      | other -> other
                    MethodCallEmitter.emit (mi, argsExprs) stack
            | NewRecord (typ, argsExprs) ->
                let ctor = typ.GetConstructor(argsExprs |> List.map (fun e -> e.Type) |> List.toArray)
                stack.Push(Compiling (fun gen ->
                  gen.Emit(Newobj ctor)
                ))
                argsExprs |> List.rev |> List.iter (fun argExpr -> stack.Push(CompileTarget argExpr))
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
                  failwithf "unsupported value type: %A" typ
            | DefaultValue typ ->
                let local = gen.DeclareLocal("$defaultValue", typ)
                gen.Emit(ILOpCode.ldloca local "$defaultValue")
                gen.Emit(Initobj typ)
                gen.Emit(ILOpCode.ldloc local "$defaultValue")
            | Var v ->
                match List.pick (fun (n, _, info) -> if n = v.Name then Some info else None) !varEnv with
                | Arg 0 -> gen.Emit(Ldarg_0)
                | Arg 1 -> gen.Emit(Ldarg_1)
                | Arg 2 -> gen.Emit(Ldarg_2)
                | Arg 3 -> gen.Emit(Ldarg_3)
                | Arg idx -> gen.Emit(Ldarg idx)
                | Local (local, name) -> gen.Emit(ILOpCode.ldloc local name)
                | Field fi -> gen.Emit(Ldarg_0); gen.Emit(Ldfld fi)
            | VarSet (v, expr) ->
                stack.Push(Compiling (fun gen ->
                  match List.pick (fun (n, _, info) -> if n = v.Name then Some info else None) !varEnv with
                  | Arg idx -> gen.Emit(Starg idx)
                  | Local (local, name) -> gen.Emit(ILOpCode.stloc local name)
                  | Field fi -> gen.Emit(Ldarg_0); gen.Emit(Stfld fi)
                ))
                stack.Push(CompileTarget expr)
            | UnionCaseTest (expr, case) ->
                let typ = case.DeclaringType
                let prop = typ.GetProperty("Is" + case.Name)
                MethodCallEmitter.emit (prop.GetMethod, [expr]) stack
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
                failwithf "unsupported expr: %A" expr
    finally
      // clean up all gen
      while stack.Count <> 0 do
        match stack.Pop() with
        | RestoreGen g -> g.Close()
        | _ -> ()
      gen.Close()

    let x = Activator.CreateInstance(typ.CreateType()) :?> ICompiledType<'T>
    x.ExecuteCompiledCode()