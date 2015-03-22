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

    while stack.Count <> 0 do
      match stack.Pop() with
      | RestoreGen g -> gen.Close(); gen <- g
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
          | Lambda (var, body) ->
              let baseType = fsharpFuncType var.Type body.Type
              let baseCtor =
                baseType.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], null)
              // TODO : unique name
              let lambda =
                parentMod.DefineType("lambda0", TypeAttributes.Public, baseType, [])

              let ctor = lambda.DefineConstructor(MethodAttributes.Public, [])
              let ctorGen = ctor.GetILGenerator()
              ctorGen.Emit(Ldarg_0)
              ctorGen.Emit(Call (Ctor baseCtor))
              ctorGen.Emit(Ret)

              let invoke =
                lambda.DefineOverrideMethod(baseType, "Invoke", MethodAttributes.Public, var.Type, [ body.Type ])
              let invokeGen = invoke.GetILGenerator()
              stack.Push(Compiling (fun gen -> gen.Emit(Newobj ctor.RawBuilder)))
              stack.Push(RestoreGen gen)
              gen <- invokeGen
              stack.Push(Compiling (fun gen -> gen.Emit(Ret); lambda.CreateType() |> ignore))
              stack.Push(CompileTarget body)
          | Call (None, mi, argsExprs) ->
              MethodCallEmitter.emit (mi, argsExprs) stack
          | Call (Some recv, mi, argsExprs) ->
              MethodCallEmitter.emit (mi, recv::argsExprs) stack
          | PropertyGet (Some recv, pi, argsExprs) ->
              MethodCallEmitter.emit (pi.GetMethod, recv::argsExprs) stack
          | FieldGet (None, fi) ->
              gen.Emit(Ldsfld fi)
          | Value (null, _) ->
              gen.Emit(Ldnull)
          | Value (value, typ) ->
              if typ = typeof<int> then
                emitLoadInteger<int> value gen
              elif typ = typeof<bool> then
                emitLoadInteger<int> (if unbox<bool> value then 1 else 0) gen
              elif typ = typeof<string> then
                gen.Emit(Ldstr (unbox<string> value))
              else
                failwithf "unsupported value type: %A" typ
          | Var _ ->
              // TODO : impl
              gen.Emit(Ldarg_1)
          | expr ->
              failwithf "unsupported expr: %A" expr

    gen.Emit(Ret)

    gen.Close()

    let x = Activator.CreateInstance(typ.CreateType()) :?> ICompiledType<'T>
    x.ExecuteCompiledCode()