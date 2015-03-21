namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System
open System.Reflection
open System.Reflection.Emit

module ExprCompiler =
  let inline emitLoadInteger< ^TInteger when ^TInteger : (static member op_Explicit: ^TInteger -> int) > (value: obj) (gen: ILGenerator) =
    match int (unbox< ^TInteger > value) with
    | -1 -> gen.Emit(OpCodes.Ldc_I4_M1)
    | 0 -> gen.Emit(OpCodes.Ldc_I4_0)
    | 1 -> gen.Emit(OpCodes.Ldc_I4_1)
    | 2 -> gen.Emit(OpCodes.Ldc_I4_2)
    | 3 -> gen.Emit(OpCodes.Ldc_I4_3)
    | 4 -> gen.Emit(OpCodes.Ldc_I4_4)
    | 5 -> gen.Emit(OpCodes.Ldc_I4_5)
    | 6 -> gen.Emit(OpCodes.Ldc_I4_6)
    | 7 -> gen.Emit(OpCodes.Ldc_I4_7)
    | 8 -> gen.Emit(OpCodes.Ldc_I4_8)
    | i when 9 <= i && i <= 127 ->
        gen.Emit(OpCodes.Ldc_I4_S, i)
    | i ->
        gen.Emit(OpCodes.Ldc_I4, i)

  type ICompiledType<'T> =
    abstract member ExecuteCompiledCode: unit -> 'T

  let fsharpFuncType argType retType =
    typedefof<FSharpFunc<_, _>>.MakeGenericType([|argType; retType|])

  let compile (expr: Expr<'T>) : 'T =
    let asm =
      AppDomain.CurrentDomain.DefineDynamicAssembly(
        AssemblyName("CompiledAssembly"),
        AssemblyBuilderAccess.Run)
    let parentMod = asm.DefineDynamicModule("CompiledModule")
    let typ = parentMod.DefineType("CompiledType", TypeAttributes.Public, typeof<obj>, [| typeof<ICompiledType<'T>> |])
    let m = typ.DefineMethod("ExecuteCompiledCode", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<'T>, [||])
    typ.DefineMethodOverride(m, typeof<ICompiledType<'T>>.GetMethod("ExecuteCompiledCode"))

    let mutable gen = m.GetILGenerator()

    let stack = CompileStack()
    stack.Push(CompileTarget expr)

    while stack.Count <> 0 do
      match stack.Pop() with
      | RestoreGen g -> gen <- g
      | Compiling f -> f gen
      | CompilingIfThenElse (falseLabel, ifEndLabel, cond, truePart, falsePart) ->
          match cond, truePart, falsePart with
          | NotYet cond, _, _ ->
              stack.Push(CompilingIfThenElse (falseLabel, ifEndLabel, Done, truePart, falsePart))
              stack.Push(CompileTarget cond)
          | Done, NotYet truePart, _ ->
              gen.Emit(OpCodes.Brfalse, falseLabel)
              stack.Push(CompilingIfThenElse (falseLabel, ifEndLabel, Done, Done, falsePart))
              stack.Push(CompileTarget truePart)
          | Done, Done, NotYet falsePart ->
              gen.Emit(OpCodes.Br, ifEndLabel)
              gen.MarkLabel(falseLabel)
              stack.Push(CompilingIfThenElse (falseLabel, ifEndLabel, Done, Done, Done))
              stack.Push(CompileTarget falsePart)
          | Done, Done, Done ->
              gen.MarkLabel(ifEndLabel)
      | CompileTarget target ->
          match target with
          | IfThenElse (cond, truePart, falsePart) ->
              stack.Push(CompilingIfThenElse (gen.DefineLabel(), gen.DefineLabel(), NotYet cond, NotYet truePart, NotYet falsePart))
          | Lambda (var, body) ->
              let baseType = fsharpFuncType var.Type body.Type
              let baseCtor =
                baseType.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], null)
              // TODO : unique name
              let lambda =
                parentMod.DefineType("lambda0", TypeAttributes.Public, baseType)

              let ctor = lambda.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
              let ctorGen = ctor.GetILGenerator()
              ctorGen.Emit(OpCodes.Ldarg_0)
              ctorGen.Emit(OpCodes.Call, baseCtor)
              ctorGen.Emit(OpCodes.Ret)

              let invoke =
                lambda.DefineMethod("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual, var.Type, [| body.Type |])
              lambda.DefineMethodOverride(invoke, baseType.GetMethod("Invoke"))
              let invokeGen = invoke.GetILGenerator()
              stack.Push(Compiling (fun gen -> gen.Emit(OpCodes.Newobj, ctor)))
              stack.Push(RestoreGen gen)
              gen <- invokeGen
              stack.Push(Compiling (fun gen -> gen.Emit(OpCodes.Ret); lambda.CreateType() |> ignore))
              stack.Push(CompileTarget body)
          | Call (None, mi, argsExprs) ->
              MethodCallEmitter.emit (mi, argsExprs) stack
          | Call (Some recv, mi, argsExprs) ->
              MethodCallEmitter.emit (mi, recv::argsExprs) stack
          | PropertyGet (Some recv, pi, argsExprs) ->
              MethodCallEmitter.emit (pi.GetMethod, recv::argsExprs) stack
          | FieldGet (None, fi) ->
              gen.Emit(OpCodes.Ldsfld, fi)
          | Value (null, _) ->
              gen.Emit(OpCodes.Ldnull)
          | Value (value, typ) ->
              if typ = typeof<int> then
                emitLoadInteger<int> value gen
              elif typ = typeof<bool> then
                emitLoadInteger<int> (if unbox<bool> value then 1 else 0) gen
              elif typ = typeof<string> then
                gen.Emit(OpCodes.Ldstr, unbox<string> value)
              else
                failwithf "unsupported value type: %A" typ
          | Var _ ->
              // TODO : impl
              gen.Emit(OpCodes.Ldarg_1)
          | expr ->
              failwithf "unsupported expr: %A" expr

    gen.Emit(OpCodes.Ret)

    let x = Activator.CreateInstance(typ.CreateType()) :?> ICompiledType<'T>
    x.ExecuteCompiledCode()