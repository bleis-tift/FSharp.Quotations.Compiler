namespace FSharp.Quotations.Compiler

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System
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

  let compile (expr: Expr<'T>) : 'T =
    let m = DynamicMethod("compiledMethod", typeof<'T>, [||])

    let gen = m.GetILGenerator()

    let stack = CompileStack()
    stack.Push(CompileTarget expr)

    while stack.Count <> 0 do
      match stack.Pop() with
      | Compiling f -> f gen
      | CompileTarget target ->
          match target with
          | Call (None, mi, argsExprs) ->
              MethodCallEmitter.emit (mi, argsExprs) stack
          | Value (value, typ) ->
              if typ = typeof<int> then
                emitLoadInteger<int> value gen
              elif typ = typeof<bool> then
                emitLoadInteger<int> (if unbox<bool> value then 1 else 0) gen
              else
                failwithf "unsupported value type: %A" typ
          | expr ->
              failwithf "unsupported expr: %A" expr

    gen.Emit(OpCodes.Ret)
    let f : Func<'T> = unbox (m.CreateDelegate(typeof<Func<'T>>))
    f.Invoke()