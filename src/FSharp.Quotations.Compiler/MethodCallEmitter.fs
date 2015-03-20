namespace FSharp.Quotations.Compiler

open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Quotations.Patterns

open System.Collections.Generic
open System.Runtime.CompilerServices

module internal MethodCallEmitter =

  let private getMethod = function
  | Call (_, mi, _) -> mi
  | expr -> failwithf "expr is not Method call: %A" expr

  let private identityEqualityComparer =
    { new IEqualityComparer<MethodInfo> with
        member __.Equals(x, y) = (x = y)
        member __.GetHashCode(x) = RuntimeHelpers.GetHashCode(x) }

  let doNothing (_: ILGenerator) = ()

  let emitOneOpCode opcode (gen: ILGenerator) = gen.Emit(opcode)
  let emitTwoOpCodes (opcode1, opcode2) (gen: ILGenerator) = gen.Emit(opcode1); gen.Emit(opcode2)

  let private altEmitterTable1 =
    let dict = Dictionary<MethodInfo, (ILGenerator -> unit)>(identityEqualityComparer)
    dict.Add(getMethod <@ +(1) @>, doNothing)
    dict.Add(getMethod <@ -(1) @>, emitOneOpCode OpCodes.Neg)
    dict.Add(getMethod <@ 1 - 1 @>, emitOneOpCode OpCodes.Sub)
    dict.Add(getMethod <@ 1 / 1 @>, emitOneOpCode OpCodes.Div)
    dict.Add(getMethod <@ 1 % 1 @>, emitOneOpCode OpCodes.Rem)
    dict.Add(getMethod <@ 1 &&& 1 @>, emitOneOpCode OpCodes.And)
    dict.Add(getMethod <@ 1 ||| 1 @>, emitOneOpCode OpCodes.Or)
    dict.Add(getMethod <@ 1 ^^^ 1 @>, emitOneOpCode OpCodes.Xor)
    dict.Add(getMethod <@ 1 >>> 1 @>, emitOneOpCode OpCodes.Shr)
    dict.Add(getMethod <@ 1 <<< 1 @>, emitOneOpCode OpCodes.Shl)
    dict.Add(getMethod <@ ~~~1 @>, emitOneOpCode OpCodes.Not)
    dict.Add(getMethod <@ byte 1 @>, doNothing)
    dict.Add(getMethod <@ sbyte 1 @>, doNothing)
    dict :> IReadOnlyDictionary<_, _>

  open Microsoft.FSharp.Core.Operators.Checked

  let private altEmitterTable2 =
    let dict = Dictionary<MethodInfo, (ILGenerator -> unit)>(identityEqualityComparer)
    dict.Add(getMethod <@ -(1) @>, emitTwoOpCodes (OpCodes.Ldc_I4_M1, OpCodes.Mul_Ovf))
    dict.Add(getMethod <@ 1 - 1 @>, emitOneOpCode OpCodes.Sub_Ovf)
    dict.Add(getMethod <@ 1 * 1 @>, emitOneOpCode OpCodes.Mul_Ovf)
    dict :> IReadOnlyDictionary<_, _>

  let emit (mi: MethodInfo) isTailCall (gen: ILGenerator) =
    match altEmitterTable1.TryGetValue(mi) with
    | true, emitter -> emitter gen
    | _ ->
        match altEmitterTable2.TryGetValue(mi) with
        | true, emitter -> emitter gen
        | _ ->
            if isTailCall then
              gen.Emit(OpCodes.Tailcall)
            gen.EmitCall(OpCodes.Call, mi, null)