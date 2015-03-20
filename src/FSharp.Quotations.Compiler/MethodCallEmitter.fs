namespace FSharp.Quotations.Compiler

open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Quotations
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

  let emitCall mi (gen: ILGenerator) = gen.EmitCall(OpCodes.Call, mi, null)

  let private (|>>) emit1 emit2 = (fun (gen: ILGenerator) -> emit1 gen; emit2 gen)

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
    dict.Add(getMethod <@ char 1 @>, doNothing)
    dict.Add(getMethod <@ decimal 1 @>, emitCall (getMethod <@ System.Convert.ToDecimal(1) @>))
    dict.Add(getMethod <@ float 1 @>, emitOneOpCode OpCodes.Conv_R8)
    dict.Add(getMethod <@ float32 1 @>, emitOneOpCode OpCodes.Conv_R4)
    dict.Add(getMethod <@ int 1 @>, doNothing)
    dict.Add(getMethod <@ int16 1 @>, doNothing)
    dict.Add(getMethod <@ uint16 1 @>, doNothing)
    dict.Add(getMethod <@ int32 1 @>, doNothing)
    dict.Add(getMethod <@ uint32 1 @>, doNothing)
    dict.Add(getMethod <@ int64 1 @>, emitOneOpCode OpCodes.Conv_I8)
    dict.Add(getMethod <@ uint64 1 @>, emitOneOpCode OpCodes.Conv_I8)
    dict.Add(getMethod <@ nativeint 1 @>, emitOneOpCode OpCodes.Conv_I)
    dict.Add(getMethod <@ unativeint 1 @>, emitOneOpCode OpCodes.Conv_I)
    dict.Add(getMethod <@ byte "" @>, emitCall (getMethod <@ LanguagePrimitives.ParseUInt32("") @>)
                                      |>> emitOneOpCode OpCodes.Conv_Ovf_U1)
    dict.Add(getMethod <@ sbyte "" @>, emitCall (getMethod <@ LanguagePrimitives.ParseInt32("") @>)
                                       |>> emitOneOpCode OpCodes.Conv_Ovf_I1)
    dict.Add(getMethod <@ char "" @>, emitCall (getMethod <@ System.Char.Parse("") @>))
    dict :> IReadOnlyDictionary<_, _>

  open Microsoft.FSharp.Core.Operators.Checked

  let private altEmitterTable2 =
    let dict = Dictionary<MethodInfo, (ILGenerator -> unit)>(identityEqualityComparer)
    dict.Add(getMethod <@ -(1) @>, emitTwoOpCodes (OpCodes.Ldc_I4_M1, OpCodes.Mul_Ovf))
    dict.Add(getMethod <@ 1 - 1 @>, emitOneOpCode OpCodes.Sub_Ovf)
    dict.Add(getMethod <@ 1 * 1 @>, emitOneOpCode OpCodes.Mul_Ovf)
    dict.Add(getMethod <@ byte 1 @>, emitOneOpCode OpCodes.Conv_Ovf_U1)
    dict.Add(getMethod <@ sbyte 1 @>, emitOneOpCode OpCodes.Conv_Ovf_I1)
    dict.Add(getMethod <@ char 1 @>, emitOneOpCode OpCodes.Conv_Ovf_U2)
    dict.Add(getMethod <@ int 1 @>, doNothing)
    dict.Add(getMethod <@ int16 1 @>, emitOneOpCode OpCodes.Conv_Ovf_I2)
    dict.Add(getMethod <@ uint16 1 @>, emitOneOpCode OpCodes.Conv_Ovf_U2)
    dict.Add(getMethod <@ int32 1 @>, doNothing)
    dict.Add(getMethod <@ uint32 1 @>, emitOneOpCode OpCodes.Conv_Ovf_U4)
    dict.Add(getMethod <@ int64 1 @>, emitOneOpCode OpCodes.Conv_I8)
    dict.Add(getMethod <@ uint64 1 @>, emitOneOpCode OpCodes.Conv_Ovf_U8)
    dict.Add(getMethod <@ nativeint 1 @>, emitOneOpCode OpCodes.Conv_I)
    dict.Add(getMethod <@ unativeint 1 @>, emitOneOpCode OpCodes.Conv_Ovf_U)
    dict :> IReadOnlyDictionary<_, _>

  // shadowing the functions of the Microsoft.FSharp.Core.Operators.Checked module
  open Microsoft.FSharp.Core.Operators

  let private emitImpl (mi: MethodInfo) isTailCall (gen: ILGenerator) =
    match altEmitterTable1.TryGetValue(mi) with
    | true, emitter -> emitter gen
    | _ ->
        match altEmitterTable2.TryGetValue(mi) with
        | true, emitter -> emitter gen
        | _ ->
            let isReturnVoid = mi.ReturnType = typeof<System.Void>
            if isTailCall && not isReturnVoid then
              gen.Emit(OpCodes.Tailcall)
            gen.EmitCall(OpCodes.Call, mi, null)
            if isReturnVoid then
              gen.Emit(OpCodes.Ldnull)

  let emit (mi: MethodInfo, argsExprs: Expr list) (stack: CompileStack) =
    stack.Push(Compiling (fun gen ->
      emitImpl mi (stack.Count = 0) gen))
    argsExprs |> List.rev |> List.iter (fun argExpr -> stack.Push(CompileTarget argExpr))