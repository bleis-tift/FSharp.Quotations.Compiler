namespace FSharp.Quotations.Compiler

open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open System
open System.Globalization
open System.Collections.Generic
open System.Runtime.CompilerServices

#nowarn "1204"

module internal MethodCallEmitter =

  let typeTestGenericInt32MethodInfo = Expr.getMethodInfo <@ box 42 :? int @>

  let private identityEqualityComparer =
    { new IEqualityComparer<MethodInfo> with
        member __.Equals(x, y) = (x = y)
        member __.GetHashCode(x) = RuntimeHelpers.GetHashCode(x) }

  let doNothing = []

  let emitOpCode opcode = [ Compiling (fun (gen: ILGeneratorWrapper) -> gen.Emit(opcode)) ]
  let emitCall (mi: MethodInfo) (gen: ILGeneratorWrapper) =
    if mi.IsVirtual then
      gen.Emit(Callvirt (Method mi))
    else
      gen.Emit(Call (Method mi))

  let private (|>>) emit1 emit2 =
    match emit1, emit2 with
    | [Compiling e1], [Compiling e2] -> [ Compiling (fun (gen: ILGeneratorWrapper) -> e1 gen; e2 gen) ]
    | [Compiling e1], [Assumed e2] -> [ Assumed e2; Compiling e1 ]
    | _ -> failwith "oops!"

  let private emitCallPrim call =
    [ Assumed (function
               // 本当ならここでTailcallをemitしておきたいが、tail.命令があることでアドレス周りがおかしくなるケースがあるため一旦除去
               | IfRet, gen -> call gen
               | _, gen -> call gen) ]

  let emitCallMethod mi = emitCallPrim (fun gen -> emitCall mi gen)
  let emitCallMethodPrim mi call = emitCallPrim (fun gen -> gen.Emit(call (Method mi)))

  let emitStrToFloat (mi: MethodInfo) =
    emitOpCode (Ldc_I4 (int NumberStyles.Float))
    |>> emitOpCode (Call (PropGet (Expr.getPropertyInfo <@ CultureInfo.InvariantCulture @>)))
    |>> emitOpCode (Unbox_Any typeof<IFormatProvider>)
    |>> emitCallMethod mi

  let convInt16 = [ Compiling (fun gen -> gen.Emit(Conv_I2)) ]
  let convInt8 = [ Compiling (fun gen -> gen.Emit(Conv_I1)) ]

  let convUInt16 = [ Compiling (fun gen -> gen.Emit(Conv_U2)) ]
  let convUInt8 = [ Compiling (fun gen -> gen.Emit(Conv_U1)) ]

  let declareLocal<'T> loader =
    [
      Compiling (fun (gen: ILGeneratorWrapper) -> let loc = gen.DeclareLocal(typeof<'T>) in gen.Emit(Stloc (loc, None)); gen.Emit(loader (loc, None)))
    ]

  let private altEmitterTableUnchecked =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    dict.Add(Expr.getMethodInfo <@ 'a' + 'a' @>, emitOpCode Add)

    dict.Add(Expr.getMethodInfo <@ 1s % 1s @>, emitOpCode Rem |>> convInt16)
    dict.Add(Expr.getMethodInfo <@ 1y % 1y @>, emitOpCode Rem |>> convInt8)

    dict.Add(Expr.getMethodInfo <@ 1uL % 1uL @>, emitOpCode Rem_Un)
    dict.Add(Expr.getMethodInfo <@ 1u % 1u @>, emitOpCode Rem_Un)
    dict.Add(Expr.getMethodInfo <@ 1us % 1us @>, emitOpCode Rem_Un |>> convUInt16)
    dict.Add(Expr.getMethodInfo <@ 1uy % 1uy @>, emitOpCode Rem_Un |>> convUInt8)

    dict.Add(Expr.getMethodInfo <@ 1L >>> 1 @>, emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shr)
    dict.Add(Expr.getMethodInfo <@ 1 >>> 1 @>, emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shr)
    dict.Add(Expr.getMethodInfo <@ 1s >>> 1 @>, emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shr |>> convInt16)
    dict.Add(Expr.getMethodInfo <@ 1y >>> 1 @>, emitOpCode (Ldc_I4_7) |>> emitOpCode And |>> emitOpCode Shr |>> convInt8)

    dict.Add(Expr.getMethodInfo <@ 1L <<< 1 @>, emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ 1 <<< 1 @>, emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ 1s <<< 1 @>, emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shl |>> convInt16)
    dict.Add(Expr.getMethodInfo <@ 1y <<< 1 @>, emitOpCode (Ldc_I4_7) |>> emitOpCode And |>> emitOpCode Shl |>> convInt8)

    dict.Add(Expr.getMethodInfo <@ 1uL >>> 1 @>, emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shr_Un)
    dict.Add(Expr.getMethodInfo <@ 1u >>> 1 @>, emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shr_Un)
    dict.Add(Expr.getMethodInfo <@ 1us >>> 1 @>, emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shr_Un |>> convUInt16)
    dict.Add(Expr.getMethodInfo <@ 1uy >>> 1 @>, emitOpCode (Ldc_I4_7) |>> emitOpCode And |>> emitOpCode Shr_Un |>> convUInt8)

    dict.Add(Expr.getMethodInfo <@ 1uL <<< 1 @>, emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ 1u <<< 1 @>, emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ 1us <<< 1 @>, emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shl |>> convUInt16)
    dict.Add(Expr.getMethodInfo <@ 1uy <<< 1 @>, emitOpCode (Ldc_I4_7) |>> emitOpCode And |>> emitOpCode Shl |>> convUInt8)

    dict.Add(Expr.getMethodInfo <@ -(1I) @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_UnaryNegation(1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I - 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Subtraction(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I / 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Division(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I % 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Modulus(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I &&& 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_BitwiseAnd(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I ||| 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_BitwiseOr(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I ^^^ 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_ExclusiveOr(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I >>> 1 @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_RightShift(1I, 1) @>))
    dict.Add(Expr.getMethodInfo <@ 1I <<< 1 @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_LeftShift(1I, 1) @>))

    dict.Add(Expr.getMethodInfo <@ byte 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ sbyte 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ char 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int16 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint16 1 @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ int 1y @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1y @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int 1s @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1s @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int 1u @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1u @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ uint32 1y @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint32 1s @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint32 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint32 1u @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ int64 1L @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int64 1uL @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int64 1uy @>, emitOpCode Conv_U8)
    dict.Add(Expr.getMethodInfo <@ int64 1us @>, emitOpCode Conv_U8)
    dict.Add(Expr.getMethodInfo <@ int64 1u @>, emitOpCode Conv_U8)
    dict.Add(Expr.getMethodInfo <@ int64 'a' @>, emitOpCode Conv_U8)

    dict.Add(Expr.getMethodInfo <@ uint64 1L @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint64 1uL @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint64 1y @>, emitOpCode Conv_I8)
    dict.Add(Expr.getMethodInfo <@ uint64 1s @>, emitOpCode Conv_I8)
    dict.Add(Expr.getMethodInfo <@ uint64 1 @>, emitOpCode Conv_I8)

    dict.Add(Expr.getMethodInfo <@ float 1.0 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ float 1uy @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 1us @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 1u @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 1uL @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 'a' @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)

    dict.Add(Expr.getMethodInfo <@ float32 1.0f @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ float32 1uy @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 1us @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 1u @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 1uL @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 'a' @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)

    dict.Add(Expr.getMethodInfo <@ decimal 1uy @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1uy) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1y @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1y) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1s @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1s) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1us @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1us) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1 @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1u @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1u) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1L @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1L) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1uL @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1uL) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1.0 @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1.0) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1.0f @>, emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1.0f) @>))

    dict.Add(Expr.getMethodInfo <@ char 'a' @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ byte 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : byte @>))
    dict.Add(Expr.getMethodInfo <@ sbyte 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : sbyte @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : decimal @>))
    dict.Add(Expr.getMethodInfo <@ float 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : float @>))
    dict.Add(Expr.getMethodInfo <@ float32 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : float32 @>))
    dict.Add(Expr.getMethodInfo <@ int 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int @>))
    dict.Add(Expr.getMethodInfo <@ int16 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int16 @>))
    dict.Add(Expr.getMethodInfo <@ uint16 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : uint16 @>))
    dict.Add(Expr.getMethodInfo <@ int32 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int32 @>))
    dict.Add(Expr.getMethodInfo <@ uint32 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : uint32 @>))
    dict.Add(Expr.getMethodInfo <@ int64 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int64 @>))
    dict.Add(Expr.getMethodInfo <@ uint64 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : uint64 @>))

    dict.Add(Expr.getMethodInfo <@ byte "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt32("") @>)))
                                      |>> emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ sbyte "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ char "" @>, emitCallMethod (Expr.getMethodInfo <@ Char.Parse("") @>))
    dict.Add(Expr.getMethodInfo <@ decimal "" @>, emitStrToFloat (Expr.getMethodInfo <@ Decimal.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ float "" @>, emitStrToFloat (Expr.getMethodInfo <@ Double.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ float32 "" @>, emitStrToFloat (Expr.getMethodInfo <@ Single.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ int "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(Expr.getMethodInfo <@ int16 "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ uint16 "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt32("") @>)))
                                        |>> emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ int32 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(Expr.getMethodInfo <@ uint32 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt32("") @>))
    dict.Add(Expr.getMethodInfo <@ int64 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt64("") @>))
    dict.Add(Expr.getMethodInfo <@ uint64 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt64("") @>))

    dict.Add(Expr.getMethodInfo <@ sign 1I @>, declareLocal<bigint> Ldloca |>> emitOpCode (Call (PropGet (Expr.getPropertyInfo <@ (1I).Sign @>))))

    dict :> IReadOnlyDictionary<_, _>

  open Microsoft.FSharp.Core.Operators.Checked

  let private altEmitterTableChecked =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    dict.Add(Expr.getMethodInfo <@ -(1.0) @>, emitOpCode Neg)
    dict.Add(Expr.getMethodInfo <@ -(1.0f) @>, emitOpCode Neg)

    dict.Add(Expr.getMethodInfo <@ 1y - 1y @>, emitOpCode Sub_Ovf |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ 1s - 1s @>, emitOpCode Sub_Ovf |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ 1 - 1 @>, emitOpCode Sub_Ovf)
    dict.Add(Expr.getMethodInfo <@ 1L - 1L @>, emitOpCode Sub_Ovf)
    dict.Add(Expr.getMethodInfo <@ 1uy - 1uy @>, emitOpCode Sub_Ovf_Un |>> emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ 1us - 1us @>, emitOpCode Sub_Ovf_Un |>> emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ 1u - 1u @>, emitOpCode Sub_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ 1uL - 1uL @>, emitOpCode Sub_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ 1.0 - 1.0 @>, emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ 1.0f - 1.0f @>, emitOpCode Sub)

    dict.Add(Expr.getMethodInfo <@ 1y * 1y @>, emitOpCode Mul_Ovf |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ 1s * 1s @>, emitOpCode Mul_Ovf |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ 1 * 1 @>, emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ 1L * 1L @>, emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ 1uy * 1uy @>, emitOpCode Mul_Ovf_Un |>> emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ 1us * 1us @>, emitOpCode Mul_Ovf_Un |>> emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ 1u * 1u @>, emitOpCode Mul_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ 1uL * 1uL @>, emitOpCode Mul_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ 1.0 * 1.0 @>, emitOpCode Mul)
    dict.Add(Expr.getMethodInfo <@ 1.0f * 1.0f @>, emitOpCode Mul)

    dict.Add(Expr.getMethodInfo <@ byte 1uy @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ byte 1us @>, emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ byte 1u @>, emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ byte 1uL @>, emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ byte 'a' @>, emitOpCode Conv_Ovf_U1_Un)

    dict.Add(Expr.getMethodInfo <@ sbyte 1y @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ sbyte 1uy @>, emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ sbyte 1us @>, emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ sbyte 1u @>, emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ sbyte 1uL @>, emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ sbyte 'a' @>, emitOpCode Conv_Ovf_I1_Un)

    dict.Add(Expr.getMethodInfo <@ int16 1s @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int16 1uy @>, emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ int16 1us @>, emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ int16 1u @>, emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ int16 1uL @>, emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ int16 'a' @>, emitOpCode Conv_Ovf_I2_Un)

    dict.Add(Expr.getMethodInfo <@ uint16 1uy @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ uint16 1us @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint16 1u @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ uint16 1uL @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ uint16 'a' @>, emitOpCode Conv_Ovf_U2_Un)

    dict.Add(Expr.getMethodInfo <@ char 1uy @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ char 1us @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ char 1u @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ char 1uL @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ char 'a' @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ -(1I) @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_UnaryNegation(1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I - 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Subtraction(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I * 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Multiply(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I / 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Division(1I, 1I) @>))
    dict.Add(Expr.getMethodInfo <@ 1I % 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Modulus(1I, 1I) @>))

    dict.Add(Expr.getMethodInfo <@ int 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1 @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int 1uy @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int32 1uy @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int 1us @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int32 1us @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int 1u @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int32 1u @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int 1uL @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int32 1uL @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int 'a' @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ int32 'a' @>, emitOpCode Conv_Ovf_I4_Un)

    dict.Add(Expr.getMethodInfo <@ uint32 1uy @>, emitOpCode Conv_Ovf_U4_Un)
    dict.Add(Expr.getMethodInfo <@ uint32 1us @>, emitOpCode Conv_Ovf_U4_Un)
    dict.Add(Expr.getMethodInfo <@ uint32 1u @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint32 1uL @>, emitOpCode Conv_Ovf_U4_Un)
    dict.Add(Expr.getMethodInfo <@ uint32 'a' @>, emitOpCode Conv_Ovf_U4_Un)

    dict.Add(Expr.getMethodInfo <@ int64 1L @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int64 1uy @>, emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ int64 1us @>, emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ int64 1u @>, emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ int64 1uL @>, emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ int64 'a' @>, emitOpCode Conv_Ovf_I8_Un)

    dict.Add(Expr.getMethodInfo <@ uint64 1uy @>, emitOpCode Conv_Ovf_U8_Un)
    dict.Add(Expr.getMethodInfo <@ uint64 1us @>, emitOpCode Conv_Ovf_U8_Un)
    dict.Add(Expr.getMethodInfo <@ uint64 1u @>, emitOpCode Conv_Ovf_U8_Un)
    dict.Add(Expr.getMethodInfo <@ uint64 1uL @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint64 'a' @>, emitOpCode Conv_Ovf_U8_Un)

    dict.Add(Expr.getMethodInfo <@ byte 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : byte @>))
    dict.Add(Expr.getMethodInfo <@ sbyte 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : sbyte @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : decimal @>))
    dict.Add(Expr.getMethodInfo <@ float 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : float @>))
    dict.Add(Expr.getMethodInfo <@ float32 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : float32 @>))
    dict.Add(Expr.getMethodInfo <@ int 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int @>))
    dict.Add(Expr.getMethodInfo <@ int16 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int16 @>))
    dict.Add(Expr.getMethodInfo <@ uint16 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : uint16 @>))
    dict.Add(Expr.getMethodInfo <@ int32 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int32 @>))
    dict.Add(Expr.getMethodInfo <@ uint32 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : uint32 @>))
    dict.Add(Expr.getMethodInfo <@ int64 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : int64 @>))
    dict.Add(Expr.getMethodInfo <@ uint64 1I @>, emitCallMethod (Expr.getMethodInfo <@ Numerics.BigInteger.op_Explicit(1I) : uint64 @>))

    dict.Add(Expr.getMethodInfo <@ byte "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt32("") @>)))
                                      |>> emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ sbyte "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ char "" @>, emitCallMethod (Expr.getMethodInfo <@ Char.Parse("") @>))
    dict.Add(Expr.getMethodInfo <@ int "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(Expr.getMethodInfo <@ int16 "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ uint16 "" @>, emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt32("") @>)))
                                        |>> emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ int32 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(Expr.getMethodInfo <@ uint32 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt32("") @>))
    dict.Add(Expr.getMethodInfo <@ int64 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt64("") @>))
    dict.Add(Expr.getMethodInfo <@ uint64 "" @>, emitCallMethod (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt64("") @>))

    dict :> IReadOnlyDictionary<_, _>

  // shadowing the functions of the Microsoft.FSharp.Core.Operators.Checked module
  open Microsoft.FSharp.Core.Operators

  let private altGenericEmitterTable =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    let x = 42
    dict.Add(Expr.getGenericMethodInfo <@ +(x) @>, doNothing)
    dict.Add(Expr.getGenericMethodInfo <@ -(x) @>, emitOpCode Neg)
    dict.Add(Expr.getGenericMethodInfo <@ x - x @>, emitOpCode Sub)
    dict.Add(Expr.getGenericMethodInfo <@ x / x @>, emitOpCode Div)
    dict.Add(Expr.getGenericMethodInfo <@ x % x @>, emitOpCode Rem)

    dict.Add(Expr.getGenericMethodInfo <@ x &&& x @>, emitOpCode And)
    dict.Add(Expr.getGenericMethodInfo <@ x ||| x @>, emitOpCode Or)
    dict.Add(Expr.getGenericMethodInfo <@ x ^^^ x @>, emitOpCode Xor)
    dict.Add(Expr.getGenericMethodInfo <@ ~~~x @>, emitOpCode Not)

    dict.Add(Expr.getGenericMethodInfo <@ byte x @>, emitOpCode Conv_U1)
    dict.Add(Expr.getGenericMethodInfo <@ sbyte x @>, emitOpCode Conv_I1)
    dict.Add(Expr.getGenericMethodInfo <@ int16 x @>, emitOpCode Conv_I2)
    dict.Add(Expr.getGenericMethodInfo <@ uint16 x @>, emitOpCode Conv_U2)
    dict.Add(Expr.getGenericMethodInfo <@ int x @>, emitOpCode Conv_I4)
    dict.Add(Expr.getGenericMethodInfo <@ int32 x @>, emitOpCode Conv_I4)
    dict.Add(Expr.getGenericMethodInfo <@ uint32 x @>, emitOpCode Conv_U4)
    dict.Add(Expr.getGenericMethodInfo <@ int64 x @>, emitOpCode Conv_I8)
    dict.Add(Expr.getGenericMethodInfo <@ uint64 x @>, emitOpCode Conv_U8)
    dict.Add(Expr.getGenericMethodInfo <@ nativeint x @>, emitOpCode Conv_I)
    dict.Add(Expr.getGenericMethodInfo <@ unativeint x @>, emitOpCode Conv_U)
    dict.Add(Expr.getGenericMethodInfo <@ float32 x @>, emitOpCode Conv_R4)
    dict.Add(Expr.getGenericMethodInfo <@ float x @>, emitOpCode Conv_R8)
    dict.Add(Expr.getGenericMethodInfo <@ char x @>, emitOpCode Conv_U2)

    dict.Add(Expr.getGenericMethodInfo <@ Checked.(~-) x @>, emitOpCode Ldc_I4_M1 |>> emitOpCode Mul_Ovf)

    dict.Add(Expr.getGenericMethodInfo <@ Checked.byte x @>, emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.sbyte x @>, emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.char x @>, emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.int x @>, emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.int16 x @>, emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.uint16 x @>, emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.int32 x @>, emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.uint32 x @>, emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.int64 x @>, emitOpCode Conv_Ovf_I8)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.uint64 x @>, emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.nativeint x @>, emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getGenericMethodInfo <@ Checked.unativeint x @>, emitOpCode Conv_Ovf_U)
    dict :> IReadOnlyDictionary<_, _>

  let private altEmitterTableReceiver (recv: Expr option, mi: MethodInfo) =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    match recv with
    | Some r ->
        if r.Type.IsValueType then
          if mi.DeclaringType = typeof<obj> then
            dict.Add(mi, emitOpCode (Constrainted r.Type) |>> emitCallMethodPrim mi Callvirt)
          else
            dict.Add(mi, emitCallMethod mi)
        dict
    | _ -> dict
    :> IReadOnlyDictionary<_, _>

  let loadReceiverAddress (r: Expr) (varEnv: VariableEnv ref) =
    let v = match r with | Patterns.Var v -> v | _ -> failwith "argument is not variable"
    let (local, name) =
      match List.pick (fun (n, _, info) -> if n = v.Name then Some info else None) !varEnv with
      | Local (local, name) -> (local, name)
      | _ -> failwith "variable is not found in varEnv"
    [
      Compiling (fun (gen: ILGeneratorWrapper) -> gen.Emit(ILOpCode.ldloca local name))
    ]

  let private tryGetGenericEmitter (mi: MethodInfo) =
    if mi.IsGenericMethod then
      match altGenericEmitterTable.TryGetValue(mi.GetGenericMethodDefinition()) with
      | true, emitter -> Some emitter
      | _ -> None
    else
      None

  let private getPushingCompileStackInfos (recv: Expr option) (mi: MethodInfo) =
    // TODO : rewrite using option computation builder
    match altEmitterTableUnchecked.TryGetValue(mi) with
    | true, emitter -> (emitter, None)
    | _ ->
        match altEmitterTableChecked.TryGetValue(mi) with
        | true, emitter -> (emitter, None)
        | _ ->
            match altEmitterTableReceiver(recv, mi).TryGetValue(mi) with
            | true, emitter -> (emitter, Some loadReceiverAddress)
            | _ ->
                match tryGetGenericEmitter mi with
                | Some emitter -> (emitter, None)
                | _ ->
                    let emitCall (gen: ILGeneratorWrapper) =
                      if mi.IsVirtual then gen.Emit(Callvirt (Method mi))
                      else gen.Emit(Call (Method mi))

                    let isReturnVoid = mi.ReturnType = typeof<Void>
                    let assumed = 
                      if isReturnVoid then
                        [ Assumed (function
                                   | IfSequential, gen -> emitCall gen
                                   | _, gen -> emitCall gen; gen.Emit(Ldnull)) ]
                      elif mi.ReturnType = typeof<unit> then
                        [ Assumed (function
                                   | IfSequential, gen -> emitCall gen; gen.Emit(Pop)
                                   | IfRet, gen -> gen.Emit(Tailcall); emitCall gen
                                   | _, gen -> emitCall gen; gen.Emit(Ldnull)) ]
                      else
                        [ Assumed (function
                                   | IfRet, gen -> gen.Emit(Tailcall); emitCall gen
                                   | _, gen -> emitCall gen) ]
                    (assumed, None)

  let emit (recv: Expr option, mi: MethodInfo, argsExprs: Expr list) (stack: CompileStack) (varEnv: VariableEnv ref) =
    let (emitter, recvEmitter) = getPushingCompileStackInfos recv mi
    emitter |> List.iter stack.Push
    argsExprs |> List.rev |> List.iter (fun argExpr -> stack.Push(CompileTarget argExpr))
    match recv, recvEmitter with
    | Some r, Some re ->
        re r varEnv |> List.iter stack.Push
    | Some r, _ ->
        stack.Push(CompileTarget r)
    | _, _ -> ()
