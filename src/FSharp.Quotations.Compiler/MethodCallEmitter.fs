(*
 * FSharp.Quotations.Compiler - a compiler for F# expression tree
 * Written in 2015 by bleis-tift (hey_c_est_la_vie@hotmail.co.jp)
 * kyonmm, zakky-dev
 * 
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain worldwide.
 * This software is distributed without any warranty.
 * 
 * You should have received a copy of the CC0 Public Domain Dedication along with this software.
 * If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
 *)
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

  let emitOpCode opcode = [ Compiling (fun gen -> gen.Emit(opcode)) ]
  let emitCall (mi: MethodInfo) (gen: ILGeneratorWrapper) =
    if mi.IsVirtual then
      gen.Emit(Callvirt (Method mi))
    else
      gen.Emit(Call (Method mi))

  let private (|>>) emit1 emit2 =
    match emit1, emit2 with
    | [Compiling e1], [Compiling e2] -> [ Compiling (fun gen -> e1 gen; e2 gen) ]
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

  let declareTemp<'T> loader =
    [
      Compiling (fun gen -> let loc = gen.DeclareTemp(typeof<'T>) in gen.Emit(Stloc (loc, None)); gen.Emit(loader (loc, None)))
    ]

  let private nint: Expr<nativeint> = Expr.Value(1n) |> Expr.Cast
  let private unint: Expr<unativeint> = Expr.Value(1un) |> Expr.Cast

  let private rangeByteM = Expr.getMethodInfo <@ OperatorIntrinsics.RangeByte @>
  let private rangeCharM = Expr.getMethodInfo <@ OperatorIntrinsics.RangeChar @>
  let private rangeDoubleM = Expr.getMethodInfo <@ OperatorIntrinsics.RangeDouble @>
  let private rangeInt16M = Expr.getMethodInfo <@ OperatorIntrinsics.RangeInt16 @>
  let private rangeInt32M = Expr.getMethodInfo <@ OperatorIntrinsics.RangeInt32 @>
  let private rangeInt64M = Expr.getMethodInfo <@ OperatorIntrinsics.RangeInt64 @>
  let private rangeIntPtrM = Expr.getMethodInfo <@ OperatorIntrinsics.RangeIntPtr @>
  let private rangeSByteM = Expr.getMethodInfo <@ OperatorIntrinsics.RangeSByte  @>
  let private rangeSingleM = Expr.getMethodInfo <@ OperatorIntrinsics.RangeSingle @>
  let private rangeUInt16M = Expr.getMethodInfo <@ OperatorIntrinsics.RangeUInt16 @>
  let private rangeUInt32M = Expr.getMethodInfo <@ OperatorIntrinsics.RangeUInt32 @>
  let private rangeUInt64M = Expr.getMethodInfo <@ OperatorIntrinsics.RangeUInt64 @>
  let private rangeUIntPtrM = Expr.getMethodInfo <@ OperatorIntrinsics.RangeUIntPtr @>

  let private emitRange<'T> mi =
    [ Compiling (fun gen ->
        let tmp = gen.DeclareTemp(typeof<'T>)
        gen.Emit(Stloc (tmp, None))
        gen.Emit(Ldc_I4_1)
        gen.Emit(Ldloc (tmp, None))
      ) ]
    |>> emitCallMethod mi

  let private altEmitterTable =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    dict.Add(Expr.getMethodInfo <@ (..) 1uy @>,    emitRange<byte>       rangeByteM)
    dict.Add(Expr.getMethodInfo <@ (..) 1.0 @>,    emitRange<float>      rangeDoubleM)
    dict.Add(Expr.getMethodInfo <@ (..) 1s @>,     emitRange<int16>      rangeInt16M)
    dict.Add(Expr.getMethodInfo <@ (..) 1 @>,      emitRange<int>        rangeInt32M)
    dict.Add(Expr.getMethodInfo <@ (..) 1L @>,     emitRange<int64>      rangeInt64M)
    dict.Add(Expr.getMethodInfo <@ (..) %nint @>,  emitRange<nativeint>  rangeIntPtrM)
    dict.Add(Expr.getMethodInfo <@ (..) 1y @>,     emitRange<sbyte>      rangeSByteM)
    dict.Add(Expr.getMethodInfo <@ (..) 1.0f @>,   emitRange<float32>    rangeSingleM)
    dict.Add(Expr.getMethodInfo <@ (..) 1us @>,    emitRange<uint16>     rangeUInt16M)
    dict.Add(Expr.getMethodInfo <@ (..) 1u @>,     emitRange<uint32>     rangeUInt32M)
    dict.Add(Expr.getMethodInfo <@ (..) 1uL @>,    emitRange<uint64>     rangeUInt64M)
    dict.Add(Expr.getMethodInfo <@ (..) %unint @>, emitRange<unativeint> rangeUIntPtrM)

    dict.Add(Expr.getMethodInfo <@ (..) 'a' @>,       emitCallMethod rangeCharM)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1uy @>,    emitCallMethod rangeByteM)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1.0 @>,    emitCallMethod rangeDoubleM)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1s @>,     emitCallMethod rangeInt16M)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1 @>,      emitCallMethod rangeInt32M)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1L @>,     emitCallMethod rangeInt64M)
    dict.Add(Expr.getMethodInfo <@ (.. ..) %nint @>,  emitCallMethod rangeIntPtrM)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1y @>,     emitCallMethod rangeSByteM)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1.0f @>,   emitCallMethod rangeSingleM)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1us @>,    emitCallMethod rangeUInt16M)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1u @>,     emitCallMethod rangeUInt32M)
    dict.Add(Expr.getMethodInfo <@ (.. ..) 1uL @>,    emitCallMethod rangeUInt64M)
    dict.Add(Expr.getMethodInfo <@ (.. ..) %unint @>, emitCallMethod rangeUIntPtrM)

    dict.Add(Expr.getMethodInfo <@ 'a' + 'a' @>, emitOpCode Add)

    dict.Add(Expr.getMethodInfo <@ (-) 1 @>,      emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) 1.0 @>,    emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) 1.0f @>,   emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) 1L @>,     emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) 1u @>,     emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) 1uL @>,    emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) %nint @>,  emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) %unint @>, emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ (-) 1s @>,     emitOpCode Sub |>> emitOpCode Conv_I2)
    dict.Add(Expr.getMethodInfo <@ (-) 1us @>,    emitOpCode Sub |>> emitOpCode Conv_U2)
    dict.Add(Expr.getMethodInfo <@ (-) 1y @>,     emitOpCode Sub |>> emitOpCode Conv_I1)
    dict.Add(Expr.getMethodInfo <@ (-) 1uy @>,    emitOpCode Sub |>> emitOpCode Conv_U1)

    dict.Add(Expr.getMethodInfo <@ (/) 1 @>,      emitOpCode Div)
    dict.Add(Expr.getMethodInfo <@ (/) 1.0 @>,    emitOpCode Div)
    dict.Add(Expr.getMethodInfo <@ (/) 1.0f @>,   emitOpCode Div)
    dict.Add(Expr.getMethodInfo <@ (/) 1L @>,     emitOpCode Div)
    dict.Add(Expr.getMethodInfo <@ (/) 1uL @>,    emitOpCode Div_Un)
    dict.Add(Expr.getMethodInfo <@ (/) 1u @>,     emitOpCode Div_Un)
    dict.Add(Expr.getMethodInfo <@ (/) %nint @>,  emitOpCode Div)
    dict.Add(Expr.getMethodInfo <@ (/) %unint @>, emitOpCode Div_Un)
    dict.Add(Expr.getMethodInfo <@ (/) 1s @>,     emitOpCode Div    |>> emitOpCode Conv_I2)
    dict.Add(Expr.getMethodInfo <@ (/) 1us @>,    emitOpCode Div_Un |>> emitOpCode Conv_U2)
    dict.Add(Expr.getMethodInfo <@ (/) 1y @>,     emitOpCode Div    |>> emitOpCode Conv_I1)
    dict.Add(Expr.getMethodInfo <@ (/) 1uy @>,    emitOpCode Div_Un |>> emitOpCode Conv_U1)

    dict.Add(Expr.getMethodInfo <@ (%) 1 @>,      emitOpCode Rem)
    dict.Add(Expr.getMethodInfo <@ (%) 1.0 @>,    emitOpCode Rem)
    dict.Add(Expr.getMethodInfo <@ (%) 1.0f @>,   emitOpCode Rem)
    dict.Add(Expr.getMethodInfo <@ (%) 1L @>,     emitOpCode Rem)
    dict.Add(Expr.getMethodInfo <@ (%) 1uL @>,    emitOpCode Rem_Un)
    dict.Add(Expr.getMethodInfo <@ (%) 1u @>,     emitOpCode Rem_Un)
    dict.Add(Expr.getMethodInfo <@ (%) %nint @>,  emitOpCode Rem)
    dict.Add(Expr.getMethodInfo <@ (%) %unint @>, emitOpCode Rem_Un)
    dict.Add(Expr.getMethodInfo <@ (%) 1s @>,     emitOpCode Rem    |>> emitOpCode Conv_I2)
    dict.Add(Expr.getMethodInfo <@ (%) 1us @>,    emitOpCode Rem_Un |>> emitOpCode Conv_U2)
    dict.Add(Expr.getMethodInfo <@ (%) 1y @>,     emitOpCode Rem    |>> emitOpCode Conv_I1)
    dict.Add(Expr.getMethodInfo <@ (%) 1uy @>,    emitOpCode Rem_Un |>> emitOpCode Conv_U1)

    dict.Add(Expr.getMethodInfo <@ (<<<) 1 @>,      emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ (<<<) 1u @>,     emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ (<<<) 1L @>,     emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ (<<<) 1uL @>,    emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ (<<<) %nint @>,  emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ (<<<) %unint @>, emitOpCode Shl)
    dict.Add(Expr.getMethodInfo <@ (<<<) 1s @>,     emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shl |>> emitOpCode Conv_I2)
    dict.Add(Expr.getMethodInfo <@ (<<<) 1us @>,    emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shl |>> emitOpCode Conv_U2)
    dict.Add(Expr.getMethodInfo <@ (<<<) 1y @>,     emitOpCode (Ldc_I4_S 7)  |>> emitOpCode And |>> emitOpCode Shl |>> emitOpCode Conv_I1)
    dict.Add(Expr.getMethodInfo <@ (<<<) 1uy @>,    emitOpCode (Ldc_I4_S 7)  |>> emitOpCode And |>> emitOpCode Shl |>> emitOpCode Conv_U1)

    dict.Add(Expr.getMethodInfo <@ (>>>) 1 @>,      emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shr)
    dict.Add(Expr.getMethodInfo <@ (>>>) 1u @>,     emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shr_Un)
    dict.Add(Expr.getMethodInfo <@ (>>>) 1L @>,     emitOpCode (Ldc_I4_S 31) |>> emitOpCode And |>> emitOpCode Shr)
    dict.Add(Expr.getMethodInfo <@ (>>>) 1uL @>,    emitOpCode (Ldc_I4_S 63) |>> emitOpCode And |>> emitOpCode Shr_Un)
    dict.Add(Expr.getMethodInfo <@ (>>>) %nint @>,  emitOpCode Shr)
    dict.Add(Expr.getMethodInfo <@ (>>>) %unint @>, emitOpCode Shr_Un)
    dict.Add(Expr.getMethodInfo <@ (>>>) 1s @>,     emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shr    |>> emitOpCode Conv_I2)
    dict.Add(Expr.getMethodInfo <@ (>>>) 1us @>,    emitOpCode (Ldc_I4_S 15) |>> emitOpCode And |>> emitOpCode Shr_Un |>> emitOpCode Conv_U2)
    dict.Add(Expr.getMethodInfo <@ (>>>) 1y @>,     emitOpCode (Ldc_I4_S 7)  |>> emitOpCode And |>> emitOpCode Shr    |>> emitOpCode Conv_I1)
    dict.Add(Expr.getMethodInfo <@ (>>>) 1uy @>,    emitOpCode (Ldc_I4_S 7)  |>> emitOpCode And |>> emitOpCode Shr_Un |>> emitOpCode Conv_U1)

    dict.Add(Expr.getMethodInfo <@ (~~~) 1 @>,      emitOpCode Not)
    dict.Add(Expr.getMethodInfo <@ (~~~) 1L @>,     emitOpCode Not)
    dict.Add(Expr.getMethodInfo <@ (~~~) 1uL @>,    emitOpCode Not)
    dict.Add(Expr.getMethodInfo <@ (~~~) 1u @>,     emitOpCode Not)
    dict.Add(Expr.getMethodInfo <@ (~~~) %nint @>,  emitOpCode Not)
    dict.Add(Expr.getMethodInfo <@ (~~~) %unint @>, emitOpCode Not)
    dict.Add(Expr.getMethodInfo <@ (~~~) 1s @>,     emitOpCode Not |>> emitOpCode Conv_I2)
    dict.Add(Expr.getMethodInfo <@ (~~~) 1us @>,    emitOpCode Not |>> emitOpCode Conv_U2)
    dict.Add(Expr.getMethodInfo <@ (~~~) 1y @>,     emitOpCode Not |>> emitOpCode Conv_I1)
    dict.Add(Expr.getMethodInfo <@ (~~~) 1uy @>,    emitOpCode Not |>> emitOpCode Conv_U1)

    dict.Add(Expr.getMethodInfo <@ byte 1 @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ byte 1uy @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ sbyte 1 @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ sbyte 1y @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ char 1 @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ char 'a' @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ int8 1 @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ int8 1y @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ uint8 1 @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ uint8 1uy @>,  doNothing)
    dict.Add(Expr.getMethodInfo <@ int16 1 @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ int16 1s @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ uint16 1 @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ uint16 1us @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ int 1y @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ int 1s @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ int 1 @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ int 1u @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1y @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1s @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1 @>,  doNothing)
    dict.Add(Expr.getMethodInfo <@ int32 1u @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ uint32 1y @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint32 1s @>, doNothing)
    dict.Add(Expr.getMethodInfo <@ uint32 1 @>,  doNothing)
    dict.Add(Expr.getMethodInfo <@ uint32 1u @>, doNothing)

    dict.Add(Expr.getMethodInfo <@ int64 1L @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ int64 1uL @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ int64 1uy @>,    emitOpCode Conv_U8)
    dict.Add(Expr.getMethodInfo <@ int64 1us @>,    emitOpCode Conv_U8)
    dict.Add(Expr.getMethodInfo <@ int64 1u @>,     emitOpCode Conv_U8)
    dict.Add(Expr.getMethodInfo <@ int64 'a' @>,    emitOpCode Conv_U8)
    dict.Add(Expr.getMethodInfo <@ int64 %unint @>, emitOpCode Conv_U8)

    dict.Add(Expr.getMethodInfo <@ uint64 1L @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ uint64 1uL @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ uint64 1y @>,    emitOpCode Conv_I8)
    dict.Add(Expr.getMethodInfo <@ uint64 1s @>,    emitOpCode Conv_I8)
    dict.Add(Expr.getMethodInfo <@ uint64 1 @>,     emitOpCode Conv_I8)
    dict.Add(Expr.getMethodInfo <@ uint64 %nint @>, emitOpCode Conv_I8)

    dict.Add(Expr.getMethodInfo <@ float 1.0 @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ float 1uy @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 1us @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 1u @>,     emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 1uL @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float 'a' @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)
    dict.Add(Expr.getMethodInfo <@ float %unint @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)

    dict.Add(Expr.getMethodInfo <@ float32 1.0f @>,   doNothing)
    dict.Add(Expr.getMethodInfo <@ float32 1uy @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 1us @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 1u @>,     emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 1uL @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 'a' @>,    emitOpCode Conv_R_Un |>> emitOpCode Conv_R4)
    dict.Add(Expr.getMethodInfo <@ float32 %unint @>, emitOpCode Conv_R_Un |>> emitOpCode Conv_R8)

    dict.Add(Expr.getMethodInfo <@ decimal 1uy @>,    emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1uy) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1y @>,     emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1y) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1s @>,     emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1s) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1us @>,    emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1us) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1 @>,      emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1u @>,     emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1u) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1L @>,     emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1L) @>))
    dict.Add(Expr.getMethodInfo <@ decimal %nint @>,  emitOpCode Conv_I8 |>> emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1L) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1uL @>,    emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1uL) @>))
    dict.Add(Expr.getMethodInfo <@ decimal %unint @>, emitOpCode Conv_U8 |>> emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1uL) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1.0 @>,    emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1.0) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1.0f @>,   emitCallMethod (Expr.getMethodInfo <@ Convert.ToDecimal(1.0f) @>))
    dict.Add(Expr.getMethodInfo <@ decimal 1.0M @>,   doNothing)

    dict.Add(Expr.getMethodInfo <@ nativeint 1uL @>, emitOpCode Conv_U)
    dict.Add(Expr.getMethodInfo <@ nativeint 1u @>,  emitOpCode Conv_U)
    dict.Add(Expr.getMethodInfo <@ nativeint 1us @>, emitOpCode Conv_U)
    dict.Add(Expr.getMethodInfo <@ nativeint 'a' @>, emitOpCode Conv_U)

    dict.Add(Expr.getMethodInfo <@ unativeint 1L @>, emitOpCode Conv_I)
    dict.Add(Expr.getMethodInfo <@ unativeint 1 @>,  emitOpCode Conv_I)
    dict.Add(Expr.getMethodInfo <@ unativeint 1s @>, emitOpCode Conv_I)
    dict.Add(Expr.getMethodInfo <@ unativeint 1y @>, emitOpCode Conv_I)

    let emitParseInt32  = emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt32 @>)))
    let emitParseUInt32 = emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt32 @>)))
    let emitParseInt64  = emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseInt64 @>)))
    let emitParseUInt64 = emitOpCode (Call (Method (Expr.getMethodInfo <@ LanguagePrimitives.ParseUInt64 @>)))

    dict.Add(Expr.getMethodInfo <@ sbyte "" @>,   emitParseInt32  |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ byte "" @>,    emitParseUInt32 |>> emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ int8 "" @>,    emitParseInt32  |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ uint8 "" @>,   emitParseUInt32 |>> emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ int16 "" @>,   emitParseInt32  |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ uint16 "" @>,  emitParseUInt32 |>> emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ char "" @>,    emitCallMethod (Expr.getMethodInfo <@ Char.Parse("") @>))
    dict.Add(Expr.getMethodInfo <@ decimal "" @>, emitStrToFloat (Expr.getMethodInfo <@ Decimal.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ float "" @>,   emitStrToFloat (Expr.getMethodInfo <@ Double.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ double "" @>,  emitStrToFloat (Expr.getMethodInfo <@ Double.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ float32 "" @>, emitStrToFloat (Expr.getMethodInfo <@ Single.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ single "" @>,  emitStrToFloat (Expr.getMethodInfo <@ Single.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(Expr.getMethodInfo <@ int "" @>,     emitParseInt32)
    dict.Add(Expr.getMethodInfo <@ int32 "" @>,   emitParseInt32)
    dict.Add(Expr.getMethodInfo <@ uint32 "" @>,  emitParseUInt32)
    dict.Add(Expr.getMethodInfo <@ int64 "" @>,   emitParseInt64)
    dict.Add(Expr.getMethodInfo <@ uint64 "" @>,  emitParseUInt64)

    dict.Add(Expr.getMethodInfo <@ sign 1I @>, declareTemp<bigint> Ldloca |>> emitOpCode (Call (PropGet (Expr.getPropertyInfo <@ (1I).Sign @>))))

    dict.Add(Expr.getMethodInfo <@ Checked.sbyte "" @>,  emitParseInt32  |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte "" @>,   emitParseUInt32 |>> emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 "" @>,  emitParseInt32  |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 "" @>, emitParseUInt32 |>> emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char "" @>,   emitCallMethod (Expr.getMethodInfo <@ Char.Parse("") @>))
    dict.Add(Expr.getMethodInfo <@ Checked.int "" @>,    emitParseInt32)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 "" @>,  emitParseInt32)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 "" @>, emitParseUInt32)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 "" @>,  emitParseInt64)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 "" @>, emitParseUInt64)

    dict.Add(Expr.getMethodInfo <@ Checked.(~-) 1 @>,     emitOpCode Ldc_I4_M1 |>> emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(~-) 1.0 @>,   emitOpCode Neg)
    dict.Add(Expr.getMethodInfo <@ Checked.(~-) 1.0f @>,  emitOpCode Neg)
    dict.Add(Expr.getMethodInfo <@ Checked.(~-) 1L @>,    emitOpCode Ldc_I4_M1 |>> emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(~-) 1s @>,    emitOpCode Ldc_I4_M1 |>> emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(~-) %nint @>, emitOpCode Ldc_I4_M1 |>> emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(~-) 1y @>,    emitOpCode Ldc_I4_M1 |>> emitOpCode Mul_Ovf)

    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1 @>,      emitOpCode Sub_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1.0 @>,    emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1.0f @>,   emitOpCode Sub)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1L @>,     emitOpCode Sub_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1uL @>,    emitOpCode Sub_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1u @>,     emitOpCode Sub_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) %nint @>,  emitOpCode Sub_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) %unint @>, emitOpCode Sub_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1s @>,     emitOpCode Sub_Ovf    |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1us @>,    emitOpCode Sub_Ovf_Un |>> emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1y @>,     emitOpCode Sub_Ovf    |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.(-) 1uy @>,    emitOpCode Sub_Ovf_Un |>> emitOpCode Conv_Ovf_U1_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1y @>,     emitOpCode Mul_Ovf |>> emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1s @>,     emitOpCode Mul_Ovf |>> emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1L @>,     emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1 @>,      emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) %nint @>,  emitOpCode Mul_Ovf)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1uy @>,    emitOpCode Mul_Ovf_Un |>> emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1us @>,    emitOpCode Mul_Ovf_Un |>> emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1u @>,     emitOpCode Mul_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1uL @>,    emitOpCode Mul_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) %unint @>, emitOpCode Mul_Ovf_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1.0 @>,    emitOpCode Mul)
    dict.Add(Expr.getMethodInfo <@ Checked.(*) 1.0f @>,   emitOpCode Mul)

    dict.Add(Expr.getMethodInfo <@ Checked.byte 1.0 @>,    emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1.0f @>,   emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1L @>,     emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1 @>,      emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1s @>,     emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte %nint @>,  emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1y @>,     emitOpCode Conv_Ovf_U1)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1uL @>,    emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1u @>,     emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1us @>,    emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 'a' @>,    emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.byte %unint @>, emitOpCode Conv_Ovf_U1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.byte 1uy @>,    doNothing)

    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1.0 @>,    emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1.0f @>,   emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1L @>,     emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1 @>,      emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1s @>,     emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte %nint @>,  emitOpCode Conv_Ovf_I1)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1y @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1uL @>,    emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1u @>,     emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1us @>,    emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 'a' @>,    emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte %unint @>, emitOpCode Conv_Ovf_I1_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.sbyte 1uy @>,    emitOpCode Conv_Ovf_I1_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.int16 1.0 @>,    emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1.0f @>,   emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1L @>,     emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1 @>,      emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1s @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 %nint @>,  emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1y @>,     emitOpCode Conv_Ovf_I2)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1uL @>,    emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1u @>,     emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1us @>,    emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 'a' @>,    emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 %unint @>, emitOpCode Conv_Ovf_I2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int16 1uy @>,    emitOpCode Conv_Ovf_I2_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1.0 @>,    emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1.0f @>,   emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1L @>,     emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1 @>,      emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1s @>,     emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 %nint @>,  emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1y @>,     emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1uL @>,    emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1u @>,     emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1us @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 'a' @>,    emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 %unint @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint16 1uy @>,    emitOpCode Conv_Ovf_U2_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.char 1.0 @>,    emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1.0f @>,   emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1L @>,     emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1 @>,      emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1s @>,     emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char %nint @>,  emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1y @>,     emitOpCode Conv_Ovf_U2)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1uL @>,    emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1u @>,     emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1us @>,    emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.char 'a' @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.char %unint @>, emitOpCode Conv_Ovf_U2_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.char 1uy @>,    emitOpCode Conv_Ovf_U2_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.int 1.0 @>,    emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1.0f @>,   emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1L @>,     emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1 @>,      doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1s @>,     emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int %nint @>,  emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1y @>,     emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1uL @>,    emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1u @>,     emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1us @>,    emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int 'a' @>,    emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int %unint @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int 1uy @>,    emitOpCode Conv_Ovf_I4_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.int32 1.0 @>,    emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1.0f @>,   emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1L @>,     emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1 @>,      doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1s @>,     emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 %nint @>,  emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1y @>,     emitOpCode Conv_Ovf_I4)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1uL @>,    emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1u @>,     emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1us @>,    emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 'a' @>,    emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 %unint @>, emitOpCode Conv_Ovf_I4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int32 1uy @>,    emitOpCode Conv_Ovf_I4_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1.0 @>,    emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1.0f @>,   emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1L @>,     emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1 @>,      emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1s @>,     emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 %nint @>,  emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1y @>,     emitOpCode Conv_Ovf_U4)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1uL @>,    emitOpCode Conv_Ovf_U4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1u @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1us @>,    emitOpCode Conv_Ovf_U4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 'a' @>,    emitOpCode Conv_Ovf_U4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 %unint @>, emitOpCode Conv_Ovf_U4_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint32 1uy @>,    emitOpCode Conv_Ovf_U4_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.int64 1.0 @>,    emitOpCode Conv_Ovf_I8)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1.0f @>,   emitOpCode Conv_Ovf_I8)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1L @>,     doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1 @>,      emitOpCode Conv_Ovf_I8)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1s @>,     emitOpCode Conv_Ovf_I8)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 %nint @>,  emitOpCode Conv_Ovf_I8)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1y @>,     emitOpCode Conv_Ovf_I8)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1uL @>,    emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1u @>,     emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1us @>,    emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 'a' @>,    emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 %unint @>, emitOpCode Conv_Ovf_I8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.int64 1uy @>,    emitOpCode Conv_Ovf_I8_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1.0 @>,    emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1.0f @>,   emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1L @>,     emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1 @>,      emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1s @>,     emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 %nint @>,  emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1y @>,     emitOpCode Conv_Ovf_U8)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1uL @>,    doNothing)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1u @>,     emitOpCode Conv_Ovf_U8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1us @>,    emitOpCode Conv_Ovf_U8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 'a' @>,    emitOpCode Conv_Ovf_U8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 %unint @>, emitOpCode Conv_Ovf_U8_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.uint64 1uy @>,    emitOpCode Conv_Ovf_U8_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1.0 @>,    emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1.0f @>,   emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1L @>,     emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1 @>,      emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1s @>,     emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint %nint @>,  emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1y @>,     emitOpCode Conv_Ovf_I)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1uL @>,    emitOpCode Conv_Ovf_I_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1u @>,     emitOpCode Conv_Ovf_I_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1us @>,    emitOpCode Conv_Ovf_I_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 'a' @>,    emitOpCode Conv_Ovf_I_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint %unint @>, emitOpCode Conv_Ovf_I_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.nativeint 1uy @>,    emitOpCode Conv_Ovf_I_Un)

    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1.0 @>,    emitOpCode Conv_Ovf_U)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1.0f @>,   emitOpCode Conv_Ovf_U)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1L @>,     emitOpCode Conv_Ovf_U)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1 @>,      emitOpCode Conv_Ovf_U)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1s @>,     emitOpCode Conv_Ovf_U)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint %nint @>,  emitOpCode Conv_Ovf_U)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1y @>,     emitOpCode Conv_Ovf_U)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1uL @>,    emitOpCode Conv_Ovf_U_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1u @>,     emitOpCode Conv_Ovf_U_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1us @>,    emitOpCode Conv_Ovf_U_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 'a' @>,    emitOpCode Conv_Ovf_U_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint %unint @>, emitOpCode Conv_Ovf_U_Un)
    dict.Add(Expr.getMethodInfo <@ Checked.unativeint 1uy @>,    emitOpCode Conv_Ovf_U_Un)

    dict :> IReadOnlyDictionary<_, _>

  let private altGenericEmitterTable =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)

    dict.Add(Expr.getGenericMethodInfo <@ (~+) @>, doNothing)
    dict.Add(Expr.getGenericMethodInfo <@ (~-) @>, emitOpCode Neg)

    dict.Add(Expr.getGenericMethodInfo <@ (&&&) @>, emitOpCode And)
    dict.Add(Expr.getGenericMethodInfo <@ (|||) @>, emitOpCode Or)
    dict.Add(Expr.getGenericMethodInfo <@ (^^^) @>, emitOpCode Xor)

    dict.Add(Expr.getGenericMethodInfo <@ (byte) @>, emitOpCode Conv_U1)
    dict.Add(Expr.getGenericMethodInfo <@ (uint8) @>, emitOpCode Conv_U1)
    dict.Add(Expr.getGenericMethodInfo <@ (sbyte) @>, emitOpCode Conv_I1)
    dict.Add(Expr.getGenericMethodInfo <@ (int8) @>, emitOpCode Conv_I1)
    dict.Add(Expr.getGenericMethodInfo <@ (int16) @>, emitOpCode Conv_I2)
    dict.Add(Expr.getGenericMethodInfo <@ (uint16) @>, emitOpCode Conv_U2)
    dict.Add(Expr.getGenericMethodInfo <@ (int) @>, emitOpCode Conv_I4)
    dict.Add(Expr.getGenericMethodInfo <@ (int32) @>, emitOpCode Conv_I4)
    dict.Add(Expr.getGenericMethodInfo <@ (uint32) @>, emitOpCode Conv_U4)
    dict.Add(Expr.getGenericMethodInfo <@ (int64) @>, emitOpCode Conv_I8)
    dict.Add(Expr.getGenericMethodInfo <@ (uint64) @>, emitOpCode Conv_U8)
    dict.Add(Expr.getGenericMethodInfo <@ (nativeint) @>, emitOpCode Conv_I)
    dict.Add(Expr.getGenericMethodInfo <@ (unativeint) @>, emitOpCode Conv_U)
    dict.Add(Expr.getGenericMethodInfo <@ (float32) @>, emitOpCode Conv_R4)
    dict.Add(Expr.getGenericMethodInfo <@ (float) @>, emitOpCode Conv_R8)
    dict.Add(Expr.getGenericMethodInfo <@ (single) @>, emitOpCode Conv_R4)
    dict.Add(Expr.getGenericMethodInfo <@ (double) @>, emitOpCode Conv_R8)
    dict.Add(Expr.getGenericMethodInfo <@ (char) @>, emitOpCode Conv_U2)

    dict.Add(Expr.getGenericMethodInfo <@ try () with _ -> reraise () @>, emitOpCode Rethrow)

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
    match List.pick (fun (var, info) -> if v = var then Some info else None) !varEnv with
    | Local (local, name) ->
        [ Compiling (fun gen -> gen.Emit(ILOpCode.ldloca local name)) ]
    | Arg idx ->
        [ Compiling (fun gen -> gen.Emit(Ldarga idx)) ]
    | Field fi ->
        [ Compiling (fun gen -> gen.Emit(Ldarg_0); gen.Emit(Ldflda fi)) ]

  let private tryGetOpMethod name paramTypes (typ: Type) =
    let paramTypes = Array.ofList paramTypes
    typ.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
    |> Array.tryFind (fun mi -> mi.Name = name && (mi.GetParameters() |> Array.map (fun p -> p.ParameterType)) = paramTypes)

  let (|OpMethod|_|) expr (mi: MethodInfo) =
    let genMethod = Expr.getGenericMethodInfo expr
    if genMethod = mi.GetGenericMethodDefinition() then
      let opName = genMethod.Name
      match mi.GetParameters() |> Array.map (fun p -> p.ParameterType) with
      | [|typ|] -> typ |> tryGetOpMethod opName [typ]
      | [|typ1; typ2|] ->
          match typ1 |> tryGetOpMethod opName [typ1; typ2] with
          | Some mi -> Some mi
          | None -> typ2 |> tryGetOpMethod opName [typ1; typ2]
      | _ -> None
    else
      None

  let (|OpExplicit|_|) expr (mi: MethodInfo) =
    let genMethod = Expr.getGenericMethodInfo expr
    if genMethod = mi.GetGenericMethodDefinition() then
      let typ = mi.GetParameters().[0].ParameterType
      typ.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
      |> Array.tryFind (fun m -> m.Name = "op_Explicit" && m.ReturnType = mi.ReturnType)
    else
      None

  let private tryGetOverloadedOpEmitter (mi: MethodInfo) =
    if mi.IsGenericMethod then
      match mi with
      | OpMethod <@ (~+) @> op
      | OpMethod <@ (~-) @> op
      | OpMethod <@ (+) @> op
      | OpMethod <@ (-) @> op
      | OpMethod <@ (*) @> op
      | OpMethod <@ (/) @> op
      | OpMethod <@ (%) @> op
      | OpMethod <@ (<<<) @> op
      | OpMethod <@ (>>>) @> op
      | OpMethod <@ (&&&) @> op
      | OpMethod <@ (|||) @> op
      | OpMethod <@ (^^^) @> op
      | OpMethod <@ (~~~) @> op
      | OpMethod <@ Checked.(~-) @> op
      | OpMethod <@ Checked.(+) @> op
      | OpMethod <@ Checked.(-) @> op
      | OpMethod <@ Checked.(*) @> op
      | OpExplicit <@ byte @> op
      | OpExplicit <@ char @> op
      | OpExplicit <@ decimal @> op
      | OpExplicit <@ double @> op
      | OpExplicit <@ float @> op
      | OpExplicit <@ float32 @> op
      | OpExplicit <@ int @> op
      | OpExplicit <@ int16 @> op
      | OpExplicit <@ int32 @> op
      | OpExplicit <@ int64 @> op
      | OpExplicit <@ int8 @> op
      | OpExplicit <@ nativeint @> op
      | OpExplicit <@ sbyte @> op
      | OpExplicit <@ single @> op
      | OpExplicit <@ uint16 @> op
      | OpExplicit <@ uint32 @> op
      | OpExplicit <@ uint64 @> op
      | OpExplicit <@ uint8 @> op
      | OpExplicit <@ unativeint @> op
      | OpExplicit <@ Checked.byte @> op
      | OpExplicit <@ Checked.char @> op
      | OpExplicit <@ Checked.int @> op
      | OpExplicit <@ Checked.int16 @> op
      | OpExplicit <@ Checked.int32 @> op
      | OpExplicit <@ Checked.int64 @> op
      | OpExplicit <@ Checked.nativeint @> op
      | OpExplicit <@ Checked.sbyte @> op
      | OpExplicit <@ Checked.uint16 @> op
      | OpExplicit <@ Checked.uint32 @> op
      | OpExplicit <@ Checked.uint64 @> op
      | OpExplicit <@ Checked.unativeint @> op
          -> Some (emitCallMethod op)
      | _ -> None
    else
      None

  let private tryGetGenericEmitter (mi: MethodInfo) =
    if mi.IsGenericMethod then
      match altGenericEmitterTable.TryGetValue(mi.GetGenericMethodDefinition()) with
      | true, emitter -> Some emitter
      | _ -> None
    else
      None

  let private getPushingCompileStackInfos (recv: Expr option) (mi: MethodInfo) =
    // TODO : rewrite using option computation builder
    match altEmitterTable.TryGetValue(mi) with
    | true, emitter -> (emitter, None)
    | _ ->
        match altEmitterTableReceiver(recv, mi).TryGetValue(mi) with
        | true, emitter -> (emitter, Some loadReceiverAddress)
        | _ ->
            match tryGetOverloadedOpEmitter mi with
            | Some emitter -> (emitter, None)
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
                                   | _, gen -> emitCall gen) ]
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
