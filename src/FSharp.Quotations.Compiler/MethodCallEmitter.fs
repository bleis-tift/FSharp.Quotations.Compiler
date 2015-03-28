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

  let private getMethod = function
  | Let (_, _, Call(_, mi, _)) -> mi
  | Call (_, mi, _) -> mi
  | expr -> failwithf "expr is not Method call: %A" expr

  let private getProperty = function
  | PropertyGet (_, pi, _) -> pi
  | expr -> failwithf "expr is not property get: %A" expr

  let typeTestGenericInt32MethodInfo = getMethod <@ box 42 :? int @>
  let genericEqualityIntrinsicM = (getMethod <@ LanguagePrimitives.HashCompare.GenericEqualityIntrinsic null null @>).GetGenericMethodDefinition()

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

  let emitCallMethod mi =
    [ Assumed (function
               | IfRet, gen -> gen.Emit(Tailcall); emitCall mi gen
               | _, gen -> emitCall mi gen) ]

  let emitCallVoidMethod mi =
    [ Assumed (function
               | IfSequential, _ -> ()
               | _, gen -> gen.Emit(Ldnull))
      Compiling (emitCall mi) ]

  let emitStrToFloat (mi: MethodInfo) =
    emitOpCode (Ldc_I4 (int NumberStyles.Float))
    |>> emitOpCode (Call (PropGet (getProperty <@ CultureInfo.InvariantCulture @>)))
    |>> emitOpCode (Unbox_Any typeof<IFormatProvider>)
    |>> emitCallMethod mi

  let private altEmitterTableUnchecked =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    dict.Add(getMethod <@ +(1) @>, doNothing)
    dict.Add(getMethod <@ +(1.0) @>, doNothing)
    dict.Add(getMethod <@ -(1) @>, emitOpCode Neg)
    dict.Add(getMethod <@ -(1.0) @>, emitOpCode Neg)
    dict.Add(getMethod <@ 'a' + 'a' @>, emitOpCode Add)
    dict.Add(getMethod <@ 1 - 1 @>, emitOpCode Sub)
    dict.Add(getMethod <@ 1.0 - 1.0 @>, emitOpCode Sub)
    dict.Add(getMethod <@ 1 / 1 @>, emitOpCode Div)
    dict.Add(getMethod <@ 1.0 / 1.0 @>, emitOpCode Div)
    dict.Add(getMethod <@ 1 % 1 @>, emitOpCode Rem)
    dict.Add(getMethod <@ 1 &&& 1 @>, emitOpCode And)
    dict.Add(getMethod <@ 1 ||| 1 @>, emitOpCode Or)
    dict.Add(getMethod <@ 1 ^^^ 1 @>, emitOpCode Xor)
    dict.Add(getMethod <@ 1 >>> 1 @>, emitOpCode Shr)
    dict.Add(getMethod <@ 1 <<< 1 @>, emitOpCode Shl)
    dict.Add(getMethod <@ ~~~1 @>, emitOpCode Not)

    dict.Add(getMethod <@ byte 1 @>, doNothing)
    dict.Add(getMethod <@ sbyte 1 @>, doNothing)
    dict.Add(getMethod <@ char 1 @>, doNothing)
    dict.Add(getMethod <@ decimal 1 @>, emitCallMethod (getMethod <@ Convert.ToDecimal(1) @>))
    dict.Add(getMethod <@ float 1 @>, emitOpCode Conv_R8)
    dict.Add(getMethod <@ float32 1 @>, emitOpCode Conv_R4)
    dict.Add(getMethod <@ int 1 @>, doNothing)
    dict.Add(getMethod <@ int16 1 @>, doNothing)
    dict.Add(getMethod <@ uint16 1 @>, doNothing)
    dict.Add(getMethod <@ int32 1 @>, doNothing)
    dict.Add(getMethod <@ uint32 1 @>, doNothing)
    dict.Add(getMethod <@ int64 1 @>, emitOpCode Conv_I8)
    dict.Add(getMethod <@ uint64 1 @>, emitOpCode Conv_I8)
    dict.Add(getMethod <@ nativeint 1 @>, emitOpCode Conv_I)
    dict.Add(getMethod <@ unativeint 1 @>, emitOpCode Conv_I)

    dict.Add(getMethod <@ byte "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseUInt32("") @>)))
                                      |>> emitOpCode Conv_Ovf_U1)
    dict.Add(getMethod <@ sbyte "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I1)
    dict.Add(getMethod <@ char "" @>, emitCallMethod (getMethod <@ Char.Parse("") @>))
    dict.Add(getMethod <@ decimal "" @>, emitStrToFloat (getMethod <@ Decimal.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(getMethod <@ float "" @>, emitStrToFloat (getMethod <@ Double.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(getMethod <@ float32 "" @>, emitStrToFloat (getMethod <@ Single.Parse("", NumberStyles.None, Unchecked.defaultof<IFormatProvider>) @>))
    dict.Add(getMethod <@ int "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(getMethod <@ int16 "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I2)
    dict.Add(getMethod <@ uint16 "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseUInt32("") @>)))
                                        |>> emitOpCode Conv_Ovf_U2)
    dict.Add(getMethod <@ int32 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(getMethod <@ uint32 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseUInt32("") @>))
    dict.Add(getMethod <@ int64 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseInt64("") @>))
    dict.Add(getMethod <@ uint64 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseUInt64("") @>))

    dict.Add(getMethod <@ byte 'a' @>, emitOpCode Conv_U1)
    dict.Add(getMethod <@ sbyte 'a' @>, emitOpCode Conv_I1)
    dict.Add(getMethod <@ char 'a' @>, doNothing)
    dict.Add(getMethod <@ float 'a' @>, emitOpCode Conv_R8)
    dict.Add(getMethod <@ float32 'a' @>, emitOpCode Conv_R4)
    dict.Add(getMethod <@ int 'a' @>, emitOpCode Conv_I4)
    dict.Add(getMethod <@ int16 'a' @>, emitOpCode Conv_I2)
    dict.Add(getMethod <@ uint16 'a' @>, emitOpCode Conv_U2)
    dict.Add(getMethod <@ int32 'a' @>, emitOpCode Conv_I4)
    dict.Add(getMethod <@ uint32 'a' @>, emitOpCode Conv_U4)
    dict.Add(getMethod <@ int64 'a' @>, emitOpCode Conv_I8)
    dict.Add(getMethod <@ uint64 'a' @>, emitOpCode Conv_U8)
    dict.Add(getMethod <@ nativeint 'a' @>, emitOpCode Conv_I)
    dict.Add(getMethod <@ unativeint 'a' @>, emitOpCode Conv_U)

    dict.Add(getMethod <@ byte 1.0 @>, emitOpCode Conv_U1)
    dict.Add(getMethod <@ sbyte 1.0 @>, emitOpCode Conv_I1)
    dict.Add(getMethod <@ char 1.0 @>, emitOpCode Conv_U2)
    dict.Add(getMethod <@ decimal 1.0 @>, emitCallMethod (getMethod <@ Convert.ToDecimal(1.0) @>))
    dict.Add(getMethod <@ float 1.0 @>, doNothing)
    dict.Add(getMethod <@ float32 1.0 @>, emitOpCode Conv_R4)
    dict.Add(getMethod <@ int 1.0 @>, emitOpCode Conv_I4)
    dict.Add(getMethod <@ int16 1.0 @>, emitOpCode Conv_I2)
    dict.Add(getMethod <@ uint16 1.0 @>, emitOpCode Conv_U2)
    dict.Add(getMethod <@ int32 1.0 @>, emitOpCode Conv_I4)
    dict.Add(getMethod <@ uint32 1.0 @>, emitOpCode Conv_U4)
    dict.Add(getMethod <@ int64 1.0 @>, emitOpCode Conv_I8)
    dict.Add(getMethod <@ uint64 1.0 @>, emitOpCode Conv_U8)
    dict.Add(getMethod <@ nativeint 1.0 @>, emitOpCode Conv_I)
    dict.Add(getMethod <@ unativeint 1.0 @>, emitOpCode Conv_U)

    dict :> IReadOnlyDictionary<_, _>

  open Microsoft.FSharp.Core.Operators.Checked

  let private altEmitterTableChecked =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    dict.Add(getMethod <@ -(1) @>, emitOpCode Ldc_I4_M1 |>> emitOpCode Mul_Ovf)
    dict.Add(getMethod <@ -(1.0) @>, emitOpCode Neg)
    dict.Add(getMethod <@ 1 - 1 @>, emitOpCode Sub_Ovf)
    dict.Add(getMethod <@ 1.0 - 1.0 @>, emitOpCode Sub)
    dict.Add(getMethod <@ 1 * 1 @>, emitOpCode Mul_Ovf)
    dict.Add(getMethod <@ 1.0 * 1.0 @>, emitOpCode Mul)
    dict.Add(getMethod <@ byte 1 @>, emitOpCode Conv_Ovf_U1)
    dict.Add(getMethod <@ sbyte 1 @>, emitOpCode Conv_Ovf_I1)
    dict.Add(getMethod <@ char 1 @>, emitOpCode Conv_Ovf_U2)
    dict.Add(getMethod <@ int 1 @>, doNothing)
    dict.Add(getMethod <@ int16 1 @>, emitOpCode Conv_Ovf_I2)
    dict.Add(getMethod <@ uint16 1 @>, emitOpCode Conv_Ovf_U2)
    dict.Add(getMethod <@ int32 1 @>, doNothing)
    dict.Add(getMethod <@ uint32 1 @>, emitOpCode Conv_Ovf_U4)
    dict.Add(getMethod <@ int64 1 @>, emitOpCode Conv_I8)
    dict.Add(getMethod <@ uint64 1 @>, emitOpCode Conv_Ovf_U8)
    dict.Add(getMethod <@ nativeint 1 @>, emitOpCode Conv_I)
    dict.Add(getMethod <@ unativeint 1 @>, emitOpCode Conv_Ovf_U)

    dict.Add(getMethod <@ byte "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseUInt32("") @>)))
                                      |>> emitOpCode Conv_Ovf_U1)
    dict.Add(getMethod <@ sbyte "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I1)
    dict.Add(getMethod <@ char "" @>, emitCallMethod (getMethod <@ Char.Parse("") @>))
    dict.Add(getMethod <@ int "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(getMethod <@ int16 "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseInt32("") @>)))
                                       |>> emitOpCode Conv_Ovf_I2)
    dict.Add(getMethod <@ uint16 "" @>, emitOpCode (Call (Method (getMethod <@ LanguagePrimitives.ParseUInt32("") @>)))
                                        |>> emitOpCode Conv_Ovf_U2)
    dict.Add(getMethod <@ int32 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseInt32("") @>))
    dict.Add(getMethod <@ uint32 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseUInt32("") @>))
    dict.Add(getMethod <@ int64 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseInt64("") @>))
    dict.Add(getMethod <@ uint64 "" @>, emitCallMethod (getMethod <@ LanguagePrimitives.ParseUInt64("") @>))

    dict.Add(getMethod <@ byte 'a' @>, emitOpCode Conv_Ovf_U1)
    dict.Add(getMethod <@ sbyte 'a' @>, emitOpCode Conv_Ovf_I1)
    dict.Add(getMethod <@ char 'a' @>, doNothing)
    dict.Add(getMethod <@ int 'a' @>, emitOpCode Conv_Ovf_I4)
    dict.Add(getMethod <@ int16 'a' @>, emitOpCode Conv_Ovf_I2)
    dict.Add(getMethod <@ uint16 'a' @>, emitOpCode Conv_Ovf_U2)
    dict.Add(getMethod <@ int32 'a' @>, emitOpCode Conv_Ovf_I4)
    dict.Add(getMethod <@ uint32 'a' @>, emitOpCode Conv_Ovf_U4)
    dict.Add(getMethod <@ int64 'a' @>, emitOpCode Conv_Ovf_I8)
    dict.Add(getMethod <@ uint64 'a' @>, emitOpCode Conv_Ovf_U8)
    dict.Add(getMethod <@ nativeint 'a' @>, emitOpCode Conv_Ovf_I)
    dict.Add(getMethod <@ unativeint 'a' @>, emitOpCode Conv_Ovf_U)

    dict.Add(getMethod <@ byte 1.0 @>, emitOpCode Conv_Ovf_U1)
    dict.Add(getMethod <@ sbyte 1.0 @>, emitOpCode Conv_Ovf_I1)
    dict.Add(getMethod <@ char 1.0 @>, emitOpCode Conv_Ovf_U2)
    dict.Add(getMethod <@ int 1.0 @>, emitOpCode Conv_Ovf_I4)
    dict.Add(getMethod <@ int16 1.0 @>, emitOpCode Conv_Ovf_I2)
    dict.Add(getMethod <@ uint16 1.0 @>, emitOpCode Conv_Ovf_U2)
    dict.Add(getMethod <@ int32 1.0 @>, emitOpCode Conv_Ovf_I4)
    dict.Add(getMethod <@ uint32 1.0 @>, emitOpCode Conv_Ovf_U4)
    dict.Add(getMethod <@ int64 1.0 @>, emitOpCode Conv_Ovf_I8)
    dict.Add(getMethod <@ uint64 1.0 @>, emitOpCode Conv_Ovf_U8)
    dict.Add(getMethod <@ nativeint 1.0 @>, emitOpCode Conv_Ovf_I)
    dict.Add(getMethod <@ unativeint 1.0 @>, emitOpCode Conv_Ovf_U)

    dict :> IReadOnlyDictionary<_, _>

  // shadowing the functions of the Microsoft.FSharp.Core.Operators.Checked module
  open Microsoft.FSharp.Core.Operators

  let loadVariableInLocalScope (r: Expr) =
    [
      Compiling (fun (gen: ILGeneratorWrapper) -> let loc = gen.DeclareLocal(r.Type) in gen.Emit(Stloc (loc, None)); gen.Emit(Ldloca (loc, None)))
    ]

  let private altEmitterTableReceiver (recv: Expr option, mi: MethodInfo) =
    let dict = Dictionary<MethodInfo, CompileStackInfo list>(identityEqualityComparer)
    match recv with
    | Some r ->
        if r.Type.IsValueType then
          if mi.DeclaringType = typeof<obj> then
            let assumed =
              [
                Assumed (function
                          | IfRet, gen -> gen.Emit(Tailcall); gen.Emit(Callvirt (Method mi))
                          | _, gen -> gen.Emit(Callvirt (Method mi)))
              ]
            dict.Add(mi, emitOpCode (Constrainted r.Type) |>> assumed)
          else
            dict.Add(mi, emitCallMethod mi)
        dict
    | _ -> dict
    :> IReadOnlyDictionary<_, _>

  let private getPushingCompileStackInfos (recv: Expr option) (mi: MethodInfo) =
    match altEmitterTableUnchecked.TryGetValue(mi) with
    | true, emitter -> (emitter, None)
    | _ ->
        match altEmitterTableChecked.TryGetValue(mi) with
        | true, emitter -> (emitter, None)
        | _ ->
            match altEmitterTableReceiver(recv, mi).TryGetValue(mi) with
            | true, emitter -> (emitter, Some (fun (r: Expr) -> loadVariableInLocalScope r))
            | _ ->
                let emitCall (gen: ILGeneratorWrapper) =
                  if mi.IsVirtual then gen.Emit(Callvirt (Method mi))
                  else gen.Emit(Call (Method mi))

                let isReturnVoid = mi.ReturnType = typeof<Void>
                let assumed = 
                  if isReturnVoid then
                    [ Assumed (function
                               | IfSequential, _ -> ()
                               | _, gen -> gen.Emit(Ldnull))
                      Compiling emitCall ]
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

  let emit (recv: Expr option, mi: MethodInfo, argsExprs: Expr list) (stack: CompileStack) =
    let (emitter, recvEmitter) = getPushingCompileStackInfos recv mi
    emitter |> List.iter stack.Push
    argsExprs |> List.rev |> List.iter (fun argExpr -> stack.Push(CompileTarget argExpr))
    match recv, recvEmitter with
    | Some r, Some re ->
        re r |> List.iter stack.Push
        stack.Push(CompileTarget r)
    | Some r, _ ->
        stack.Push(CompileTarget r)
    | _, _ -> ()
