namespace FSharp.Quotations.Compiler

open System
open System.Reflection
open System.Reflection.Emit

type CallTarget =
  | Method of MethodInfo
  | Ctor of ConstructorInfo
  | PropGet of PropertyInfo

type ILOpCode =
  | And
  | Or
  | Xor
  | Not
  | Shl
  | Shr
  | Div
  | Mul_Ovf
  | Neg
  | Rem
  | Sub
  | Sub_Ovf
  | Brfalse of Label
  | Br of Label
  | Conv_I
  | Conv_I4
  | Conv_I8
  | Conv_R4
  | Conv_R8
  | Conv_Ovf_I1
  | Conv_Ovf_I2
  | Conv_Ovf_U
  | Conv_Ovf_U1
  | Conv_Ovf_U2
  | Conv_Ovf_U4
  | Conv_Ovf_U8
  | Unbox_Any of Type
  | Stloc of LocalBuilder
  | Ldloc of LocalBuilder
  | Ldarg_0
  | Ldarg_1
  | Ldarg_2
  | Ldarg_3
  | Ldarg of int
  | Ldsfld of FieldInfo
  | Ldnull
  | Ldstr of string
  | Ldc_I4_M1
  | Ldc_I4_0
  | Ldc_I4_1
  | Ldc_I4_2
  | Ldc_I4_3
  | Ldc_I4_4
  | Ldc_I4_5
  | Ldc_I4_6
  | Ldc_I4_7
  | Ldc_I4_8
  | Ldc_I4_S of int
  | Ldc_I4 of int
  | Tailcall
  | Call of CallTarget
  | Newobj of ConstructorInfo
  | Pop
  | Ret

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ILOpCode =
  let toRawOpCode = function
  | And -> OpCodes.And
  | Or -> OpCodes.Or
  | Xor -> OpCodes.Xor
  | Not -> OpCodes.Not
  | Shl -> OpCodes.Shl
  | Shr -> OpCodes.Shr
  | Div -> OpCodes.Div
  | Mul_Ovf -> OpCodes.Mul_Ovf
  | Neg -> OpCodes.Neg
  | Rem -> OpCodes.Rem
  | Sub -> OpCodes.Sub
  | Sub_Ovf -> OpCodes.Sub_Ovf
  | Brfalse _ -> OpCodes.Brfalse
  | Br _ -> OpCodes.Br
  | Conv_I -> OpCodes.Conv_I
  | Conv_I4 -> OpCodes.Conv_I4
  | Conv_I8 -> OpCodes.Conv_I8
  | Conv_R4 -> OpCodes.Conv_R4
  | Conv_R8 -> OpCodes.Conv_R8
  | Conv_Ovf_I1 -> OpCodes.Conv_Ovf_I1
  | Conv_Ovf_I2 -> OpCodes.Conv_Ovf_I2
  | Conv_Ovf_U -> OpCodes.Conv_Ovf_U
  | Conv_Ovf_U1 -> OpCodes.Conv_Ovf_U1
  | Conv_Ovf_U2 -> OpCodes.Conv_Ovf_U2
  | Conv_Ovf_U4 -> OpCodes.Conv_Ovf_U4
  | Conv_Ovf_U8 -> OpCodes.Conv_Ovf_U8
  | Unbox_Any _ -> OpCodes.Unbox_Any
  | Stloc _ -> OpCodes.Stloc
  | Ldloc _ -> OpCodes.Ldloc
  | Ldarg_0 -> OpCodes.Ldarg_0
  | Ldarg_1 -> OpCodes.Ldarg_1
  | Ldarg_2 -> OpCodes.Ldarg_2
  | Ldarg_3 -> OpCodes.Ldarg_3
  | Ldarg _ -> OpCodes.Ldarg
  | Ldsfld _ -> OpCodes.Ldsfld
  | Ldnull -> OpCodes.Ldnull
  | Ldstr _ -> OpCodes.Ldstr
  | Ldc_I4_M1 -> OpCodes.Ldc_I4_M1
  | Ldc_I4_0 -> OpCodes.Ldc_I4_0
  | Ldc_I4_1 -> OpCodes.Ldc_I4_1
  | Ldc_I4_2 -> OpCodes.Ldc_I4_2
  | Ldc_I4_3 -> OpCodes.Ldc_I4_3
  | Ldc_I4_4 -> OpCodes.Ldc_I4_4
  | Ldc_I4_5 -> OpCodes.Ldc_I4_5
  | Ldc_I4_6 -> OpCodes.Ldc_I4_6
  | Ldc_I4_7 -> OpCodes.Ldc_I4_7
  | Ldc_I4_8 -> OpCodes.Ldc_I4_8
  | Ldc_I4_S _ -> OpCodes.Ldc_I4_S
  | Ldc_I4 _ -> OpCodes.Ldc_I4
  | Tailcall -> OpCodes.Tailcall
  | Call _ -> OpCodes.Call
  | Newobj _ -> OpCodes.Newobj
  | Pop -> OpCodes.Pop
  | Ret -> OpCodes.Ret