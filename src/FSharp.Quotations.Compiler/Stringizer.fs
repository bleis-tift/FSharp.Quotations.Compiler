﻿(*
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

open System
open System.Reflection

[<AutoOpen>]
module internal Stringizer =

  let rec private toSimpleName (typ: Type) =
    if typ = typeof<obj> then "obj"
    elif typ = typeof<unit> then "unit"
    elif typ = typeof<Void> then "unit"
    elif typ = typeof<int> then "int"
    elif typ = typeof<char> then "char"
    elif typ = typeof<float> then "float"
    elif typ = typeof<bool> then "bool"
    elif typ = typeof<string> then "string"
    elif typ = typeof<exn> then "exn"
    else
      if typ.IsGenericType then
        let genTypeDef = typ.GetGenericTypeDefinition()
        if genTypeDef = typedefof<_ list> then
          toSimpleName (typ.GetGenericArguments().[0]) + " list"
        elif genTypeDef = typedefof<_ option> then
          toSimpleName (typ.GetGenericArguments().[0]) + " option"
        elif genTypeDef = typedefof<_ seq> then
          toSimpleName (typ.GetGenericArguments().[0]) + " seq"
        elif genTypeDef = typedefof<Map<_, _>> then
          "Map<" + toSimpleName (typ.GetGenericArguments().[0]) + ", " + toSimpleName (typ.GetGenericArguments().[1]) + ">"
        elif genTypeDef = typedefof<_ -> _> then
          "(" + toSimpleName (typ.GetGenericArguments().[0]) + " -> " + toSimpleName (typ.GetGenericArguments().[1]) + ")"
        elif genTypeDef = typedefof<Tuple<_>> then
          toSimpleName (typ.GetGenericArguments().[0])
        elif genTypeDef = typedefof<_ * _> then
          String.concat " * " (typ.GetGenericArguments() |> Array.map toSimpleName)
        elif genTypeDef = typedefof<_ * _ * _> then
          String.concat " * " (typ.GetGenericArguments() |> Array.map toSimpleName)
        elif genTypeDef = typedefof<_ * _ * _ * _> then
          String.concat " * " (typ.GetGenericArguments() |> Array.map toSimpleName)
        elif genTypeDef = typedefof<_ * _ * _ * _ * _> then
          String.concat " * " (typ.GetGenericArguments() |> Array.map toSimpleName)
        elif genTypeDef = typedefof<_ * _ * _ * _ * _ * _> then
          String.concat " * " (typ.GetGenericArguments() |> Array.map toSimpleName)
        elif genTypeDef = typedefof<_ * _ * _ * _ * _ * _ * _> then
          String.concat " * " (typ.GetGenericArguments() |> Array.map toSimpleName)
        elif genTypeDef = typedefof<Tuple<_, _, _, _, _, _, _, _>> then
          let strTo7 =
            String.concat " * " (typ.GetGenericArguments().[0..6] |> Array.map toSimpleName)
          strTo7 + " * " + toSimpleName (typ.GetGenericArguments().[7])
        else
          let genArgs = typ.GetGenericArguments()
          let str = string typ
          let pos = str.IndexOf("`")
          str.Substring(0, pos) + "<" + (String.concat ", " (genArgs |> Array.map toSimpleName)) + ">"
      elif typ.IsArray then
        (toSimpleName (typ.GetElementType())) + "[]"
      else
        string typ

  type Type with
    member this.ToReadableText() = toSimpleName this

  type FieldInfo with
    member this.ToReadableText() =
      let typ = this.DeclaringType.ToReadableText()
      typ + "." + this.Name

  type MethodInfo with
    member this.ToReadableText() =
      let retType = this.ReturnType.ToReadableText()
      let selfType = this.DeclaringType.ToReadableText()
      let paramsType = this.GetParameters() |> Array.map (fun p -> p.ParameterType.ToReadableText()) |> String.concat ", "
      match this.GetGenericArguments() with
      | null | [||] -> retType + " " + selfType + "." + this.Name + "(" + paramsType + ")"
      | genericArgs ->
          let str = this.Name
          let genericArgs = genericArgs |> Array.map (fun a -> a.ToReadableText()) |> String.concat ", "
          match str.IndexOf("`") with
          | -1 -> retType + " " + selfType + "." + this.Name + "<" + genericArgs + ">(" + paramsType + ")"
          | pos -> retType + " " + selfType + "." + this.Name.Substring(0, pos) + "<" + genericArgs + ">(" + paramsType + ")"

  type ConstructorInfo with
    member this.ToReadableText() =
      let paramsType = this.GetParameters() |> Array.map (fun p -> p.ParameterType.ToReadableText()) |> String.concat ", "
      this.DeclaringType.ToReadableText() + ".ctor(" + paramsType + ")"

  type PropertyInfo with
    member this.ToReadableText() =
      let retType = this.PropertyType.ToReadableText()
      let selfType = this.DeclaringType.ToReadableText()
      let paramsType = this.GetIndexParameters() |> Array.map (fun p -> p.ParameterType.ToReadableText()) |> String.concat ", "
      retType + " " + selfType + "." + this.Name + "[" + paramsType + "]"

  let private b2s (b: byte) = b.ToString("X").PadLeft(2, '0')
  let private richInfoStr (x: float) =
    if Double.IsNaN(x) then "// NaN"
    elif Double.IsPositiveInfinity(x) then "// infinity"
    elif Double.IsNegativeInfinity(x) then "// -infinity"
    else
      let str = string x
      try
        if float str = x then "// " + str
        else "// about " + str
      with
        _ -> "// about " + str

  type Double with
    member private this.ToBytesStr() =
      "(" + (BitConverter.GetBytes(this) |> Array.map b2s |> String.concat " ") + ")"

    member this.ToStringWithRichInfo() =
      let bytesStr = this.ToBytesStr()
      bytesStr + " " + richInfoStr this

  let private richInfoStr2 (x: float32) =
    if Single.IsNaN(x) then "// NaN"
    elif Single.IsPositiveInfinity(x) then "// infinity"
    elif Single.IsNegativeInfinity(x) then "// -infinity"
    else
      let str = string x
      try
        if float32 str = x then "// " + str
        else "// about " + str
      with
        _ -> "// about " + str

  type Single with
    member private this.ToBytesStr() =
      "(" + (BitConverter.GetBytes(this) |> Array.map b2s |> String.concat " ") + ")"

    member this.ToStringWithRichInfo() =
      let bytesStr = this.ToBytesStr()
      bytesStr + " " + richInfoStr2 this