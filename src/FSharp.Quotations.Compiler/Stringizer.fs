namespace FSharp.Quotations.Compiler

open System
open System.Reflection

[<AutoOpen>]
module Stringizer =

  type Type with
    member this.ToReadableText() =
      match this.GetGenericArguments() with
      | null | [||] -> string this
      | genericArgs ->
          let str = string this
          match str.IndexOf("`") with
          | -1 -> str + "<" + (String.concat ", " (genericArgs |> Array.map (fun a -> a.ToReadableText()))) + ">"
          | pos -> str.Substring(0, pos) + "<" + (String.concat ", " (genericArgs |> Array.map (fun a -> a.ToReadableText()))) + ">"

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
