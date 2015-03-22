namespace FSharp.Quotations.Compiler

open System
open System.Reflection
open System.Reflection.Emit

type TypeBuilderWrapper private (moduleBuilder: ModuleBuilderWrapper, builder: TypeBuilder, name: string) =
  static member Create(moduleBuilder, builder, name) =
    TypeBuilderWrapper(moduleBuilder, builder, name)

  member __.Parent = moduleBuilder
  member __.RawBuilder = builder
  member __.Name = name

  member __.CreateType() = builder.CreateType()

[<AutoOpen>]
module ModuleBuilderWrapperExtension =
  type ModuleBuilderWrapper with
    member this.DefineType(name: string, attrs: TypeAttributes, baseType: Type, interfaces: Type list) =
      let typ = this.RawBuilder.DefineType(name, attrs, baseType, Array.ofList interfaces)
      TypeBuilderWrapper.Create(this, typ, name)