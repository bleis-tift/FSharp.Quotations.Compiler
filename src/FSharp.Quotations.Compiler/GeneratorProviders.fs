namespace FSharp.Quotations.Compiler

open System
open System.Reflection
open System.Reflection.Emit

type IGeneratorProvider =
  abstract GetGenerator: unit -> ILGenerator

type MethodBuilderWrapper private (moduleBuilder: ModuleBuilderWrapper, typeBuilder: TypeBuilderWrapper, builder: MethodBuilder, name: string) =
  static member Create(moduleBuilder, typeBuilder, builder, name) =
    MethodBuilderWrapper(moduleBuilder, typeBuilder, builder, name)

  member __.ModuleBuilder = moduleBuilder
  member __.RawBuilder = builder
  member __.Name = name
  member __.FullName = moduleBuilder.Name + "." + typeBuilder.Name + "." + name
  member __.GetGenerator() = builder.GetILGenerator()

  interface IGeneratorProvider with
    member this.GetGenerator() = this.GetGenerator()

type CtorBuilderWrapper private (moduleBuilder: ModuleBuilderWrapper, typeBuilder: TypeBuilderWrapper, builder: ConstructorBuilder) =
  static member Create(moduleBuilder, typeBuilder, builder) =
    CtorBuilderWrapper(moduleBuilder, typeBuilder, builder)

  member __.ModuleBuilder = moduleBuilder
  member __.RawBuilder = builder
  member __.FullName = moduleBuilder.Name + "." + typeBuilder.Name + ".ctor"
  member __.GetGenerator() = builder.GetILGenerator()

  interface IGeneratorProvider with
    member this.GetGenerator() = this.GetGenerator()

[<AutoOpen>]
module TypeBuilderWrapperExtension =
  type TypeBuilderWrapper with
    member this.DefineMethod(name, attrs, retType, argsTypes) =
      MethodBuilderWrapper.Create(this.Parent, this, this.RawBuilder.DefineMethod(name, attrs, retType, Array.ofList argsTypes), name)

    member this.DefineOverrideMethod(baseType: Type, name, attrs, retType, argsTypes) =
      let m = this.DefineMethod(name, attrs ||| MethodAttributes.Virtual, retType, argsTypes)
      this.RawBuilder.DefineMethodOverride(m.RawBuilder, baseType.GetMethod(name, Array.ofList argsTypes))
      m

    member this.DefineConstructor(attrs, argsTypes) =
      CtorBuilderWrapper.Create(this.Parent, this, this.RawBuilder.DefineConstructor(attrs, CallingConventions.Standard, Array.ofList argsTypes))

    member this.DefineField(name, typ, attrs) =
      this.RawBuilder.DefineField(name, typ, attrs)