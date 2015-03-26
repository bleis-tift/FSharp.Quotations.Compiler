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
  open Microsoft.FSharp.Quotations

  type TypeBuilderWrapper with
    member this.DefineMethod(name, attrs, retType, args: Var list) =
      let argsTypes = args |> List.map (fun arg -> arg.Type)
      let m =
        MethodBuilderWrapper.Create(this.Parent, this, this.RawBuilder.DefineMethod(name, attrs, retType, Array.ofList argsTypes), name)
      #if DEBUG
      m.RawBuilder.DefineParameter(0, ParameterAttributes.Out, "") |> ignore
      for arg, i in List.zip args [1..args.Length] do
        m.RawBuilder.DefineParameter(i, ParameterAttributes.In, arg.Name) |> ignore
      #endif
      m

    member this.DefineOverrideMethod(baseType: Type, name, attrs, retType, args: Var list) =
      let argsTypes = args |> List.map (fun arg -> arg.Type)
      let m = this.DefineMethod(name, attrs ||| MethodAttributes.Virtual, retType, args)
      this.RawBuilder.DefineMethodOverride(m.RawBuilder, baseType.GetMethod(name, Array.ofList argsTypes))
      m

    member this.DefineConstructor(attrs, argNameAndTypes) =
      let argsTypes = argNameAndTypes |> List.map snd
      let c =
        CtorBuilderWrapper.Create(this.Parent, this, this.RawBuilder.DefineConstructor(attrs, CallingConventions.Standard, Array.ofList argsTypes))
      #if DEBUG
      for name, i in List.zip (argNameAndTypes |> List.map fst) [1..argNameAndTypes.Length] do
        c.RawBuilder.DefineParameter(i, ParameterAttributes.In, name) |> ignore
      #endif
      c

    member this.DefineField(name, typ, attrs) =
      this.RawBuilder.DefineField(name, typ, attrs)