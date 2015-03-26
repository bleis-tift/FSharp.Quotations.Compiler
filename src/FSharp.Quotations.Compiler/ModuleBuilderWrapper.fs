namespace FSharp.Quotations.Compiler

open System.Reflection.Emit
open System.Diagnostics
open System.Collections.Generic

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ModuleBuilder =

  let private setDebuggingAttr (asm: AssemblyBuilder) =
    let attr = typeof<DebuggableAttribute>
    let ctor = attr.GetConstructor([| typeof<DebuggableAttribute.DebuggingModes> |])
    let builder = CustomAttributeBuilder(ctor, [| DebuggableAttribute.DebuggingModes.DisableOptimizations ||| DebuggableAttribute.DebuggingModes.Default |])
    asm.SetCustomAttribute(builder)

  let defineDynamicModule name (asm: AssemblyBuilder) =
    #if DEBUG
    setDebuggingAttr asm
    asm.DefineDynamicModule(name, true)
    #else
    asm.DefineDynamicModule(name)
    #endif

type ModuleBuilderWrapper private (builder: ModuleBuilder, name: string) =
  let countDict = Dictionary<string, int>()
  static member Create(asm: AssemblyBuilder, name: string) =
    ModuleBuilderWrapper(ModuleBuilder.defineDynamicModule name asm, name)

  member __.RawBuilder = builder
  member __.Name = name

  member __.FreshTypeName(typeNamePrefix: string) =
    match countDict.TryGetValue(typeNamePrefix) with
    | true, count ->
        let res = typeNamePrefix + (string count)
        countDict.[typeNamePrefix] <- count + 1
        res
    | false, _ ->
        countDict.Add(typeNamePrefix, 1)
        typeNamePrefix + "0"