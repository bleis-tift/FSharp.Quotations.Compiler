namespace FSharp.Quotations.Compiler

open System.Reflection.Emit
open System.Diagnostics

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
  static member Create(asm: AssemblyBuilder, name: string) =
    ModuleBuilderWrapper(ModuleBuilder.defineDynamicModule name asm, name)

  member __.RawBuilder = builder
  member __.Name = name