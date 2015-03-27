namespace FSharp.Quotations.Compiler

open System.Reflection
open System.Reflection.Emit

module DebugUtil =

  let assemblyBuilderAccess =
    #if DEBUG
    AssemblyBuilderAccess.RunAndSave
    #else
    AssemblyBuilderAccess.Run
    #endif

  let assemblyFilePath =
    #if DEBUG
    Some "output.dll"
    #else
    None
    #endif

  let label (gen: ILGenerator) =
    #if DEBUG
    "IL_" + gen.ILOffset.ToString("X").PadLeft(4, '0') + ": "
    #else
    ""
    #endif

  let save (asm: AssemblyBuilder) =
    match assemblyFilePath with
    | Some path -> asm.Save(path)
    | None -> ()   