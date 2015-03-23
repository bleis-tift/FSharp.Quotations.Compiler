namespace FSharp.Quotations.Compiler

open System
open System.Reflection.Emit
open System.Diagnostics.SymbolStore
open System.IO

type ILGeneratorWrapper private (builder: IGeneratorProvider, gen: ILGenerator, name: string, doc: ISymbolDocumentWriter option) =
  let mutable lineNumber = 1
  let mutable indentCount = 0
  let writer = doc |> Option.map (fun d -> File.CreateText(name + ".il"))

  let pushIndent () = indentCount <- indentCount + 2
  let popIndent () = indentCount <- indentCount - 2
  let withIndent(str) =
    (String.replicate indentCount " ") + str

  static member Create(builder, gen, name, doc) =
    ILGeneratorWrapper(builder, gen, name, doc)

  member __.Write(str: string) = writer |> Option.iter (fun w -> w.Write(withIndent str))
  member __.WriteLine(line: string) =
    writer |> Option.iter (fun w ->
      w.WriteLine(withIndent line)
      lineNumber <- lineNumber + 1
    )
  member __.WriteLineAndMark(line: string) =
    writer |> Option.iter (fun w ->
      w.WriteLine(withIndent line)
      gen.MarkSequencePoint(doc.Value, lineNumber, indentCount + 1, lineNumber, indentCount + line.Length + 1)
      lineNumber <- lineNumber + 1
    )
  member __.Close() = writer |> Option.iter (fun w -> w.Close())

  member __.DeclareLocal(typ: Type) = gen.DeclareLocal(typ)

  member this.BeginExceptionBlock() =
    this.WriteLine("begin try")
    pushIndent ()
    gen.BeginExceptionBlock()
  member this.BeginCatchBlock(typ: Type) =
    popIndent ()
    this.WriteLine("begin catch " + typ.ToReadableText())
    pushIndent ()
    gen.BeginCatchBlock(typ)
  member this.BeginFinallyBlock() =
    popIndent ()
    this.WriteLine("begin finally")
    pushIndent ()
    gen.BeginFinallyBlock()
  member this.EndExceptionBlock() =
    popIndent ()
    this.WriteLine("end")
    gen.EndExceptionBlock()

  member this.DefineLabel() = gen.DefineLabel()
  member this.MarkLabel(label) =
    this.Write(string (label.GetHashCode()) + ": ")
    gen.MarkLabel(label)

  member this.Emit(opcode) =
    let raw = ILOpCode.toRawOpCode opcode
    match opcode with
    | Brfalse label | Br label ->
        this.WriteLineAndMark(raw.Name + " " + string (label.GetHashCode()))
        gen.Emit(raw, label)
    | Stloc local | Ldloc local ->
        this.WriteLineAndMark(raw.Name + " " + string local.LocalIndex)
        gen.Emit(raw, local)
    | Ldsfld fld ->
        this.WriteLineAndMark(raw.Name + " " + (fld.ToReadableText()))
        gen.Emit(raw, fld)
    | Ldstr str ->
        this.WriteLineAndMark(raw.Name + " \"" + str + "\"")
        gen.Emit(raw, str)
    | Ldc_I4_S i | Ldc_I4 i | Ldarg i | Starg i ->
        this.WriteLineAndMark(raw.Name + " " + string i)
        gen.Emit(raw, i)
    | Call target ->
        match target with
        | Method mi ->
            this.WriteLineAndMark(raw.Name + " " + mi.ToReadableText())
            gen.Emit(raw, mi)
        | Ctor ci ->
            this.WriteLineAndMark(raw.Name + " " + ci.ToReadableText())
            gen.Emit(raw, ci)
        | PropGet pi ->
            this.WriteLineAndMark(raw.Name + " " + pi.ToReadableText())
            gen.Emit(raw, pi.GetMethod)
    | Newarr typ | Stelem typ | Box typ | Unbox_Any typ ->
        this.WriteLineAndMark(raw.Name + " " + typ.ToReadableText())
        gen.Emit(raw, typ)
    | Newobj ci ->
        this.WriteLineAndMark(raw.Name + " " + ci.ToReadableText())
        gen.Emit(raw, ci)
    | And | Or | Xor | Not | Shl | Shr | Div | Mul_Ovf | Neg | Rem | Sub | Sub_Ovf
    | Conv_I | Conv_I4 | Conv_I8 | Conv_R4 | Conv_R8 | Conv_Ovf_I1 | Conv_Ovf_I2 | Conv_Ovf_U | Conv_Ovf_U1 | Conv_Ovf_U2 | Conv_Ovf_U4 | Conv_Ovf_U8
    | Ldarg_0 | Ldarg_1 | Ldarg_2 | Ldarg_3
    | Ldnull | Ldc_I4_M1 | Ldc_I4_0 | Ldc_I4_1 | Ldc_I4_2 | Ldc_I4_3 | Ldc_I4_4 | Ldc_I4_5 | Ldc_I4_6 | Ldc_I4_7 | Ldc_I4_8
    | Tailcall | Dup | Pop | Ret ->
        this.WriteLineAndMark(raw.Name)
        gen.Emit(raw)

[<AutoOpen>]
module GeneratorProvidersExtension =
  let private defineDoc (name: string) (builder: ModuleBuilderWrapper) =
    #if DEBUG
    Some (builder.RawBuilder.DefineDocument(name + ".il", SymDocumentType.Text, SymLanguageType.ILAssembly, SymLanguageVendor.Microsoft))
    #else
    None
    #endif

  type MethodBuilderWrapper with
    member this.GetILGenerator() =
      let gen = this.GetGenerator()
      let doc = defineDoc this.FullName this.ModuleBuilder
      ILGeneratorWrapper.Create(this, gen, this.FullName, doc)

  type CtorBuilderWrapper with
    member this.GetILGenerator() =
      let gen = this.GetGenerator()
      let doc = defineDoc this.FullName this.ModuleBuilder
      ILGeneratorWrapper.Create(this, gen, this.FullName, doc)