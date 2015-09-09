namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Quotations.Compiler")>]
[<assembly: AssemblyProductAttribute("FSharp.Quotations.Compiler")>]
[<assembly: AssemblyDescriptionAttribute("A compiler for F# expression tree")>]
[<assembly: AssemblyVersionAttribute("0.4")>]
[<assembly: AssemblyFileVersionAttribute("0.4")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4"
