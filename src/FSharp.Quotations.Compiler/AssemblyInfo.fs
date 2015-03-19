namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Quotations.Compiler")>]
[<assembly: AssemblyProductAttribute("FSharp.Quotations.Compiler")>]
[<assembly: AssemblyDescriptionAttribute("A compiler for F# expression tree")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
