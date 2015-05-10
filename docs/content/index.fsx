(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#I "../../bin/FSharp.Quotations.Compiler"

(**
FSharp.Quotations.Compiler
======================

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FSharp.Quotations.Compiler library can be <a href="https://nuget.org/packages/FSharp.Quotations.Compiler">installed from NuGet</a>:
      <pre>PM> Install-Package FSharp.Quotations.Compiler</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Summary
-------
FSharp.Quotations.Compiler is a library for compiling F# expression tree to IL.

There are similar libraries:

* [FSharp.Quotations.Evaluator](http://fsprojects.github.io/FSharp.Quotations.Evaluator/)
* [QuotationCompiler](https://github.com/eiriktsarpalis/QuotationCompiler)
* [Unquote](https://code.google.com/p/unquote/)

The biggest difference between FSharp.Quotations.Compiler and these is analysis policy of expression tree.
FSharp.Quotations.Compiler uses while loop rather than recursive function.
So we can use this library without thinking about stack overflow.

Example
-------

*)
#r "FSharp.Quotations.Compiler.dll"
open FSharp.Quotations.Compiler

(*** define-output: result ***)
let expr = <@ 10 + 20 @>
let res = expr.Compile()
printfn "%d" (res.ExecuteCompiledCode())

(** The output is: *)
(*** include-output: result ***)

(**

Samples & documentation
-----------------------
 * [Tutorial](tutorial.html) contains a further explanation of this library.
 * [Limitations](limitations.html) contains an explanation of limitations of this library.
 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library.

Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork
the project and submit pull requests.

The library is available under Public Domain license, which allows modification and
redistribution for both commercial and non-commercial purposes. For more information see the
[License file][license] in the GitHub repository.

  [content]: https://github.com/bleis-tift/FSharp.Quotations.Compiler/tree/master/docs/content
  [gh]: https://github.com/bleis-tift/FSharp.Quotations.Compiler
  [issues]: https://github.com/bleis-tift/FSharp.Quotations.Compiler/issues
  [license]: https://github.com/bleis-tift/FSharp.Quotations.Compiler/blob/master/LICENSE.txt
*)
