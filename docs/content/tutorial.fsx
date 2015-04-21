(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#I "../../bin/FSharp.Quotations.Compiler"

#r "FSharp.Quotations.Compiler.dll"

(**
Tutorial
========
This library use easily.

In order to use this library, open `FSharp.Quotations.Compiler` namespace.
*)
open FSharp.Quotations.Compiler

(**
Typed Expression Tree
---------------------
Suppose that you want to execute the following expression tree.
*)

let x = 10
let y = 20
let expr = <@ x + y @>

(**
You just use `Execute` method.
*)

(*** define-output: result1 ***)
printfn "%d + %d = %d" x y (expr.Execute())

(**
This code results as follows:
*)
(*** include-output: result1 ***)

(**
If you want to compile the expression tree and execute it later,
you use `Compile` method.
*)

let result = expr.Compile()

(**
Or you can use `ExprCompiler.compile` function as same as using `Compile` method.
*)

let result2 = expr |> ExprCompiler.compile

(**
In order to execute `result`, you use `ExecuteCompiledCode` method.
*)

printfn "%d + %d = %d" x y (result.ExecuteCompiledCode())

(**
Untyped Expression Tree
-----------------------
This library does not support the untyped expression tree directly.
If you want to execute untyped expression tree then use `Expr.Cast` method.
It is the method of the F# standard library.
*)

open Microsoft.FSharp.Quotations

// helper function for casting
let cast<'T> (expr: Expr) : Expr<'T> = expr |> Expr.Cast

let untypedExpr = <@@ x + y @@>
let casted = untypedExpr |> cast<int>
printfn "%d + %d = %d" x y (casted.Execute())
