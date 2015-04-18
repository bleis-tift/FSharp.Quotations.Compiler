(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#I "../../bin/FSharp.Quotations.Compiler"

#r "FSharp.Quotations.Compiler.dll"

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Compiler

(**
Limitations
===========
This library has some limitations.

Not Implemented Yet
-------------------
* `ForIntegerRangeLoop`
* `LetRecursive`
* `NewDelegate`
* `WhileLoop`

Unupported
----------
* `AddressOf`
* `AddressSet`
* `Quote`

Technical Problem
-----------------
### inline function
The inline functions that contains other inline function with `NoDynamicInvocationAttribute` can not invoke.
For example, the following code throws `System.NotSupportedException`.
*)

// (-) has NoDynamicInvocationAttribute.
let inline f1 x y = x - y
try
  let expr = <@ f1 20 10 @>
  expr.Execute() |> ignore
with
  :? System.NotSupportedException -> printfn "raised exception."

(**
Of course, the inline function with `NoDynamicInvocationAttribute` can not execute using this library.
*)

[<NoDynamicInvocationAttribute >]
let inline f2 x = x
try
  let expr = <@ f2 10 @>
  expr.Execute() |> ignore
with
  :? System.NotSupportedException -> printfn "raised exception."

(**
If you want to execute the function that contains other inline function with `NoDynamicInvocationAttribute`,
you need to remove `inline` or to inline by hand.
*)

let expr = <@ 20 - 10 @>
printfn "20 - 10 = %d" (expr.Execute())

(**
In other case, the inline functions that contains member constraint invocation expressions can not also execute by this library.
There is no workarround.

### mutable and try-with/try-finally
This library wraps `try-with` and `try-finally` in the lambda expression because they are expression that has the value.
*)

let tryWithExpr =
  <@ let x = 10
     let res =
       try x with _ -> 20
     res * 2 @>
let compiledTryWithExpr = tryWithExpr.Compile()

(**
It is compiled as following.
*)

type lambda0(x) =
  inherit FSharpFunc<unit, int>()

  override __.Invoke(_) =
    try x with _ -> 20

let x = 10
let res = lambda0(x).Invoke()
res * 2

(**
The `x` is compiled to the field of the lambda class.
So rewrite the `x` in the body of the lambda expression, it does not affect the outside of the lambda expression.
*)

let letMutableAndTryWithExpr =
  <@ let mutable y = 0
     let res2 =
       try y <- 10; y with _ -> 20
     (y, res2) @>
let compiledLetMutableAndTryWithExpr = letMutableAndTryWithExpr.Compile()

(**
It is compiled as following.
*)

type lambda1(y) =
  inherit FSharpFunc<unit, int>()

  member val y = y with get, set
  override this.Invoke(_) =
    try this.y <- 10; y with _ -> 20

let mutable y = 0
let tmp = lambda1(y)
let res2 = tmp.Invoke()
// should assign after invocation of the lambda expression.
// But now implementation does not assign.
// y <- tmp.y
(y, res2)

(**
### Expr.Value
The `Expr.Value` is not supported the type that does not have the literal.
*)

let valueExpr: Expr<System.DateTime> =
  Expr.Value(System.DateTime.UtcNow)
  |> Expr.Cast

try
  valueExpr.Execute() |> ignore
with
  e -> printfn "%A" e

(**
You should use the quoted expression instead of using the `Expr.Value`.
*)

let codeQuote = <@ System.DateTime.UtcNow @>
printfn "%A" (codeQuote.Execute())

(**
The above quoted expression does not become the `Expr.Value`.
It becomes the `Expr.PropertyGet`.
*)