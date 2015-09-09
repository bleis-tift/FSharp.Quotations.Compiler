(*
 * FSharp.Quotations.Compiler - a compiler for F# expression tree
 * Written in 2015 by bleis-tift (hey_c_est_la_vie@hotmail.co.jp)
 * kyonmm, zakky-dev
 * 
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain worldwide.
 * This software is distributed without any warranty.
 * 
 * You should have received a copy of the CC0 Public Domain Dedication along with this software.
 * If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
 *)
namespace FSharp.Quotations.Compiler.Tests

open System
open System.Collections.Generic
open Microsoft.FSharp.Quotations

[<TestModule>]
module Limitations =

  let inline f1 x y = x + y
  let inline f2 x y = x - y

  [<Test>]
  let ``inline function`` () =
    <@ f1 20 10 @> |> check 30 // It's OK but...
    <@ f2 20 10 @> |> checkExnType (typeof<NotSupportedException>) // (-) has NoDynamicInvocationAttribute. So it throws NotSupportedException.

  [<Test>]
  let ``mutable and try finally`` () =
    // try-finally is compiled to lambda.
    <@ let mutable x = 0
       try
         // This x is the field of the lambda.
         // So this assignment does not affect to outer x.
         x <- 10
       finally
         // This x is the field of the lambda.
         // So this assignment does not affect to outer x.
         x <- 20
       x @>
    |> check 0

  [<Test>]
  let ``mutable and try with`` () =
    // try-with is compiled to lambda.
    <@ let mutable x = 0
       try
         // This x is the field of the lambda.
         // So this assignment does not affect to outer x.
         x <- 10
       with
       | _ ->
           // This x is the field of the lambda.
           // So this assignment does not affect to outer x.
           x <- 20
       x @>
    |> check 0

  let rec factOuterQuotation = function
  | 0 -> 1
  | n -> n * (factOuterQuotation (n - 1))

  [<Test>]
  let ``let rec`` () =
    <@ factOuterQuotation 5 @> |> check 120 // It's OK but...
    <@ let rec factInnerQuotation = function
       | 0 -> 1
       | n -> n * (factInnerQuotation (n - 1)) // Reference to factInnerQuotation does not exist in varenv at this point yet.
       factInnerQuotation 5 @>
    |> checkExnType (typeof<KeyNotFoundException>) // So it throws KeyNotFoundException.

  [<Test>]
  let ``for loop`` () =
    // for loop is not implemented yet.
    <@ let res = ResizeArray<_>()
       for x in 0..10 do 
         res.Add(x)
       res.ToArray() @>
    |> checkExnType (typeof<exn>)

  let cast<'T> (expr: Expr) : Expr<'T> = expr |> Expr.Cast

  [<Test>]
  let ``Expr.Value is not supported the value type that does not have the literal`` () =
    Expr.Value(DateTime.Now) |> cast<DateTime> |> checkExnType (typeof<exn>)