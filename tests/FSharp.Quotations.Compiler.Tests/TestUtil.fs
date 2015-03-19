namespace FSharp.Quotations.Compiler.Tests

open NUnit.Framework
open FsUnit

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Compiler

[<AutoOpen>]
module TestUtil =
  let check (expected: 'T) (expr: Expr<'T>) =
    expr |> ExprCompiler.compile
    |> should equal expected

  let checkExn<'TExn when 'TExn :> exn> (expr: Expr<_>) =
    try
      ExprCompiler.compile expr |> ignore
      Assert.Fail("exception is not thrown.")
    with :? 'TExn -> ()