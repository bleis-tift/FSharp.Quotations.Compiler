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

open NUnit.Framework
open FsUnit

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Compiler
open System
open System.IO

[<AutoOpen>]
module TestUtil =
  type TestModule () =
    inherit TestFixtureAttribute()

  type Test () =
    inherit TestAttribute()

  [<AttributeUsage(AttributeTargets.Parameter, AllowMultiple=true)>]
  type Values ([<ParamArray>] arguments: obj []) =
    inherit ValuesAttribute(arguments)

  [<AttributeUsage(AttributeTargets.Method, AllowMultiple=true)>]
  type TestCase ([<ParamArray>] arguments: obj []) =
    inherit TestCaseAttribute(arguments)

  type Ignore(message:string) =
    inherit IgnoreAttribute(message)

  type IntRange(from: int, ``to``: int, step: int) =
    inherit RangeAttribute(from, ``to``, step)

    new (from, ``to``) = IntRange(from, ``to``, 1)

  type CharRange(from: char, ``to``: char) =
    inherit ValuesAttribute([|from .. ``to``|] |> Array.map (fun e -> e :> obj))

    new (from, ``to``) = CharRange(from, ``to``)

  let check (expected: 'T) (expr: Expr<'T>) =
    expr |> ExprCompiler.compile
    |> should equal expected

  let checkExn<'T, 'TExn when 'TExn :> exn> (expr: Expr<'T>) =
    try
      ExprCompiler.compile expr |> ignore
      Assert.Fail("exception is not thrown.")
    with :? 'TExn -> ()

  let checkExnMsg<'T, 'TExn when 'TExn :> exn> (expected: string) (expr: Expr<'T>) =
    try
      ExprCompiler.compile expr |> ignore
      Assert.Fail("exception is not thrown.")
    with
      :? 'TExn as e ->
        e.Message |> should equal expected

  let checkExnType (expectedType: Type) (expr: Expr<_>) =
    try
      ExprCompiler.compile expr |> ignore
      Assert.Fail("exception is not thrown.")
    with e ->
      e.GetType() |> should equal expectedType

  type RobWriter(encoding: Text.Encoding) =
    inherit TextWriter()

    let mutable result = ""

    member __.Result = result

    override __.Encoding = encoding
    override this.Write(c: char) =
      result <- result + c.ToString()

  type PrintType = Error | Out

  let private getDefaultPrinter printType: (TextWriter * (TextWriter -> unit)) =
    match printType with
    | Error -> (stderr, fun w -> System.Console.SetError(w))
    | Out -> (stdout, fun w -> System.Console.SetOut(w))

  let checkPrinted printType (expected: string) (expr: Expr<unit>) =
    let (defaultPrinter, setter) = getDefaultPrinter printType
    use writer = new RobWriter(defaultPrinter.Encoding)
    setter writer
    expr |> ExprCompiler.compile
    setter defaultPrinter
    writer.Result |> should equal expected
