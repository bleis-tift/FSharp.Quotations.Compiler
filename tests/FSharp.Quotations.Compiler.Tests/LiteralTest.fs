namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module LiteralTest =
  [<Test>]
  let int ([<IntRange(-2, 128)>] i: int) = <@ i @> |> check i