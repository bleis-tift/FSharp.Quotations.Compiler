namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module LetTest =
  [<Test>]
  let ``simple let`` () = <@ let n = 42 in n @> |> check 42
