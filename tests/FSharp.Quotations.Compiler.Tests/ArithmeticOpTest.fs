namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ArithmeticOpTest =
  [<Test>]
  let ``+ int`` () = <@ +(1) @> |> check 1