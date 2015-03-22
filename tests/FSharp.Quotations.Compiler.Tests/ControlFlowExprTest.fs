namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ControlFlowExprTest =
  [<Test>]
  let ``try-with`` () =
    <@ try 10 with _e -> -1 @> |> check 10
    <@ try failwith "" with _e -> -1 @> |> check -1

  [<Test>]
  let ``try-finally`` () =
    <@ try 10 finally () @> |> check 10
