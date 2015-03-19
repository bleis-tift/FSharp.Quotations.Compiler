namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ArithmeticOpTest =
  [<Test>]
  let ``+ int`` () = <@ +(1) @> |> check 1

  [<Test>]
  let ``- int`` () =
    <@ -(1) @> |> check -1
    <@ - System.Int32.MinValue @> |> check (-System.Int32.MinValue)