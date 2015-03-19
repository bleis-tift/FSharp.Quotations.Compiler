namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module ArithmeticOpTest =
  [<Test>]
  let ``+ int`` () = <@ +(1) @> |> check 1

  [<Test>]
  let ``- int`` () =
    <@ -(1) @> |> check -1
    <@ - Int32.MinValue @> |> check Int32.MinValue

  [<Test>]
  let ``int + int`` () =
    <@ 1 + 2 @> |> check 3
    <@ Int32.MaxValue + 1 @> |> check Int32.MinValue