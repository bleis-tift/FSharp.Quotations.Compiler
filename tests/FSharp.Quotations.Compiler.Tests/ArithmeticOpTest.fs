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

  [<Test>]
  let ``int - int`` () =
    <@ 3 - 1 @> |> check 2
    <@ Int32.MinValue - 1 @> |> check Int32.MaxValue

  [<Test>]
  let ``int * int`` () =
    <@ 2 * 3 @> |> check 6
    <@ Int32.MinValue * -1 @> |> check Int32.MinValue

  [<Test>]
  let ``int / int`` () =
    <@ 5 / 2 @> |> check 2
    <@ Int32.MinValue / -1 @> |> checkExn<int, OverflowException>

  [<Test>]
  let ``int % int``() =
    <@ 5 % 2 @> |> check 1
    <@ Int32.MinValue % -1 @> |> checkExn<int, OverflowException>

  module Checked =
    open Microsoft.FSharp.Core.Operators.Checked

    [<Test>]
    let ``int + int`` () =
      <@ 1 + 2 @> |> check 3
      <@ Int32.MaxValue + 1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``int - int`` () =
      <@ 3 - 1 @> |> check 2
      <@ Int32.MinValue - 1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``int * int`` () =
      <@ 2 * 3 @> |> check 6
      <@ Int32.MinValue * -1 @> |> checkExn<_, OverflowException>