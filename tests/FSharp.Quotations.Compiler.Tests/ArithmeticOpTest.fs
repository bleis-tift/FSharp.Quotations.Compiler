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
    <@ Int32.MinValue / -1 @> |> checkExn<_, OverflowException>
    <@ 1 / 0 @> |> checkExn<_, DivideByZeroException>

  [<Test>]
  let ``int % int``() =
    <@ 5 % 2 @> |> check 1
    <@ Int32.MinValue % -1 @> |> checkExn<_, OverflowException>
    <@ 1 % 0 @> |> checkExn<_, DivideByZeroException>

  [<Test>]
  let ``char + char`` () =
    <@ 'a' + 'a' @> |> check ('a' + 'a')
    <@ Char.MaxValue + (char 1) @> |> check (Char.MaxValue + (char 1))

  [<Test>]
  let ``+ float`` () = <@ +(1.0) @> |> check 1.0

  [<Test>]
  let ``- float`` () =
    <@ -(1.0) @> |> check -1.0
    <@ - (Double.MinValue) @> |> check Double.MaxValue

  [<Test>]
  let ``float + float`` () =
    <@ 1.0 + 2.0 @> |> check 3.0
    <@ Double.MaxValue + Double.MaxValue @> |> check infinity

  [<Test>]
  let ``float - float`` () =
    <@ 3.0 - 1.0 @> |> check 2.0
    <@ Double.MinValue - Double.MaxValue @> |> check -infinity

  [<Test>]
  let ``float * float`` () =
    <@ 2.0 * 3.0 @> |> check 6.0
    <@ Double.MinValue * -1.0 @> |> check Double.MaxValue

  [<Test>]
  let ``float ** float`` () = <@ 3.0 ** 2.0 @> |> check 9.0

  [<Test>]
  let ``float / float`` () =
    <@ 5.0 / 2.0 @> |> check 2.5
    <@ 1.0 / 0.0 @> |> check infinity
    <@ 1.0 / -0.0 @> |> check -infinity
    <@ 0.0 / 0.0 @> |> check nan

  [<Test>]
  let ``string + string`` () =
    <@ "aaa" + "bbb" @> |> check "aaabbb"

  module Checked =
    open Microsoft.FSharp.Core.Operators.Checked

    [<Test>]
    let ``- int`` () =
      <@ -(1) @> |> check -1
      <@ - Int32.MinValue @> |> checkExn<_, OverflowException>

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

    [<Test>]
    let ``char + char`` () =
      <@ 'a' + 'a' @> |> check ('a' + 'a')
      <@ Char.MaxValue + (char 1) @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``- float`` () =
      <@ -(1.0) @> |> check -1.0
      <@ - (Double.MinValue) @> |> check Double.MaxValue

    [<Test>]
    let ``float + float`` () =
      <@ 1.0 + 2.0 @> |> check 3.0
      <@ Double.MaxValue + Double.MaxValue @> |> check infinity

    [<Test>]
    let ``string + string`` () =
      <@ "aaa" + "bbb" @> |> check "aaabbb"