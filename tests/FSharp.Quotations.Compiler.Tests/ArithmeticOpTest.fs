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
  let ``+ bigint`` () =
    <@ +(1I) @> |> check (+(1I))

  [<Test>]
  let ``- bigint`` () =
    <@ -(1I) @> |> check (-(1I))

  [<Test>]
  let ``bigint + bigint`` () =
    <@ 1I + 1I @> |> check (1I + 1I)

  [<Test>]
  let ``bigint - bigint`` () =
    <@ 1I - 1I @> |> check (1I - 1I)

  [<Test>]
  let ``bigint * bigint`` () =
    <@ 1I * 1I @> |> check (1I * 1I)

  [<Test>]
  let ``bigint / bigint`` () =
    <@ 1I / 1I @> |> check (1I / 1I)

  [<Test>]
  let ``bigint % bigint`` () =
    <@ 1I % 1I @> |> check (1I % 1I)

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
  let ``+ decimal`` () =
    <@ +(1.0) @> |> check (+(1.0))

  [<Test>]
  let ``- decimal`` () =
    <@ -(1.0) @> |> check (-(1.0))

  [<Test>]
  let ``decimal + decimal`` () =
    <@ 1.0m + 1.0m @> |> check (1.0m + 1.0m)

  [<Test>]
  let ``decimal - decimal`` () =
    <@ 1.0m - 1.0m @> |> check (1.0m - 1.0m)

  [<Test>]
  let ``decimal * decimal`` () =
    <@ 1.0m * 1.0m @> |> check (1.0m * 1.0m)

  [<Test>]
  let ``decimal / decimal`` () =
    <@ 1.0m / 1.0m @> |> check (1.0m / 1.0m)

  [<Test>]
  let ``decimal % decimal`` () =
    <@ 1.0m % 1.0m @> |> check (1.0m % 1.0m)

  [<Test>]
  let ``string + string`` () =
    <@ "aaa" + "bbb" @> |> check "aaabbb"

  [<TestCase(0uy)>]
  [<TestCase(1uy)>]
  [<TestCase(254uy)>]
  [<TestCase(255uy)>]
  let ``+ byte `` (i) =  <@ +(i) @> |> check i

  [<TestCase(0uy, 1uy)>]
  [<TestCase(1uy, 254uy)>]
  [<TestCase(255uy, 0uy)>]
  [<TestCase(255uy, 1uy)>]
  let `` byte + byte `` (b1:byte, b2:byte) =  <@ b1 + b2 @> |> check(b1 + b2)

  [<TestCase(0uy, 1uy)>]
  [<TestCase(255uy, 0uy)>]
  [<TestCase(255uy, 1uy)>]
  [<TestCase(255uy, 255uy)>]
  let `` byte - byte `` (b1:byte, b2:byte) =  <@ b1 - b2 @> |> check(b1 - b2)

  [<TestCase(0uy, 1uy)>]
  [<TestCase(1uy, 1uy)>]
  [<TestCase(10uy, 2uy)>]
  [<TestCase(255uy, 1uy)>]
  [<TestCase(1uy, 255uy)>]
  let `` byte % byte `` (b1:byte, b2:byte) =  <@ b1 % b2 @> |> check(b1 % b2)

  [<TestCase(0uy, 1uy)>]
  [<TestCase(1uy, 0uy)>]
  [<TestCase(1uy, 255uy)>]
  [<TestCase(255uy, 1uy)>]
  [<TestCase(2uy, 255uy)>]
  [<TestCase(255uy, 255uy)>]
  let `` byte * byte `` (b1:byte, b2:byte) =  <@ b1 * b2 @> |> check(b1 * b2)

  [<TestCase(0uy, 1uy)>]
  [<TestCase(0uy, 255uy)>]
  [<TestCase(1uy, 1uy)>]
  [<TestCase(255uy, 255uy)>]
  [<TestCase(255uy, 1uy)>]
  [<TestCase(255uy, 5uy)>]
  let `` byte / byte `` (b1:byte, b2:byte) =  <@ b1 / b2 @> |> check(b1 / b2)

  [<TestCase(1uy, 0uy)>]
  [<TestCase(255uy, 0uy)>]
  let `` byte / 0 `` (b1:byte, b2:byte) =  <@ b1 / b2 @> |> checkExn<_, DivideByZeroException>

  [<TestCase(1uy, 0uy)>]
  [<TestCase(255uy, 0uy)>]
  let `` byte % 0 `` (b1:byte, b2:byte) =  <@ b1 % b2 @> |> checkExn<_, DivideByZeroException>

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
    let ``+ bigint`` () =
      <@ +(1I) @> |> check (+(1I))

    [<Test>]
    let ``- bigint`` () =
      <@ -(1I) @> |> check (-(1I))

    [<Test>]
    let ``bigint + bigint`` () =
      <@ 1I + 1I @> |> check (1I + 1I)

    [<Test>]
    let ``bigint - bigint`` () =
      <@ 1I - 1I @> |> check (1I - 1I)

    [<Test>]
    let ``bigint * bigint`` () =
      <@ 1I * 1I @> |> check (1I * 1I)

    [<Test>]
    let ``bigint / bigint`` () =
      <@ 1I / 1I @> |> check (1I / 1I)

    [<Test>]
    let ``bigint % bigint`` () =
      <@ 1I % 1I @> |> check (1I % 1I)

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
    let ``float - float`` () =
      <@ 3.0 - 1.0 @> |> check 2.0
      <@ Double.MinValue - Double.MaxValue @> |> check -infinity

    [<Test>]
    let ``float * float`` () =
      <@ 2.0 * 3.0 @> |> check 6.0
      <@ Double.MinValue * -1.0 @> |> check Double.MaxValue
      
    [<Test>]
    let ``+ decimal`` () =
      <@ +(1.0) @> |> check (+(1.0))

    [<Test>]
    let ``- decimal`` () =
      <@ -(1.0) @> |> check (-(1.0))

    [<Test>]
    let ``decimal + decimal`` () =
      <@ 1.0m + 1.0m @> |> check (1.0m + 1.0m)

    [<Test>]
    let ``decimal - decimal`` () =
      <@ 1.0m - 1.0m @> |> check (1.0m - 1.0m)

    [<Test>]
    let ``decimal * decimal`` () =
      <@ 1.0m * 1.0m @> |> check (1.0m * 1.0m)

    [<Test>]
    let ``decimal / decimal`` () =
      <@ 1.0m / 1.0m @> |> check (1.0m / 1.0m)

    [<Test>]
    let ``decimal % decimal`` () =
      <@ 1.0m % 1.0m @> |> check (1.0m % 1.0m)

    [<Test>]
    let ``byte - byte`` () =
      <@ 254uy - 255uy @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``byte + byte`` () =
      <@ 1uy + 255uy @> |> checkExn<_, OverflowException>

    [<TestCase(2uy, 255uy)>]
    [<TestCase(255uy, 255uy)>]
    let `` byte * byte `` (b1:byte, b2:byte) =  <@ b1 * b2 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``string + string`` () =
      <@ "aaa" + "bbb" @> |> check "aaabbb"