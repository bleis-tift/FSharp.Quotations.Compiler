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

[<TestModule>]
module LiteralTest =
  [<Test>]
  let unit () = <@ () @> |> check ()

  #if FULLTEST
  [<Test>]
  let int ([<IntRange(-2, 128)>] i: int) = <@ i @> |> check i
  #else
  [<Test>]
  let int () = <@ 42 @> |> check 42
  #endif

  [<TestCase(0uy)>]
  [<TestCase(1uy)>]
  [<TestCase(254uy)>]
  [<TestCase(255uy)>]
  let ``byte`` (b:byte) =  <@ b @> |> check b

  // TODO : more tests
  [<Test>]
  let sbyte () = <@ 1y @> |> check 1y

  // TODO : more tests
  [<Test>]
  let int16 () = <@ 1s @> |> check 1s

  // TODO : more tests
  [<Test>]
  let uint16 () = <@ 1us @> |> check 1us

  // TODO : more tests
  [<Test>]
  let uint32 () = <@ 1u @> |> check 1u

  #if FULLTEST
  [<Test>]
  let char ([<CharRange('\000', '\128')>] c: char) = <@ c @> |> check c
  #else
  [<Test>]
  let char () = <@ 'a' @> |> check 'a'
  #endif

  [<TestCase(true)>]
  [<TestCase(false)>]
  let bool (b: bool) = <@ b @> |> check b

  [<TestCase(0L)>]
  [<TestCase(System.Int64.MaxValue)>]
  [<TestCase(System.Int64.MinValue)>]
  let int64 (i: int64) = <@ i @> |> check i

  [<TestCase(0UL)>]
  [<TestCase(System.UInt64.MaxValue)>]
  [<TestCase(System.UInt64.MinValue)>]
  let uint64 (i: uint64) = <@ i @> |> check i

  // TODO : more tests
  [<Test>]
  let bigint () = <@ 1I @> |> check 1I

  // TODO : more tests
  [<Test>]
  let float32 () = <@ 42.0f @> |> check 42.0f

  [<Test>]
  let float () = <@ 42.0 @> |> check 42.0

  // TODO : more tests
  [<Test>]
  let decimal () =
    <@ 42.0m @> |> check 42.0m
    <@ 42.0M @> |> check 42.0M

  [<TestCase("test string")>]
  [<TestCase(null: string)>]
  let string (str: string) = <@ str @> |> check str

  [<Test>]
  let array () =
    <@ [||] @> |> check [||]
    <@ [|1; 2; 3|] @> |> check [|1; 2; 3|]
    <@ [|1..3|] @> |> check [|1; 2; 3|]
    <@ [|1..2..5|] @> |> check [|1; 3; 5|]

  [<Test>]
  let list () =
    <@ [] @> |> check []
    <@ [1; 2; 3] @> |> check [1; 2; 3]
    <@ [1..3] @> |> check [1; 2; 3]
    <@ [1..2..5] @> |> check [1; 3; 5]

  [<Test>]
  let tuple () =
    <@ (1, "str") @> |> check (1, "str")
    <@ (1, 2, 3) @> |> check (1, 2, 3)

  [<Test>]
  let ``many tuple`` () =
    <@ (1, 2, 3, 4, 5, 6, 7, 8) @> |> check (1, 2, 3, 4, 5, 6, 7, 8)
    <@ (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) @> |> check (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

  [<Test>]
  let ``nested tuple`` () =
    <@ (1, ("str", true)) @> |> check (1, ("str", true))
    <@ (1, (1, 2, 3, 4, 5, 6, 7, 8)) @> |> check (1, (1, 2, 3, 4, 5, 6, 7, 8))

  [<TestCase(0uy, 1uy)>]
  [<TestCase(1uy, 10uy)>]
  [<TestCase(1uy, 255uy)>]
  let ``byte .. byte`` (a:byte, b:byte) =  <@ [a .. b] @> |> check [a .. b]

  [<TestCase(0uy, 1uy, 0uy)>]
  [<TestCase(1uy, 1uy, 1uy)>]
  [<TestCase(1uy, 3uy, 10uy)>]
  [<TestCase(2uy, 3uy, 10uy)>]
  [<TestCase(10uy, 2uy, 255uy)>]
  let ``byte .. byte .. byte`` (a:byte, b:byte, c:byte) =  <@ [a .. b .. c] @> |> check [a .. b .. c]

  [<TestCase(1uy, 10uy)>]
  let ``byte .. 0 .. byte`` (a:byte, b:byte) =  <@ [a .. 0uy .. b] @> |> checkExn<_, System.ArgumentException>
