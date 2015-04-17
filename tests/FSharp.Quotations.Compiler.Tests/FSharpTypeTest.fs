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
module FSharpTypeTest =
  type Record = {
    Value1: int
    Value2: string
  }

  [<Test>]
  let ``new record`` () =
    <@ { Value1 = 10; Value2 = "str" } @> |> check { Value1 = 10; Value2 = "str" }

  [<Test>]
  let ``new generic record`` () =
    <@ { contents = 10 } @> |> check (ref 10)

  type SimpleDU = Tag

  [<Test>]
  let ``new simple DU`` () = <@ Tag @> |> check Tag

  type SimpleDUWithValue = Tag of int

  [<Test>]
  let ``new simple DU with value`` () = <@ Tag 42 @> |> check (Tag 42)

  [<Test>]
  let ``option`` () =
    <@ Some 42 @> |> check (Some 42)
    <@ None @> |> check None

  [<Test>]
  let ``tuple get`` () =
    <@ let a, b = 10, "hoge" in string a + b @>
    |> check "10hoge"

  [<Test>]
  let ``10 tuple get`` () =
    <@
       let x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
       [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10]
    @>
    |> check [1..10]

  [<Test>]
  let ``15 tuple get`` () =
    <@
       let x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
       [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12; x13; x14; x15]
    @>
    |> check [1..15]

  [<Test>]
  let ``create ref`` () =
    <@ ref 42 @> |> check (ref 42)
    <@ ref "" @> |> check (ref "")

  [<Test>]
  let ``reference ref`` () =
    <@ let x = ref 42 in !x @> |> check 42
    <@ let x = ref "" in !x @> |> check ""

  [<Test>]
  let ``substitute ref`` () =
    <@ let x = ref 42 in x := 10; !x @> |> check 10
    <@ let x = ref "" in x := "str"; !x @> |> check "str"

  [<Test>]
  let ``incr ref`` () = <@ let x = ref 0 in incr x; !x @> |> check 1

  [<Test>]
  let ``decr ref`` () = <@ let x = ref 0 in decr x; !x @> |> check -1

  [<TestCase(true, true)>]
  [<TestCase(true, false)>]
  [<TestCase(false, true)>]
  [<TestCase(false, false)>]
  let ``partial apply`` (a, b) =
    <@ let f x = (||) x in f a b @> |> check (a || b)
    <@ let f x = (&&) x in f a b @> |> check (a && b)