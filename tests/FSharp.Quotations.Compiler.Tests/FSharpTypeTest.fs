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