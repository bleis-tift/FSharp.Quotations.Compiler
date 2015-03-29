namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module LiteralTest =
  [<Test>]
  let unit () = <@ () @> |> check ()

  [<Test>]
  let int ([<IntRange(-2, 128)>] i: int) = <@ i @> |> check i

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

  [<Test>]
  let char ([<CharRange('\000', '\128')>] c: char) = <@ c @> |> check c

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
  let float32 () = <@ 42.0f @> |> check 42.0f

  [<Test>]
  let float () = <@ 42.0 @> |> check 42.0

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