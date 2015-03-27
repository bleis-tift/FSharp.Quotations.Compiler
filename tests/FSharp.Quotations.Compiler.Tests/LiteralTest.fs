namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module LiteralTest =
  [<Test>]
  let unit () = <@ () @> |> check ()

  [<Test>]
  let int ([<IntRange(-2, 128)>] i: int) = <@ i @> |> check i

  [<Test>]
  let char ([<CharRange('\000', '\128')>] c: char) = <@ c @> |> check c

  [<TestCase(true)>]
  [<TestCase(false)>]
  let bool (b: bool) = <@ b @> |> check b

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