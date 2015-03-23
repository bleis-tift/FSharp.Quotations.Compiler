namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module LiteralTest =
  [<Test>]
  let int ([<IntRange(-2, 128)>] i: int) = <@ i @> |> check i

  [<TestCase(true)>]
  [<TestCase(false)>]
  let bool (b: bool) = <@ b @> |> check b

  [<TestCase("test string")>]
  [<TestCase(null: string)>]
  let string (str: string) = <@ str @> |> check str

  [<Test>]
  let array () =
    <@ [||] @> |> check [||]
    <@ [|1; 2; 3|] @> |> check [|1; 2; 3|]
    <@ [|1..3|] @> |> check [|1; 2; 3|]
    <@ [|1..2..5|] @> |> check [|1; 3; 5|]
