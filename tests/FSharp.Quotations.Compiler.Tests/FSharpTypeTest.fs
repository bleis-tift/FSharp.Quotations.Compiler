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