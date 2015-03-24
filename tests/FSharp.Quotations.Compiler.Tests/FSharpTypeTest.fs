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