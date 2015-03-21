namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module StringFuncTest =
  [<Test>]
  let ``String.length`` () =
    <@ String.length "str" @> |> check 3
    <@ String.length null @> |> check 0