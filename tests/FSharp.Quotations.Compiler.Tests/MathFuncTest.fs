namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module MathFuncTest =
  [<Test>]
  let ``abs int`` () = <@ abs -1 @> |> check 1

  [<Test>]
  let ``sign int`` () = <@ sign -10 @> |> check -1
