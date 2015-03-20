namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module BitOpTest =
  [<Test>]
  let ``int &&& int`` () = <@ 42 &&& 63 @> |> check 42

  [<Test>]
  let ``int ||| int`` () = <@ 42 ||| 0 @> |> check 42