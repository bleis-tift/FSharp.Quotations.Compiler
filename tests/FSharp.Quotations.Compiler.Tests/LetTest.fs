namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module LetTest =
  [<Test>]
  let ``simple let`` () = <@ let n = 42 in n @> |> check 42

  [<Test>]
  let ``mutable let`` () =
    <@ let mutable n = 42
       ignore n
       n <- 0
       n @>
    |> check 0