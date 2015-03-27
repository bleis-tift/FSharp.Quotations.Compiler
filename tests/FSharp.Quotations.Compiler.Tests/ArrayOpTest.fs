namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module ArrayOpTest =
  [<Test>]
  let ``get elem from int[]`` () =
    <@ let xs = [|0..10|] in xs.[5] @> |> check 5
    <@ let xs = [|0..10|] in xs.[100] @> |> checkExn<_, IndexOutOfRangeException>

  [<Test>]
  let ``set elem to int[]`` () =
    <@ let xs = Array.zeroCreate 10
       xs.[5] <- 10
       xs.[5] @>
    |> check 10

  [<Test>]
  let ``slice int[]`` () =
    <@ let xs = [|0..10|] in xs.[2..8] @> |> check [|0..10|].[2..8]