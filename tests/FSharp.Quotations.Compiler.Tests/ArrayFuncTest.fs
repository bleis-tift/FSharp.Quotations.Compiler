namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ArrayFuncTest =
  [<Test>]
  let ``Array.empty`` () =
    <@ Array.empty: int[] @> |> check [||]
    <@ Array.empty: string[] @> |> check [||]

  [<Test>]
  let ``Array.init`` () =
    <@ Array.init 3 id @> |> check [|0; 1; 2|]
    <@ Array.init 3 string @> |> check [|"0"; "1"; "2"|]

  [<Test>]
  let ``Array.create`` () =
    <@ Array.create 3 1 @> |> check [|1; 1; 1|]
    <@ Array.create 3 "str" @> |> check [|"str"; "str"; "str"|]

  [<Test>]
  let ``Array.zeroCreate`` () =
    <@ Array.zeroCreate 3 @> |> check [|0; 0; 0|]
    <@ Array.zeroCreate 3 @> |> check [|null; null; null|]

  [<Test>]
  let ``Array.length`` () = <@ Array.length [|0; 1; 2|] @> |> check 3

  [<Test>]
  let ``Array.Length`` () = <@ [|0; 1; 2|].Length @> |> check 3

  [<Test>]
  let ``Array.Map`` () = <@ [|0..10|] |> Array.filter (fun x -> x % 5 = 0) @> |> check [|0; 5; 10|]