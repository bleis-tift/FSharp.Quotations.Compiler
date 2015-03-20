namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module BitOpTest =
  [<Test>]
  let ``int &&& int`` () = <@ 42 &&& 63 @> |> check 42

  [<Test>]
  let ``int ||| int`` () = <@ 42 ||| 0 @> |> check 42

  [<Test>]
  let ``int ^^^ int`` () = <@ 1 ^^^ 1 @> |> check 0

  [<Test>]
  let ``int >>> int`` () =
    <@ 42 >>> 1 @> |> check 21
    <@ 42 >>> 31 @> |> check 0
    <@ -1 >>> 1 @> |> check -1
    <@ -1 >>> 31 @> |> check -1
    <@ -1 >>> 32 @> |> check -1

  [<Test>]
  let ``int <<< int`` () =
    <@ 42 <<< 1 @> |> check 84
    <@ Int32.MaxValue <<< 1 @> |> check -2
    <@ Int32.MinValue <<< 1 @> |> check 0