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

  [<Test>]
  let ``nested let`` () =
    <@ let a = 10
       let b = a * 2
       let a = b + 2
       a @>
    |> check 22

  [<Test>]
  let ``more nested let`` () =
    <@ let a = 10
       let b =
         let a = a * 2
         a + 2
       a + b @>
    |> check 32

  [<Test>]
  let ``simple let rec`` () = <@ let rec n = 42 in n @> |> check 42

  [<Test>]
  let ``let rec list`` () =
    <@
       let rec a = 10
       and b = 20
       a + b
    @>
    |> check 30