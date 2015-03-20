namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module CoreFuncTest =
  [<Test>]
  let ``abs int`` () = <@ abs -1 @> |> check 1

  [<Test>]
  let ``box int`` () = <@ box 1 @> |> check (box 1)

  [<Test>]
  let ``compare int int`` () = <@ compare 1 1 @> |> check 0

  [<Test>]
  let ``hash int`` () = <@ hash 1 @> |> check 1

  [<Test>]
  let ``limitedHash int int`` () = <@ limitedHash 1 1 @> |> check 1

  [<Test>]
  let ``id int`` () = <@ id 1 @> |> check 1

  [<Test>]
  let ``max int int`` () = <@ max 10 20 @> |> check 20

  [<Test>]
  let ``min int int`` () = <@ min 10 20 @> |> check 10

  [<Test>]
  let ``sign int`` () = <@ sign -10 @> |> check -1

  module Unchecked =
    open Microsoft.FSharp.Core.Operators.Unchecked

    [<Test>]
    let ``Unchecked.compre int int`` () = <@ compare 1 1 @> |> check 0

    [<Test>]
    let ``Unchecked.hash int`` () = <@ hash 1 @> |> check 1

    [<Test>]
    let ``Unchecked.equals int int`` () = <@ equals 1 1 @> |> check true