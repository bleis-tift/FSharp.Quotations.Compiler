namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module RangeOpTest =
  [<Test>]
  let ``byte .. byte`` () =
    <@ seq { 1uy..255uy } @> |> check (seq { 1uy..255uy })
    <@ [ 1uy..255uy ] @> |> check [ 1uy..255uy ]

  [<Test>]
  let ``byte .. byte .. byte`` () =
    <@ seq { 1uy..2uy..255uy } @> |> check (seq { 1uy..2uy..255uy })
    <@ [ 1uy..2uy..255uy ] @> |> check [ 1uy..2uy..255uy ]