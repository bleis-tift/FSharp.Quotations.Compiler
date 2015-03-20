namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module CmpOpTest =
  [<Test>]
  let ``int = int`` () =
    <@ 1 = 1 @> |> check true
    <@ 0 = 1 @> |> check false

  [<Test>]
  let ``int <> int`` () =
    <@ 1 <> 1 @> |> check false
    <@ 0 <> 1 @> |> check true

  [<Test>]
  let ``int < int`` () =
    <@ 1 < 0 @> |> check false
    <@ 1 < 1 @> |> check false
    <@ 0 < 1 @> |> check true

  [<Test>]
  let ``int <= int`` () =
    <@ 1 <= 0 @> |> check false
    <@ 1 <= 1 @> |> check true
    <@ 0 <= 1 @> |> check true

  [<Test>]
  let ``int > int`` () =
    <@ 1 > 0 @> |> check true
    <@ 1 > 1 @> |> check false
    <@ 0 > 1 @> |> check false

  [<Test>]
  let ``int >= int`` () =
    <@ 1 >= 0 @> |> check true
    <@ 1 >= 1 @> |> check true
    <@ 0 >= 1 @> |> check false

  [<Test>]
  let ``bool = bool`` () =
    <@ true = true @> |> check true
    <@ false = true @> |> check false

  [<Test>]
  let ``bool <> bool`` () =
    <@ true <> true @> |> check false
    <@ false <> true @> |> check true

  [<Test>]
  let ``bool < bool`` () =
    <@ true < false @> |> check false
    <@ true < true @> |> check false
    <@ false < true @> |> check true

  [<Test>]
  let ``bool <= bool`` () =
    <@ true <= false @> |> check false
    <@ true <= true @> |> check true
    <@ false <= true @> |> check true

  [<Test>]
  let ``bool > bool`` () =
    <@ true > false @> |> check true
    <@ true > true @> |> check false
    <@ false > true @> |> check false

  [<Test>]
  let ``bool >= bool`` () =
    <@ true >= false @> |> check true
    <@ true >= true @> |> check true
    <@ false >= true @> |> check false