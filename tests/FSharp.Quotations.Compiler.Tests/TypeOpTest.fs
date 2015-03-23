namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module TypeOpTest =
  [<Test>]
  let ``int :> obj`` () = <@ 42 :> obj @> |> check (42 :> obj)

  [<Test>]
  let ``string :> obj`` () = <@ "hoge" :> obj @> |> check ("hoge" :> obj)