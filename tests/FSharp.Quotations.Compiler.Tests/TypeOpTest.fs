namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module TypeOpTest =

  [<Test>]
  let ``int :> obj`` () = <@ 42 :> obj @> |> check (42 :> obj)

  [<Test>]
  let ``bigint :> obj`` () = <@ 1I :> obj @> |> check (1I :> obj)

  [<Test>]
  let `` char :> obj`` () = <@ 'c' :> obj @> |> check ('c' :> obj)

  [<Test>]
  let ``string :> obj`` () = <@ "hoge" :> obj @> |> check ("hoge" :> obj)

  [<Test>]
  let ``int :> IEquatable<int>`` () =
    <@ 42 :> IEquatable<int> @> |> check (42 :> IEquatable<int>)

  [<Test>]
  let ``bigint :> IEquatable<bigint>`` () =
    <@ 42I :> IEquatable<bigint> @> |> check (42I :> IEquatable<bigint>)

  [<Test>]
  let ``char :> IEquatable<char>`` () =
    <@ 'c' :> IEquatable<char> @> |> check ('c' :> IEquatable<char>)

  [<Test>]
  let ``string :> IEquatable<string>`` () =
    <@ "hoge" :> IEquatable<string> @> |> check ("hoge" :> IEquatable<string>)
