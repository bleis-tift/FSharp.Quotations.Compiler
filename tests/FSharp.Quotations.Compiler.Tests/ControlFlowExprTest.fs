namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ControlFlowExprTest =
  [<Test>]
  let ``try-with`` () =
    <@ try 10 with _e -> -1 @> |> check 10
    <@ try failwith "" with _e -> -1 @> |> check -1

  [<Test>]
  let ``try-finally`` () =
    <@ try 10 finally () @> |> check 10

  [<Test>]
  let ``try-with raise exception`` () =
    <@ try failwith "oops!" with e -> e.Message @>
    |> check "oops!"

  [<Test>]
  let ``sequential`` () =
    <@ (); 10 @> |> check 10
    <@ ignore 10; 20 @> |> check 20
    <@ ignore 10; ignore 20; 30 @> |> check 30

  [<Test>]
  let ``match type test`` () =
    <@
      let x =
        match "hoge" :> obj with
        | :? string -> "str"
        | :? int -> "int"
        | _ -> "other"
      let y =
        match 42 :> obj with
        | :? string -> "str"
        | :? int -> "int"
        | _ -> "other"
      x + y
    @> |> check "strint"