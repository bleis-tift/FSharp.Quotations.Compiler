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
    @>
    |> check "strint"

  [<Test>]
  let ``match option`` () =
    <@
       let f x =
         match x with
         | Some n -> string n
         | None -> "none"
       f (Some 42) + f None
    @>
    |> check "42none"

  type SimpleDU = Tag1 | Tag2

  [<Test>]
  let ``match simple DU`` () =
    <@
       let f x =
         match x with
         | Tag1 -> "1"
         | Tag2 -> "2"
       f Tag1 + f Tag2
    @>
    |> check "12"

  type SimpleDUWithValue = Tag1 of int | Tag2

  [<Test>]
  let ``match DU with value`` () =
    <@
       let f x =
         match x with
         | Tag1 i -> string i
         | Tag2 -> "tag2"
       f (Tag1 0) + f Tag2
    @>
    |> check "0tag2"