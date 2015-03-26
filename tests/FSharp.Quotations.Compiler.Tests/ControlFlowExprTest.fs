namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module ControlFlowExprTest =
  [<Test>]
  let ``sequential`` () =
    <@ (); 10 @> |> check 10
    <@ ignore 10; 20 @> |> check 20
    <@ ignore 10; ignore 20; 30 @> |> check 30

  [<Test>]
  let ``if-then-else`` () =
    <@ if true then "T" else "F" @> |> check "T"
    <@ if false then "T" else "F" @> |> check "F"

  [<Test>]
  let ``if-then-else (unit)`` () =
    <@ let mutable n = 0 
       if false then ()
       else n <- 10
       n @>
    |> check 10

  [<Test>]
  let ``if-then without else`` () =
    <@ let mutable n = 0 
       if true then n <- 10
       n @>
    |> check 10

  [<Test>]
  let ``try-with`` () =
    <@ try 10 with _e -> -1 @> |> check 10
    <@ try failwith "" with _e -> -1 @> |> check -1

  [<Test>]
  let ``try-with raise exception`` () =
    <@ try failwith "oops!" with e -> e.Message @>
    |> check "oops!"

  [<Test>]
  let ``try-with filter`` () =
    <@ try failwith "x" with e when e.Message = "x" -> "ok" | _ -> "ng" @>
    |> check "ok"
    <@ try failwith "y" with e when e.Message = "x" -> "ok" | _ -> "ng" @>
    |> check "ng"

  [<Test>]
  let ``try-with type test`` () =
    <@
       let f exn =
         try raise exn
         with
         | :? InvalidOperationException -> "invalid op"
         | :? ArgumentException -> " arg exn"
         | _ -> " other exn"
       f (InvalidOperationException()) + f (ArgumentException()) + f (Exception())
    @>
    |> check "invalid op arg exn other exn"

  [<Test>]
  let ``try-finally`` () =
    <@ try 10 finally () @> |> check 10

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

  type RecDU = Tag1 | Tag2 of RecDU

  [<Test>]
  let ``match recursive DU`` () =
    <@
       let f x =
         match x with
         | Tag1 -> "tag1"
         | Tag2 _ -> "tag2"
       f Tag1 + f (Tag2 Tag1)
    @>
    |> check "tag1tag2"

  [<Test>]
  let ``match list`` () =
    <@
       let f lst =
         match lst with
         | x::xs -> string x + ":" + (string xs.Length)
         | [] -> "empty"
       f [] + f [1; 2; 3]
    @>
    |> check "empty1:2"