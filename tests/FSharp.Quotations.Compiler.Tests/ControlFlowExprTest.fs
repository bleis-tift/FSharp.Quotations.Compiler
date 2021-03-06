﻿(*
 * FSharp.Quotations.Compiler - a compiler for F# expression tree
 * Written in 2015 by bleis-tift (hey_c_est_la_vie@hotmail.co.jp)
 * kyonmm, zakky-dev
 * 
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain worldwide.
 * This software is distributed without any warranty.
 * 
 * You should have received a copy of the CC0 Public Domain Dedication along with this software.
 * If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
 *)
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
  let ``if-then-else with tailcall`` () =
    <@ let cond () = true
       let f () = 42
       let g () = 12
       if cond () then f () else g () @>
    |> check 42

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
    <@ let x = ref 0
       try () finally x := 10
       !x @>
    |> check 10

  let run f = try f () with e -> e.GetType().Name

  [<Test>]
  let ``try with in let body`` () =
    <@
      let v = 10
      try
        run (fun () -> "1")
      with
        _ -> ""
    @>
    |> check "1"

  [<Test>]
  let ``rethrow`` () =
    <@ try
         try failwith "oops!"
         with _ -> reraise ()
       with e -> e.Message @>
    |> check "oops!"

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

  let (|Number|_|) (str: string) =
    match Int32.TryParse(str) with
    | true, res -> Some res
    | _ -> None

  let (|EndsWith|_|) (suffix: string) (str: string) =
    if str.EndsWith(suffix) then Some suffix
    else None

  [<Test>]
  let ``match with and pattern`` () =
    <@
       let f x =
         match x with
         | Number num & EndsWith("0") suffix -> (num, suffix)
         | _ -> (0, "")
       (f "100")
    @>
    |> check (100, "0")

  [<Test>]
  let ``match with and pattern2`` () =
    <@
       let f x =
         match x with
         | EndsWith("00") suffix1 & EndsWith("0") suffix2 ->
             suffix1 + ", " + suffix2
         | _ -> ""
       (f "100")
    @>
    |> check "00, 0"

  [<Test>]
  let ``while loop`` () =
    <@ let res = ResizeArray<_>()
       let mutable i = 0
       while i <> 10 do
         res.Add(i)
         i <- i + 1
       res.ToArray() @>
    |> check [|0..9|]

  [<Test>]
  let ``for loop`` () =
    <@ let res = ResizeArray<_>()
       for x in 5..10 do 
         res.Add(x)
       res.ToArray() @>
    |> check [|5..10|]

  [<Test>]
  let ``for loop that is compiled into while loop`` () =
    <@ let res = ResizeArray<_>()
       for x in [5..10] do
         res.Add(x)
       res.ToArray() @>
    |> check [|5..10|]