(*
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

  [<Test>]
  let ``application`` () =
    <@
       let f (x: obj) =
         match x with
         | :? string -> "str"
         | :? int -> "int"
         | _ -> "other"
       (f ("hoge" :> obj)) + (f (20 :> obj))
    @>
    |> check "strint"

  [<Test>]
  let ``application 2`` () =
    <@
       let f a b = a + b
       f 10 20
    @>
    |> check 30

  [<Test>]
  let ``nested application`` () =
    <@
       let f x = x + 10
       (fun x -> f x) 20
    @>
    |> check 30

  [<Test>]
  let ``application try-with`` () =
    let expr = <@ let const10 = fun (_: int) -> 10 in const10 (try 1 with _ -> 0) @>
    expr |> check 10

  [<Test>]
  let ``application try-finally`` () =
    let expr = <@ let const10 = fun (_: int) -> 10 in const10 (try 1 finally ()) @>
    expr |> check 10
