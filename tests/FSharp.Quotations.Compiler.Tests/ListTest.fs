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

#nowarn "1104"

[<TestModule>]
module ListTest =
  [<Test>]
  let ``int list @ int list`` () =
    <@ [1; 2; 3] @ [4; 5; 6] @> |> check [1..6]
    <@ [1; 2; 3] @ [] @> |> check [1..3]
    <@ [] @ [4; 5; 6] @> |> check [4..6]

  [<Test>]
  let ``string list @ string list`` () =
    <@ ["a"; "b"] @ ["c"; "d"] @> |> check ["a"; "b"; "c"; "d"]
    <@ ["a"; "b"] @ [] @> |> check ["a"; "b"]
    <@ [] @ ["c"; "d"] @> |> check ["c"; "d"]

  [<Test>]
  let ``cons`` () =
    <@ 1::2::3::[] @> |> check [1..3]
    <@ "a"::"b"::"c"::[] @> |> check ["a"; "b"; "c"]

  [<Test>]
  let ``cons match`` () =
    <@ match [1..3] with
       | x::xs -> (x * 10) + (xs.Length)
       | [] -> -1 @>
    |> check 12

  [<Test>]
  let ``empty`` () =
    <@ List.empty : int list @> |> check []
    <@ List.empty : string list @> |> check []

  [<Test>]
  let ``sum`` () = <@ List.sum [1..10] @> |> check (List.sum [1..10])

  [<Test>]
  let ``sumBy`` () = <@ List.sumBy int ["1"; "2"] @> |> check 3

  [<Test>]
  let ``filter`` () =
    <@ [1..10] |> List.filter (fun x -> x < 3) @> |> check [1; 2]
    <@ [1..10] |> List.filter ((=)5) @> |> check [5]