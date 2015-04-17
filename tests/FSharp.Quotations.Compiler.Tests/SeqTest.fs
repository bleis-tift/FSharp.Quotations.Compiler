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

module SeqTest =
  [<Test>]
  let ``int seq`` () =
    <@ seq [1..10] @> |> check (seq [1..10])
    <@ seq { 1..10 } @> |> check (seq { 1..10 })

  [<Test>]
  let ``empty`` () =
    <@ Seq.empty : int seq @> |> check Seq.empty
    <@ Seq.empty : string seq @> |> check Seq.empty

  [<Test>]
  let ``sum`` () =
    <@ Seq.sum [1..10] @> |> check (Seq.sum [1..10])
    <@ Seq.sum (seq [1..10]) @> |> check (Seq.sum (seq [1..10]))
    <@ Seq.sum (seq { 1..10 }) @> |> check (Seq.sum (seq { 1..10 }))

  [<Test>]
  let ``sumBy`` () =
    <@ Seq.sumBy int ["1"; "2"] @> |> check 3
    <@ Seq.sumBy int (seq ["1"; "2"]) @> |> check 3
    <@ Seq.sumBy int (seq { yield "1"; yield "2" }) @> |> check 3

  [<Test>]
  let ``filter`` () =
    <@ [1..10] |> Seq.filter (fun x -> x < 3) @> |> check (seq [1; 2])
    <@ [1..10] |> Seq.filter ((=)5) @> |> check (seq [5])