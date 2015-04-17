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

open System

[<TestModule>]
module StringFuncTest =
  [<Test>]
  let ``String.Empty`` () = <@ String.Empty @> |> check ""

  [<Test>]
  let ``String.length`` () =
    <@ String.length "str" @> |> check 3
    <@ String.length null @> |> check 0

  [<Test>]
  let ``String.Length`` () =
    <@ "str".Length @> |> check 3
    <@ (null:string).Length @> |> checkExnType typeof<NullReferenceException>

  [<Test>]
  let ``String.IndexOf`` () =
    <@ "str".IndexOf("r") @> |> check 2
    <@ (null:string).IndexOf("r") @> |> checkExnType typeof<NullReferenceException>

  [<Test>]
  let ``String.map`` () =
    <@ String.map (fun ch -> char (int ch + 1)) "abc" @>
    |> check (String.map (fun ch -> char (int ch + 1)) "abc")

  [<Test>]
  let ``String.filter`` () =
    <@ String.forall (fun ch -> Char.IsLetter(ch)) "abc" @> |> check true

  [<Test>]
  let ``nested case`` () =
    <@ String.forall (fun ch -> Char.IsLetter(ch)) (String.map (fun ch -> char (int ch + 1)) "abc") @> |> check true