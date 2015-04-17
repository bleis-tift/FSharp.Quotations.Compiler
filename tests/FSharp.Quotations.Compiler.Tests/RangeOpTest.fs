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
module RangeOpTest =
  [<Test>]
  let ``byte .. byte`` () =
    <@ seq { 1uy..255uy } @> |> check (seq { 1uy..255uy })
    <@ [ 1uy..255uy ] @> |> check [ 1uy..255uy ]

  [<Test>]
  let ``byte .. byte .. byte`` () =
    <@ seq { 1uy..2uy..255uy } @> |> check (seq { 1uy..2uy..255uy })
    <@ [ 1uy..2uy..255uy ] @> |> check [ 1uy..2uy..255uy ]