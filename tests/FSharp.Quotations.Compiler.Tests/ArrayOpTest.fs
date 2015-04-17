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
module ArrayOpTest =
  [<Test>]
  let ``get elem from int[]`` () =
    <@ let xs = [|0..10|] in xs.[5] @> |> check 5
    <@ let xs = [|0..10|] in xs.[100] @> |> checkExn<_, IndexOutOfRangeException>

  [<Test>]
  let ``set elem to int[]`` () =
    <@ let xs = Array.zeroCreate 10
       xs.[5] <- 10
       xs.[5] @>
    |> check 10

  [<Test>]
  let ``slice int[]`` () =
    <@ let xs = [|0..10|] in xs.[2..8] @> |> check [|0..10|].[2..8]