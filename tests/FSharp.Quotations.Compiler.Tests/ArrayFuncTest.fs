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
module ArrayFuncTest =
  [<Test>]
  let ``Array.empty`` () =
    <@ Array.empty: int[] @> |> check [||]
    <@ Array.empty: string[] @> |> check [||]

  [<Test>]
  let ``Array.init`` () =
    <@ Array.init 3 id @> |> check [|0; 1; 2|]
    <@ Array.init 3 string @> |> check [|"0"; "1"; "2"|]

  [<Test>]
  let ``Array.create`` () =
    <@ Array.create 3 1 @> |> check [|1; 1; 1|]
    <@ Array.create 3 "str" @> |> check [|"str"; "str"; "str"|]

  [<Test>]
  let ``Array.zeroCreate`` () =
    <@ Array.zeroCreate 3 @> |> check [|0; 0; 0|]
    <@ Array.zeroCreate 3 @> |> check [|null; null; null|]

  [<Test>]
  let ``Array.length`` () = <@ Array.length [|0; 1; 2|] @> |> check 3

  [<Test>]
  let ``Array.Length`` () = <@ [|0; 1; 2|].Length @> |> check 3

  [<Test>]
  let ``Array.Map`` () = <@ [|0..10|] |> Array.filter (fun x -> x % 5 = 0) @> |> check [|0; 5; 10|]