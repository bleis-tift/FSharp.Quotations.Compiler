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
module ExtraTopLevelOpTest =
  open System.Collections.Generic

  [<Test>]
  let ``dict`` () =
    <@ dict []: IDictionary<int, string> @> |> check (dict [])
    <@ dict [(1, "one"); (2, "two")] @> |> check (dict [(1, "one"); (2, "two")])

  [<Test>]
  let ``eprintf`` () =
    <@ eprintf "hoge" @> |> checkPrinted Error "hoge"
    <@ eprintf "hoge%d" 1 @> |> checkPrinted Error "hoge1"
    <@ eprintf "%s" "hoge" @> |> checkPrinted Error "hoge"

  [<Test>]
  let ``eprintfn`` () =
    <@ eprintfn "hoge" @> |> checkPrinted Error "hoge\r\n"
    <@ eprintfn "hoge%d" 1 @> |> checkPrinted Error "hoge1\r\n"
    <@ eprintfn "%s" "hoge" @> |> checkPrinted Error "hoge\r\n"
    
  [<Test>]
  let ``fprintf`` () =
    <@ let w = new RobWriter(System.Text.UTF8Encoding()) in fprintf w "hoge"; w.Result @> |> check "hoge"
    <@ let w = new RobWriter(System.Text.UTF8Encoding()) in fprintf w "hoge%d" 1; w.Result @> |> check "hoge1"
    <@ let w = new RobWriter(System.Text.UTF8Encoding()) in fprintf w "%s" "hoge"; w.Result @> |> check "hoge"
    
  [<Test>]
  let ``fprintfn`` () =
    <@ let w = new RobWriter(System.Text.UTF8Encoding()) in fprintfn w "hoge"; w.Result @> |> check "hoge\r\n"
    <@ let w = new RobWriter(System.Text.UTF8Encoding()) in fprintfn w "hoge%d" 1; w.Result @> |> check "hoge1\r\n"
    <@ let w = new RobWriter(System.Text.UTF8Encoding()) in fprintfn w "%s" "hoge"; w.Result @> |> check "hoge\r\n"

  [<Test>]
  let ``printf`` () =
    <@ printf "hoge" @> |> checkPrinted Out "hoge"
    <@ printf "hoge%d" 1 @> |> checkPrinted Out "hoge1"
    <@ printf "%s" "hoge" @> |> checkPrinted Out "hoge"

  [<Test>]
  let ``printfn`` () =
    <@ printfn "hoge" @> |> checkPrinted Out "hoge\r\n"
    <@ printfn "hoge%d" 1 @> |> checkPrinted Out "hoge1\r\n"
    <@ printfn "%s" "hoge" @> |> checkPrinted Out "hoge\r\n"

  [<Test>]
  let ``failwith`` () = <@ failwith "hoge" @> |> checkExnMsg "hoge"

  [<Test>]
  let ``failwithf`` () =
    <@ failwithf "hoge" @> |> checkExnMsg "hoge"
    <@ failwithf "hoge%d" 1 @> |> checkExnMsg "hoge1"
    <@ failwithf "%s" "hoge" @> |> checkExnMsg "hoge"

  [<Test>]
  let ``sprintf`` () =
    <@ sprintf "hoge" @> |> check "hoge"
    <@ sprintf "hoge%d" 1 @> |> check "hoge1"
    <@ sprintf "%s" "hoge" @> |> check "hoge"
