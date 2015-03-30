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
