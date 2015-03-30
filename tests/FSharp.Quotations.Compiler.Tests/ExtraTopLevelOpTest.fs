namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ExtraTopLevelOpTest =

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

