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
