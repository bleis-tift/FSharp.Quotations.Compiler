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