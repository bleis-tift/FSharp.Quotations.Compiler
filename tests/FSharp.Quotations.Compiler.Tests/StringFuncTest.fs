namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module StringFuncTest =
  [<Test>]
  let ``String.length`` () =
    <@ String.length "str" @> |> check 3
    <@ String.length null @> |> check 0

  [<Test>]
  let ``String.Length`` () =
    <@ "str".Length @> |> check 3
    <@ (null:string).Length @> |> checkExnType typeof<NullReferenceException>