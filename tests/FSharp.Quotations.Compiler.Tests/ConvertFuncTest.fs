namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module ConvertFuncTest =
  [<Test>]
  let ``byte int`` () =
    <@ byte 1 @> |> check 1uy
    // Byte.MaxValue = 255uy
    <@ byte 256 @> |> check 0uy
    <@ byte 257 @> |> check 1uy
    // Byte.MinValue = 0uy
    <@ byte -1 @> |> check 255uy
    <@ byte -2 @> |> check 254uy