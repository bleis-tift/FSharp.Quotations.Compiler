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

  [<Test>]
  let ``sbyte int`` () =
    <@ sbyte 1 @> |> check 1y
    // SByte.MaxValue = 127y
    <@ sbyte 128 @> |> check -128y
    <@ sbyte 129 @> |> check -127y
    // SByte.MinValue = -128y
    <@ sbyte -129 @> |> check 127y
    <@ sbyte -130 @> |> check 126y