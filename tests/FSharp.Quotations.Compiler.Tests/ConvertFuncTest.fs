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

  [<Test>]
  let ``char int`` () =
    <@ char 97 @> |> check 'a'
    // Char.MaxValue = char 65535
    <@ char 65536 @> |> check '\000'
    <@ char 65537 @> |> check '\001'
    // Char.MinValue = '\000'
    <@ char -1 @> |> check (Char.MaxValue)
    <@ char -2 @> |> check (char ((int Char.MaxValue) - 1))

  [<Test>]
  let ``decimal int`` () = <@ decimal 1 @> |> check 1M

  [<Test>]
  let ``enum int`` () =
    <@ enum 0 @> |> check StringSplitOptions.None
    <@ enum 1 @> |> check StringSplitOptions.RemoveEmptyEntries
    <@ enum 2 @> |> check (2 |> unbox<StringSplitOptions>)
    <@ enum -1 @> |> check (-1 |> unbox<StringSplitOptions>)

  [<Test>]
  let ``float int`` () = <@ float 1 @> |> check 1.0

  [<Test>]
  let ``float32 int`` () = <@ float32 1 @> |> check 1.0f