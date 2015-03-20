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

  [<Test>]
  let ``int int`` () = <@ int 1 @> |> check 1

  [<Test>]
  let ``int16 int`` () =
    <@ int16 1 @> |> check 1s
    // Int16.MaxValue = 32767s
    <@ int16 32768 @> |> check -32768s
    <@ int16 32769 @> |> check -32767s
    // Int16.MinValue = -32768s
    <@ int16 -32769 @> |> check 32767s
    <@ int16 -32770 @> |> check 32766s

  [<Test>]
  let ``uint16 int`` () =
    <@ uint16 1 @> |> check 1us
    // UInt16.MaxValue = 65535us
    <@ uint16 65536 @> |> check 0us
    <@ uint16 65537 @> |> check 1us
    // UInt16.MinValue = 0us
    <@ uint16 -1 @> |> check 65535us
    <@ uint16 -2 @> |> check 65534us

  [<Test>]
  let ``int32 int`` () = <@ int32 1 @> |> check 1

  [<Test>]
  let ``uint32 int`` () =
    <@ uint32 1 @> |> check 1u
    // UInt32.MinValue = 0u
    <@ uint32 -1 @> |> check UInt32.MaxValue
    <@ uint32 -2 @> |> check (UInt32.MaxValue - 1u)

  [<Test>]
  let ``int64 int`` () = <@ int64 1 @> |> check 1L

  [<Test>]
  let ``uint64 int`` () =
    <@ uint64 1 @> |> check 1UL
    // UInt64.MinValue = 0UL
    <@ uint64 -1 @> |> check UInt64.MaxValue
    <@ uint64 -2 @> |> check (UInt64.MaxValue - 1UL)

  [<Test>]
  let ``nativeint int`` () = <@ nativeint 0 @> |> check 0n

  [<Test>]
  let ``unativeint int`` () =
    <@ unativeint 0 @> |> check 0un
    <@ unativeint 1 @> |> check (unativeint 1)

  [<Test>]
  let ``string int`` () = <@ string 42 @> |> check "42"

  module Checked =
    open Microsoft.FSharp.Core.Operators.Checked

    [<Test>]
    let ``byte int`` () =
      <@ byte 1 @> |> check 1uy
      // Byte.MaxValue = 255uy
      <@ byte 256 @> |> checkExn<_, OverflowException>
      // Byte.MinValue = 0uy
      <@ byte -1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``sbyte int`` () =
      <@ sbyte 1 @> |> check 1y
      // SByte.MaxValue = 127y
      <@ sbyte 128 @> |> checkExn<_, OverflowException>
      // SByte.MinValue = -128y
      <@ sbyte -129 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``char int`` () =
      <@ char 97 @> |> check 'a'
      // Char.MaxValue = char 65535
      <@ char 65536 @> |> checkExn<_, OverflowException>
      // Char.MinValue = '\000'
      <@ char -1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``int int`` () = <@ int 1 @> |> check 1

    [<Test>]
    let ``int16 int`` () =
      <@ int16 1 @> |> check 1s
      // Int16.MaxValue = 32767s
      <@ int16 32768 @> |> checkExn<_, OverflowException>
      // Int16.MinValue = -32768s
      <@ int16 -32769 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``uint16 int`` () =
      <@ uint16 1 @> |> check 1us
      // UInt16.MaxValue = 65535us
      <@ uint16 65536 @> |> checkExn<_, OverflowException>
      // UInt16.MinValue = 0us
      <@ uint16 -1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``int32 int`` () = <@ int32 1 @> |> check 1

    [<Test>]
    let ``uint32 int`` () =
      <@ uint32 1 @> |> check 1u
      // UInt32.MinValue = 0u
      <@ uint32 -1 @> |> checkExn<_, OverflowException>
