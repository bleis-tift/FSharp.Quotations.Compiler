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
    <@ unativeint -1 @> |> check (unativeint -1)

  [<Test>]
  let ``string int`` () = <@ string 42 @> |> check "42"

  [<Test>]
  let ``string bool`` () = <@ string true @> |> check "True"

  [<Test>]
  let ``byte string`` () =
    <@ byte "1" @> |> check 1uy
    // Byte.MaxValue = 255uy
    <@ byte "256" @> |> checkExn<_, OverflowException>
    // Byte.MinValue = 0uy
    <@ byte "-1" @> |> checkExn<_, OverflowException>

    <@ byte "str" @> |> checkExn<_, FormatException>
    <@ byte (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``sbyte string`` () =
    <@ sbyte "1" @> |> check 1y
    // SByte.MaxValue = 127y
    <@ sbyte "128" @> |> checkExn<_, OverflowException>
    // SByte.MinValue = -128y
    <@ sbyte "-129" @> |> checkExn<_, OverflowException>

    <@ sbyte "str" @> |> checkExn<_, FormatException>
    <@ sbyte (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``char string`` () =
    <@ char "a" @> |> check 'a'
    <@ char "aa" @> |> checkExn<_, FormatException>
    <@ char (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``decimal string`` () =
    <@ decimal "1" @> |> check 1M
    // Decimal.MaxValue = 79228162514264337593543950335M
    <@ decimal "79228162514264337593543950336" @> |> checkExn<_, OverflowException>
    // Decimal.MinValue = -79228162514264337593543950335M
    <@ decimal "-79228162514264337593543950336" @> |> checkExn<_, OverflowException>

    <@ decimal "str" @> |> checkExn<_, FormatException>
    <@ decimal (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``float string`` () =
    <@ float "1" @> |> check 1.0
    <@ float "NaN" @> |> check nan
    <@ float "Infinity" @> |> check infinity
    <@ float "-Infinity" @> |> check -infinity
    // Double.MaxValue < 1.797693135e+308
    <@ float "1.797693135e+308" @> |> checkExn<_, OverflowException>
    // Double.MinValue > -1.797693135e+308
    <@ float "-1.797693135e+308" @> |> checkExn<_, OverflowException>

    <@ float "str" @> |> checkExn<_, FormatException>
    <@ float (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``float32 string`` () =
    <@ float32 "1" @> |> check 1.0f
    <@ float32 "NaN" @> |> check nanf
    <@ float32 "Infinity" @> |> check infinityf
    <@ float32 "-Infinity" @> |> check -infinityf
    // Single.MaxValue < 3.40282347e+39
    <@ float32 "3.40282347e+39" @> |> checkExn<_, OverflowException>
    // Single.MinValue > -3.40282347e+39
    <@ float32 "-3.40282347e+39" @> |> checkExn<_, OverflowException>

    <@ float32 "str" @> |> checkExn<_, FormatException>
    <@ float32 (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``int string`` () =
    <@ int "1" @> |> check 1
    // Int32.MaxValue = 2147483647
    <@ int "2147483648" @> |> checkExn<_, OverflowException>
    // Int32.MinValue = -2147483648
    <@ int "-2147483649" @> |> checkExn<_, OverflowException>

    <@ int "str" @> |> checkExn<_, FormatException>
    <@ int (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``int16 string`` () =
    <@ int16 "1" @> |> check 1s
    // Int16.MaxValue = 32767s
    <@ int16 "32768" @> |> checkExn<_, OverflowException>
    // Int16.MinValue = -32768s
    <@ int16 "-32769" @> |> checkExn<_, OverflowException>

    <@ int16 "str" @> |> checkExn<_, FormatException>
    <@ int16 (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``uint16 string`` () =
    <@ uint16 "1" @> |> check 1us
    // UInt16.MaxValue = 65535us
    <@ uint16 "65536" @> |> checkExn<_, OverflowException>
    // UInt16.MinValue = 0us
    <@ uint16 "-1" @> |> checkExn<_, OverflowException>

    <@ uint16 "str" @> |> checkExn<_, FormatException>
    <@ uint16 (null: string) @> |> checkExn<_, ArgumentNullException>

  [<Test>]
  let ``int32 string`` () =
    <@ int32 "1" @> |> check 1
    // Int32.MaxValue = 2147483647
    <@ int32 "2147483648" @> |> checkExn<_, OverflowException>
    // Int32.MinValue = -2147483648
    <@ int32 "-2147483649" @> |> checkExn<_, OverflowException>

    <@ int32 "str" @> |> checkExn<_, FormatException>
    <@ int32 (null: string) @> |> checkExn<_, ArgumentNullException>

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

    [<Test>]
    let ``int64 int`` () = <@ int64 1 @> |> check 1L

    [<Test>]
    let ``uint64 int`` () =
      <@ uint64 1 @> |> check 1UL
      // UInt64.MinValue = 0UL
      <@ uint64 -1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``nativeint int`` () = <@ nativeint 0 @> |> check 0n

    [<Test>]
    let ``unativeint int`` () =
      <@ unativeint 0 @> |> check 0un
      <@ unativeint -1 @> |> checkExn<_, OverflowException>
