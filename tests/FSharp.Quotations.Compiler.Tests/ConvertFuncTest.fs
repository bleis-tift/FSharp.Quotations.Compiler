﻿namespace FSharp.Quotations.Compiler.Tests

open System

[<TestModule>]
module ConvertFuncTest =
  type TestInfo<'TInput, 'TExpected> = {
    Data: 'TInput list
    ExprFun: 'TInput -> Microsoft.FSharp.Quotations.Expr<'TExpected>
    Fun: 'TInput -> 'TExpected
  }

  type Expected<'T> =
    | ReturnVal of 'T
    | ThrownExn of Type

  let test { Data = data; ExprFun = f1; Fun = f2 } =
    if data.IsEmpty then
      failwith "test data is empty."
    for tc in data do
      let expected =
        try ReturnVal (f2 tc)
        with e -> ThrownExn (e.GetType())
      match expected with
      | ReturnVal e -> f1 tc |> check e
      | ThrownExn t -> f1 tc |> checkExnType t

  let inline testByteFrom< ^T when ^T : (static member op_Explicit: ^T -> byte) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ byte x @>); Fun = byte }

  [<Test>]
  let ``byte int`` () =
    let max, min = int Byte.MaxValue, int Byte.MinValue
    testByteFrom<int> [1; max + 1; min - 1]

  [<Test>]
  let ``byte char`` () = testByteFrom ['a'; char (Byte.MaxValue + byte 1); char (Byte.MinValue - byte 1)]

  [<Test>]
  let ``byte float`` () =
    let max, min = float Byte.MaxValue, float Byte.MinValue
    testByteFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``byte string`` () = testByteFrom<string> ["1"; "256"; "-1"; "str"; null]

  let inline testSByteFrom< ^T when ^T : (static member op_Explicit: ^T -> sbyte) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ sbyte x @>); Fun = sbyte }

  [<Test>]
  let ``sbyte int`` () =
    let max, min = int SByte.MaxValue, int SByte.MinValue
    testSByteFrom<int> [1; max + 1; min - 1]

  [<Test>]
  let ``sbyte char`` () = testSByteFrom ['a'; char (SByte.MaxValue + sbyte 1); char (SByte.MinValue - sbyte 1)]

  [<Test>]
  let ``sbyte float`` () =
    let max, min = float Byte.MaxValue, float Byte.MinValue
    testSByteFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``sbyte string`` () = testSByteFrom<string> ["1"; "128"; "-129"; "str"; null]

  let inline testCharFrom< ^T when ^T : (static member op_Explicit: ^T -> char) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ char x @>); Fun = char }

  [<Test>]
  let ``char int`` () =
    let max, min = int Char.MaxValue, int Char.MinValue
    testCharFrom<int> [97; max + 1; min - 1]

  [<Test>]
  let ``char char`` () = testCharFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``char float`` () =
    let max, min = float Char.MaxValue, float Char.MinValue
    testCharFrom<float> [97.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``char string`` () = testCharFrom<string> ["a"; "aa"; null]

  let inline testDecimalFrom< ^T when ^T : (static member op_Explicit: ^T -> decimal) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ decimal x @>); Fun = decimal }

  [<Test>]
  let ``decimal int`` () = testDecimalFrom<int> [1]

  [<Test>]
  let ``decimal float`` () = testDecimalFrom<float> [1.0; Double.MaxValue; Double.MinValue; nan; infinity; -infinity]

  [<Test>]
  let ``decimal string`` () =
    testDecimalFrom<string> [
      "1"
      "79228162514264337593543950336" 
      "-79228162514264337593543950336" 
      "str"; null
    ]

  let inline testFloatFrom< ^T when ^T : (static member op_Explicit: ^T -> float) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ float x @>); Fun = float }

  [<Test>]
  let ``float int`` () = testFloatFrom<int> [1]

  [<Test>]
  let ``float char`` () = testFloatFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``float float`` () = testFloatFrom<float> [1.0; nan; infinity; -infinity]

  [<Test>]
  let ``float string`` () =
    testFloatFrom<string> [
      "1"; "NaN"; "Infinity"; "-Infinity";
      "1.797693135e+308"
      "-1.797693135e+308" 
      "str"; null
    ]

  let inline testFloat32From< ^T when ^T : (static member op_Explicit: ^T -> float32) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ float32 x @>); Fun = float32 }

  [<Test>]
  let ``float32 int`` () = testFloat32From<int> [1]

  [<Test>]
  let ``float32 char`` () = testFloat32From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``float32 float`` () = testFloat32From<float> [0.0; nan; infinity; -infinity]

  [<Test>]
  let ``float32 string`` () =
    testFloat32From<string> [
      "1"; "NaN"; "Infinity"; "-Infinity";
      "3.40282347e+39"
      "-3.40282347e+39" 
      "str"; null
    ]

  let inline testIntFrom< ^T when ^T : (static member op_Explicit: ^T -> int) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int x @>); Fun = int }

  [<Test>]
  let ``int int`` () = testIntFrom<int> [1]

  [<Test>]
  let ``int char`` () = testIntFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``int float`` () =
    let max, min = float Int32.MaxValue, float Int32.MinValue
    testIntFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``int string`` () = testIntFrom<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

  let inline testInt16From< ^T when ^T : (static member op_Explicit: ^T -> int16) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int16 x @>); Fun = int16 }

  [<Test>]
  let ``int16 int`` () =
    let max, min = int Int16.MaxValue, int Int16.MinValue
    testInt16From<int> [1; max + 1; min - 1]

  [<Test>]
  let ``int16 char`` () = testInt16From ['a'; char (Int16.MaxValue + int16 1); Char.MinValue]

  [<Test>]
  let ``int16 float`` () =
    let max, min = float Int16.MaxValue, float Int16.MinValue
    testInt16From<float> [1.0; max + 1.0; min - 1.0]

  [<Test>]
  let ``int16 string`` () = testInt16From<string> ["1"; "32768"; "-32769"; "str"; null]

  let inline testUInt16From< ^T when ^T : (static member op_Explicit: ^T -> uint16) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint16 x @>); Fun = uint16 }

  [<Test>]
  let ``uint16 int`` () =
    let max, min = int UInt16.MaxValue, int UInt16.MinValue
    testUInt16From<int> [1; max + 1; min - 1]

  [<Test>]
  let ``uint16 char`` () = testUInt16From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``uint16 float`` () =
    let max, min = float UInt16.MaxValue, float UInt16.MinValue
    testUInt16From<float> [1.0; max + 1.0; min - 1.0]

  [<Test>]
  let ``uint16 string`` () = testUInt16From<string> ["1"; "65536"; "-1"; "str"; null]

  let inline testInt32From< ^T when ^T : (static member op_Explicit: ^T -> int32) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int32 x @>); Fun = int32 }

  [<Test>]
  let ``int32 int`` () = testInt32From<int> [1]

  [<Test>]
  let ``int32 char`` () = testInt32From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``int32 string`` () = testInt32From<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

  let inline testUInt32From< ^T when ^T : (static member op_Explicit: ^T -> uint32) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint32 x @>); Fun = uint32 }

  [<Test>]
  let ``uint32 int`` () =
    let min = int UInt32.MinValue
    testUInt32From<int> [1; min - 1]

  [<Test>]
  let ``uint32 char`` () = testUInt32From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``uint32 string`` () = testUInt32From<string> ["1"; "4294967296"; "-1"; "str"; null]

  let inline testInt64From< ^T when ^T : (static member op_Explicit: ^T -> int64) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int64 x @>); Fun = int64 }

  [<Test>]
  let ``int64 int`` () = testInt64From<int> [1]

  [<Test>]
  let ``int64 char`` () = testInt64From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``int64 string`` () =
    testInt64From<string> ["1"; "9223372036854775808"; "-9223372036854775809"; "str"; null]

  let inline testUInt64From< ^T when ^T : (static member op_Explicit: ^T -> uint64) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint64 x @>); Fun = uint64 }

  [<Test>]
  let ``uint64 int`` () =
    let min = int UInt64.MinValue
    testUInt64From<int> [1; min - 1]

  [<Test>]
  let ``uint64 char`` () = testUInt64From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``uint64 string`` () =
    testUInt64From<string> ["1"; "18446744073709551616"; "-18446744073709551617"; "str"; null]

  let inline testNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> nativeint) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ nativeint x @>); Fun = nativeint }

  [<Test>]
  let ``nativeint int`` () = <@ nativeint 0 @> |> check 0n

  [<Test>]
  let ``nativeint char`` () = testNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]

  let inline testUNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> unativeint) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ unativeint x @>); Fun = unativeint }

  [<Test>]
  let ``unativeint int`` () =
    <@ unativeint 0 @> |> check 0un
    <@ unativeint -1 @> |> check (unativeint -1)

  [<Test>]
  let ``unativeint char`` () = testUNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``enum int`` () =
    <@ enum 0 @> |> check StringSplitOptions.None
    <@ enum 1 @> |> check StringSplitOptions.RemoveEmptyEntries
    <@ enum 2 @> |> check (2 |> unbox<StringSplitOptions>)
    <@ enum -1 @> |> check (-1 |> unbox<StringSplitOptions>)

  [<Test>]
  let ``string int`` () = <@ string 42 @> |> check "42"

  [<Test>]
  let ``string char`` () = <@ string 'a' @> |> check "a"

  [<Test>]
  let ``string bool`` () = <@ string true @> |> check "True"

  [<Test>]
  let ``string string`` () =
    <@ string "str" @> |> check "str"
    <@ string (null: string) @> |> check ""

  [<Test>]
  let ``string int[]`` () =
    <@ string [|10|] @> |> check "System.Int32[]"
    <@ string [||] @> |> check "System.Object[]"
    <@ string (null: int[]) @> |> check ""

  type MyClass() =
    override __.ToString() = "hello"

  module Checked =
    open Microsoft.FSharp.Core.Operators.Checked

    let inline testByteFrom< ^T when ^T : (static member op_Explicit: ^T -> byte) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ byte x @>); Fun = byte }

    [<Test>]
    let ``byte int`` () =
      let max, min = int Byte.MaxValue, int Byte.MinValue
      testByteFrom<int> [1; max + 1; min - 1]

    [<Test>]
    let ``byte char`` () = testByteFrom ['a'; Char.MinValue; char Byte.MaxValue + char 1]

    [<Test>]
    let ``byte string`` () = testByteFrom<string> ["1"; "256"; "-1"; "str"; null]

    let inline testSByteFrom< ^T when ^T : (static member op_Explicit: ^T -> sbyte) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ sbyte x @>); Fun = sbyte }

    [<Test>]
    let ``sbyte int`` () =
      let max, min = int SByte.MaxValue, int SByte.MinValue
      testSByteFrom<int> [1; max + 1; min - 1]

    [<Test>]
    let ``sbyte char`` () = testSByteFrom ['a'; Char.MinValue; char SByte.MaxValue + char 1]

    [<Test>]
    let ``sbyte string`` () = testSByteFrom<string> ["1"; "128"; "-129"; "str"; null]

    let inline testCharFrom< ^T when ^T : (static member op_Explicit: ^T -> char) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ char x @>); Fun = char }

    [<Test>]
    let ``char int`` () =
      let max, min = int Char.MaxValue, int Char.MinValue
      testCharFrom<int> [97; max + 1; min - 1]

    [<Test>]
    let ``char char`` () = testCharFrom ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``char string`` () = testCharFrom<string> ["a"; "aa"; null]

    let inline testIntFrom< ^T when ^T : (static member op_Explicit: ^T -> int) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int x @>); Fun = int }

    [<Test>]
    let ``int int`` () = testIntFrom<int> [1]

    [<Test>]
    let ``int char`` () = testIntFrom ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``int string`` () = testIntFrom<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

    let inline testInt16From< ^T when ^T : (static member op_Explicit: ^T -> int16) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int16 x @>); Fun = int16 }

    [<Test>]
    let ``int16 int`` () =
      let max, min = int Int16.MaxValue, int Int16.MinValue
      testInt16From<int> [1; max + 1; min - 1]

    [<Test>]
    let ``int16 char`` () = testInt16From ['a'; Char.MinValue; char Int16.MaxValue + char 1]

    [<Test>]
    let ``int16 string`` () = testInt16From<string> ["1"; "32768"; "-32769"; "str"; null]

    let inline testUInt16From< ^T when ^T : (static member op_Explicit: ^T -> uint16) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint16 x @>); Fun = uint16 }

    [<Test>]
    let ``uint16 int`` () =
      let max, min = int UInt16.MaxValue, int UInt16.MinValue
      testUInt16From<int> [1; max + 1; min - 1]

    [<Test>]
    let ``uint16 char`` () = testUInt16From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``uint16 string`` () = testUInt16From<string> ["1"; "65536"; "-1"; "str"; null]

    let inline testInt32From< ^T when ^T : (static member op_Explicit: ^T -> int32) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int32 x @>); Fun = int32 }

    [<Test>]
    let ``int32 int`` () = testInt32From<int> [1]

    [<Test>]
    let ``int32 char`` () = testInt32From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``int32 string`` () = testInt32From<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

    let inline testUInt32From< ^T when ^T : (static member op_Explicit: ^T -> uint32) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint32 x @>); Fun = uint32 }

    [<Test>]
    let ``uint32 int`` () =
      let min = int UInt32.MinValue
      testUInt32From<int> [1; min - 1]

    [<Test>]
    let ``uint32 char`` () = testUInt32From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``uint32 string`` () = testUInt32From<string> ["1"; "4294967296"; "-1"; "str"; null]

    let inline testInt64From< ^T when ^T : (static member op_Explicit: ^T -> int64) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int64 x @>); Fun = int64 }

    [<Test>]
    let ``int64 int`` () = testInt64From<int> [1]

    [<Test>]
    let ``int64 char`` () = testInt64From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``int64 string`` () =
      testInt64From<string> ["1"; "9223372036854775808"; "-9223372036854775809"; "str"; null]

    let inline testUInt64From< ^T when ^T : (static member op_Explicit: ^T -> uint64) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint64 x @>); Fun = uint64 }

    [<Test>]
    let ``uint64 int`` () =
      let min = int UInt64.MinValue
      testUInt64From<int> [1; min - 1]

    [<Test>]
    let ``uint64 char`` () = testUInt64From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``uint64 string`` () =
      testUInt64From<string> ["1"; "18446744073709551616"; "-18446744073709551617"; "str"; null]

    let inline testNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> nativeint) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ nativeint x @>); Fun = nativeint }

    [<Test>]
    let ``nativeint int`` () = <@ nativeint 0 @> |> check 0n

    [<Test>]
    let ``nativeint char`` () = testNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]

    let inline testUNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> unativeint) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ unativeint x @>); Fun = unativeint }

    [<Test>]
    let ``unativeint int`` () =
      <@ unativeint 0 @> |> check 0un
      <@ unativeint -1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``unativeint char`` () = testUNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]
