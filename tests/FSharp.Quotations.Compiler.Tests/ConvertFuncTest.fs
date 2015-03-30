namespace FSharp.Quotations.Compiler.Tests

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
  let ``byte bigint`` () =
    let max, min = Numerics.BigInteger(int Byte.MaxValue), Numerics.BigInteger(int Byte.MinValue)
    testByteFrom<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``byte char`` () = testByteFrom ['a'; char (Byte.MaxValue + byte 1); char (Byte.MinValue - byte 1)]

  [<Test>]
  let ``byte float`` () =
    let max, min = float Byte.MaxValue, float Byte.MinValue
    testByteFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``byte decimal`` () =
    let max, min = decimal Byte.MaxValue, decimal Byte.MinValue
    testByteFrom<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``byte string`` () = testByteFrom<string> ["1"; "256"; "-1"; "str"; null]

  let inline testUInt8From< ^T when ^T : (static member op_Explicit: ^T -> uint8) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint8 x @>); Fun = uint8 }

  [<Test>]
  let ``uint8 int`` () =
    let max, min = int Byte.MaxValue, int Byte.MinValue
    testUInt8From<int> [1; max + 1; min - 1]

  [<Test>]
  let ``uint8 bigint`` () =
    let max, min = Numerics.BigInteger(int Byte.MaxValue), Numerics.BigInteger(int Byte.MinValue)
    testUInt8From<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``uint8 char`` () = testUInt8From ['a']

  [<Test>]
  let ``uint8 float`` () =
    let max, min = float Byte.MaxValue, float Byte.MinValue
    testUInt8From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``uint8 decimal`` () =
    let max, min = decimal Byte.MaxValue, decimal Byte.MinValue
    testUInt8From<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``uint8 string`` () = testUInt8From<string> ["1"; "256"; "-1"; "str"; null]

  let inline testSByteFrom< ^T when ^T : (static member op_Explicit: ^T -> sbyte) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ sbyte x @>); Fun = sbyte }

  [<Test>]
  let ``sbyte int`` () =
    let max, min = int SByte.MaxValue, int SByte.MinValue
    testSByteFrom<int> [1; max + 1; min - 1]

  [<Test>]
  let ``sbyte bigint`` () =
    let max, min = Numerics.BigInteger(int SByte.MaxValue), Numerics.BigInteger(int SByte.MinValue)
    testSByteFrom<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``sbyte char`` () = testSByteFrom ['a'; char (SByte.MaxValue + sbyte 1); char (SByte.MinValue - sbyte 1)]

  [<Test>]
  let ``sbyte float`` () =
    let max, min = float Byte.MaxValue, float Byte.MinValue
    testSByteFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``sbyte decimal`` () =
    let max, min = decimal SByte.MaxValue, decimal SByte.MinValue
    testSByteFrom<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``sbyte string`` () = testSByteFrom<string> ["1"; "128"; "-129"; "str"; null]

  let inline testInt8From< ^T when ^T : (static member op_Explicit: ^T -> int8) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int8 x @>); Fun = int8 }

  [<Test>]
  let ``int8 int`` () =
    let max, min = int SByte.MaxValue, int SByte.MinValue
    testInt8From<int> [1; max + 1; min - 1]

  [<Test>]
  let ``int8 bigint`` () =
    let max, min = Numerics.BigInteger(int SByte.MaxValue), Numerics.BigInteger(int SByte.MinValue)
    testInt8From<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``int8 char`` () = testInt8From ['a']

  [<Test>]
  let ``int8 float`` () =
    let max, min = float Byte.MaxValue, float Byte.MinValue
    testInt8From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``int8 decimal`` () =
    let max, min = decimal SByte.MaxValue, decimal SByte.MinValue
    testInt8From<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``int8 string`` () = testInt8From<string> ["1"; "128"; "-129"; "str"; null]

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
  let ``char decimal`` () =
    let max, min = decimal (int Char.MaxValue), decimal (int Char.MinValue)
    testCharFrom<decimal> [97.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``char string`` () = testCharFrom<string> ["a"; "aa"; null]

  let inline testDecimalFrom< ^T when ^T : (static member op_Explicit: ^T -> decimal) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ decimal x @>); Fun = decimal }

  [<Test>]
  let ``decimal int`` () = testDecimalFrom<int> [1]

  [<Test>]
  let ``decimal bigint`` () = testDecimalFrom<bigint> [1I]

  [<Test>]
  let ``decimal float`` () = testDecimalFrom<float> [1.0; Double.MaxValue; Double.MinValue; nan; infinity; -infinity]

  [<Test>]
  let ``decimal decimal`` () = testDecimalFrom<decimal> [1.0M]

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
  let ``float bigint`` () = testFloatFrom<bigint> [1I]

  [<Test>]
  let ``float char`` () = testFloatFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``float float`` () = testFloatFrom<float> [1.0; nan; infinity; -infinity]

  [<Test>]
  let ``float decimal`` () = testFloatFrom<decimal> [1.0M; Decimal.MaxValue; Decimal.MinValue]

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
  let ``float32 bigitn`` () = testFloat32From<bigint> [1I]

  [<Test>]
  let ``float32 char`` () = testFloat32From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``float32 float`` () = testFloat32From<float> [0.0; nan; infinity; -infinity]

  [<Test>]
  let ``float32 decimal`` () = testFloat32From<decimal> [0.0M]

  [<Test>]
  let ``float32 string`` () =
    testFloat32From<string> [
      "1"; "NaN"; "Infinity"; "-Infinity";
      "3.40282347e+39"
      "-3.40282347e+39" 
      "str"; null
    ]

  let inline testDoubleFrom< ^T when ^T : (static member op_Explicit: ^T -> double) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ double x @>); Fun = double }

  [<Test>]
  let ``double int`` () = testDoubleFrom<int> [1]

  [<Test>]
  let ``double bigint`` () = testDoubleFrom<bigint> [1I]

  [<Test>]
  let ``double char`` () = testDoubleFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``double double`` () = testDoubleFrom<double> [1.0; nan; infinity; -infinity]

  [<Test>]
  let ``double decimal`` () = testDoubleFrom<decimal> [1.0M; Decimal.MaxValue; Decimal.MinValue]

  [<Test>]
  let ``double string`` () =
    testDoubleFrom<string> [
      "1"; "NaN"; "Infinity"; "-Infinity";
      "1.797693135e+308"
      "-1.797693135e+308" 
      "str"; null
    ]

  let inline testIntFrom< ^T when ^T : (static member op_Explicit: ^T -> int) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int x @>); Fun = int }

  [<Test>]
  let ``int int`` () = testIntFrom<int> [1]

  [<Test>]
  let ``int bigint`` () =
    let max, min = Numerics.BigInteger(Int32.MaxValue), Numerics.BigInteger(Int32.MinValue)
    testIntFrom<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``int char`` () = testIntFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``int float`` () =
    let max, min = float Int32.MaxValue, float Int32.MinValue
    testIntFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``int decimal`` () =
    let max, min = decimal Int32.MaxValue, decimal Int32.MinValue
    testIntFrom<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``int string`` () = testIntFrom<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

  let inline testInt16From< ^T when ^T : (static member op_Explicit: ^T -> int16) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int16 x @>); Fun = int16 }

  [<Test>]
  let ``int16 int`` () =
    let max, min = int Int16.MaxValue, int Int16.MinValue
    testInt16From<int> [1; max + 1; min - 1]

  [<Test>]
  let ``int16 bigint`` () =
    let max, min = Numerics.BigInteger(int Int16.MaxValue), Numerics.BigInteger(int Int16.MinValue)
    testInt16From<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``int16 char`` () = testInt16From ['a'; char (Int16.MaxValue + int16 1); Char.MinValue]

  [<Test>]
  let ``int16 float`` () =
    let max, min = float Int16.MaxValue, float Int16.MinValue
    testInt16From<float> [1.0; max + 1.0; min - 1.0]

  [<Test>]
  let ``int16 decimal`` () =
    let max, min = decimal Int16.MaxValue, decimal Int16.MinValue
    testInt16From<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``int16 string`` () = testInt16From<string> ["1"; "32768"; "-32769"; "str"; null]

  let inline testUInt16From< ^T when ^T : (static member op_Explicit: ^T -> uint16) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint16 x @>); Fun = uint16 }

  [<Test>]
  let ``uint16 int`` () =
    let max, min = int UInt16.MaxValue, int UInt16.MinValue
    testUInt16From<int> [1; max + 1; min - 1]

  [<Test>]
  let ``uint16 bigint`` () =
    let max, min = Numerics.BigInteger(int UInt16.MaxValue), Numerics.BigInteger(int UInt16.MinValue)
    testUInt16From<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``uint16 char`` () = testUInt16From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``uint16 float`` () =
    let max, min = float UInt16.MaxValue, float UInt16.MinValue
    testUInt16From<float> [1.0; max + 1.0; min - 1.0]

  [<Test>]
  let ``uint16 decimal`` () =
    let max, min = decimal UInt16.MaxValue, decimal UInt16.MinValue
    testUInt16From<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``uint16 string`` () = testUInt16From<string> ["1"; "65536"; "-1"; "str"; null]

  let inline testInt32From< ^T when ^T : (static member op_Explicit: ^T -> int32) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int32 x @>); Fun = int32 }

  [<Test>]
  let ``int32 int`` () = testInt32From<int> [1]

  [<Test>]
  let ``int32 char`` () = testInt32From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``int32 float`` () =
    let max, min = float Int32.MaxValue, float Int32.MinValue
    testInt32From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``int32 decimal`` () =
    let max, min = decimal Int32.MaxValue, decimal Int32.MinValue
    testInt32From<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``int32 string`` () = testInt32From<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

  let inline testUInt32From< ^T when ^T : (static member op_Explicit: ^T -> uint32) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint32 x @>); Fun = uint32 }

  [<Test>]
  let ``uint32 int`` () =
    let min = int UInt32.MinValue
    testUInt32From<int> [1; min - 1]

  [<Test>]
  let ``uint32 bigint`` () =
    let max, min = Numerics.BigInteger(UInt32.MaxValue), Numerics.BigInteger(UInt32.MinValue)
    testUInt32From<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``uint32 char`` () = testUInt32From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``uint32 float`` () =
    let max, min = float UInt32.MaxValue, float UInt32.MinValue
    testUInt32From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``uint32 decimal`` () =
    let max, min = decimal UInt32.MaxValue, decimal UInt32.MinValue
    testUInt32From<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``uint32 string`` () = testUInt32From<string> ["1"; "4294967296"; "-1"; "str"; null]

  let inline testInt64From< ^T when ^T : (static member op_Explicit: ^T -> int64) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ int64 x @>); Fun = int64 }

  [<Test>]
  let ``int64 int`` () = testInt64From<int> [1]

  [<Test>]
  let ``int64 bigint`` () =
    let max, min = Numerics.BigInteger(Int64.MaxValue), Numerics.BigInteger(Int64.MinValue)
    testInt64From<bigint> [1I; max + 1I; min + 1I]

  [<Test>]
  let ``int64 char`` () = testInt64From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``int64 float`` () =
    let max, min = float Int64.MaxValue, float Int64.MinValue
    testInt64From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``int64 decimal`` () =
    let max, min = decimal Int64.MaxValue, decimal Int64.MinValue
    testInt64From<decimal> [1.0M; max + 1.0M; min - 1.0M]

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
  let ``uint64 bigint`` () =
    let max, min = Numerics.BigInteger(UInt64.MaxValue), Numerics.BigInteger(UInt64.MinValue)
    testUInt64From<bigint> [1I; max + 1I; min - 1I]

  [<Test>]
  let ``uint64 char`` () = testUInt64From ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``uint64 float`` () =
    let max, min = float UInt64.MaxValue, float UInt64.MinValue
    testUInt64From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

  [<Test>]
  let ``uint64 decimal`` () =
    let max, min = decimal UInt64.MaxValue, decimal UInt64.MinValue
    testUInt64From<decimal> [1.0M; max + 1.0M; min - 1.0M]

  [<Test>]
  let ``uint64 string`` () =
    testUInt64From<string> ["1"; "18446744073709551616"; "-18446744073709551617"; "str"; null]

  let inline testNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> nativeint) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ nativeint x @>); Fun = nativeint }

  [<Test>]
  let ``nativeint int`` () = <@ nativeint 0 @> |> check 0n

  [<Test>]
  let ``nativeint char`` () = testNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``nativeint float`` () = testNativeIntFrom<float> [1.0; nan; infinity; -infinity]

  let inline testUNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> unativeint) > data =
    test { Data = data; ExprFun = (fun (x: ^T) -> <@ unativeint x @>); Fun = unativeint }

  [<Test>]
  let ``unativeint int`` () =
    <@ unativeint 0 @> |> check 0un
    <@ unativeint -1 @> |> check (unativeint -1)

  [<Test>]
  let ``unativeint char`` () = testUNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]

  [<Test>]
  let ``unativeint float`` () = testUNativeIntFrom<float> [1.0; nan; infinity; -infinity]

  [<Test>]
  let ``enum int`` () =
    <@ enum 0 @> |> check StringSplitOptions.None
    <@ enum 1 @> |> check StringSplitOptions.RemoveEmptyEntries
    <@ enum 2 @> |> check (2 |> unbox<StringSplitOptions>)
    <@ enum -1 @> |> check (-1 |> unbox<StringSplitOptions>)

  [<Test>]
  let ``string int`` () = <@ string 42 @> |> check "42"

  [<Test>]
  let ``string bigint`` () = <@ string 1L @> |> check (string 1L)

  [<Test>]
  let ``string char`` () = <@ string 'a' @> |> check "a"

  [<Test>]
  let ``string float`` () =
    <@ string 1.0 @> |> check "1"
    <@ string 1.5 @> |> check "1.5"
    <@ string nan @> |> check (string nan)
    <@ string infinity @> |> check (string infinity)
    <@ string -infinity @> |> check (string -infinity)

  [<Test>]
  let ``string decimal`` () =
    <@ string 1.0M @> |> check (string 1.0M)

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

  module Checked =
    open Microsoft.FSharp.Core.Operators.Checked

    let inline testByteFrom< ^T when ^T : (static member op_Explicit: ^T -> byte) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ byte x @>); Fun = byte }

    [<Test>]
    let ``byte int`` () =
      let max, min = int Byte.MaxValue, int Byte.MinValue
      testByteFrom<int> [1; max + 1; min - 1]

    [<Test>]
    let ``byte bigint`` () =
      let max, min = Numerics.BigInteger(int Byte.MaxValue), Numerics.BigInteger(int Byte.MinValue)
      testByteFrom<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``byte char`` () = testByteFrom ['a'; Char.MinValue; char Byte.MaxValue + char 1]

    [<Test>]
    let ``byte float`` () =
      let max, min = float Byte.MaxValue, float Byte.MinValue
      testByteFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``byte decimal`` () =
      let max, min = decimal Byte.MaxValue, decimal Byte.MinValue
      testByteFrom<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``byte string`` () = testByteFrom<string> ["1"; "256"; "-1"; "str"; null]

    let inline testUInt8From< ^T when ^T : (static member op_Explicit: ^T -> uint8) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint8 x @>); Fun = uint8 }

    [<Test>]
    let ``uint8 int`` () =
      let max, min = int Byte.MaxValue, int Byte.MinValue
      testUInt8From<int> [1; max + 1; min - 1]

    [<Test>]
    let ``uint8 bigint`` () =
      let max, min = Numerics.BigInteger(int Byte.MaxValue), Numerics.BigInteger(int Byte.MinValue)
      testUInt8From<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``uint8 char`` () = testUInt8From ['a']

    [<Test>]
    let ``uint8 float`` () =
      let max, min = float Byte.MaxValue, float Byte.MinValue
      testUInt8From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``uint8 decimal`` () =
      let max, min = decimal Byte.MaxValue, decimal Byte.MinValue
      testUInt8From<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``uint8 string`` () = testUInt8From<string> ["1"; "256"; "-1"; "str"; null]

    let inline testSByteFrom< ^T when ^T : (static member op_Explicit: ^T -> sbyte) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ sbyte x @>); Fun = sbyte }

    [<Test>]
    let ``sbyte int`` () =
      let max, min = int SByte.MaxValue, int SByte.MinValue
      testSByteFrom<int> [1; max + 1; min - 1]

    [<Test>]
    let ``sbyte bigint`` () =
      let max, min = Numerics.BigInteger(int SByte.MaxValue), Numerics.BigInteger(int SByte.MinValue)
      testSByteFrom<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``sbyte char`` () = testSByteFrom ['a'; Char.MinValue; char SByte.MaxValue + char 1]

    [<Test>]
    let ``sbyte float`` () =
      let max, min = float SByte.MaxValue, float SByte.MinValue
      testSByteFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``sbyte decimal`` () =
      let max, min = decimal SByte.MaxValue, decimal SByte.MinValue
      testSByteFrom<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``sbyte string`` () = testSByteFrom<string> ["1"; "128"; "-129"; "str"; null]

    let inline testInt8From< ^T when ^T : (static member op_Explicit: ^T -> int8) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int8 x @>); Fun = int8 }

    [<Test>]
    let ``int8 int`` () =
      let max, min = int SByte.MaxValue, int SByte.MinValue
      testInt8From<int> [1; max + 1; min - 1]

    [<Test>]
    let ``int8 bigint`` () =
      let max, min = Numerics.BigInteger(int SByte.MaxValue), Numerics.BigInteger(int SByte.MinValue)
      testInt8From<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``int8 char`` () = testInt8From ['a']

    [<Test>]
    let ``int8 float`` () =
      let max, min = float Byte.MaxValue, float Byte.MinValue
      testInt8From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``int8 decimal`` () =
      let max, min = decimal SByte.MaxValue, decimal SByte.MinValue
      testInt8From<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``int8 string`` () = testInt8From<string> ["1"; "128"; "-129"; "str"; null]

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
    let ``char decimal`` () =
      let max, min = decimal (int Char.MaxValue), decimal (int Char.MinValue)
      testCharFrom<decimal> [97.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``char string`` () = testCharFrom<string> ["a"; "aa"; null]

    let inline testIntFrom< ^T when ^T : (static member op_Explicit: ^T -> int) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int x @>); Fun = int }

    [<Test>]
    let ``int int`` () = testIntFrom<int> [1]

    [<Test>]
    let ``int bigint`` () =
      let max, min = Numerics.BigInteger(Int32.MaxValue), Numerics.BigInteger(Int32.MinValue)
      testIntFrom<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``int char`` () = testIntFrom ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``int float`` () =
      let max, min = float Int32.MaxValue, float Int32.MinValue
      testIntFrom<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``int decimal`` () =
      let max, min = decimal Int32.MaxValue, decimal Int32.MinValue
      testIntFrom<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``int string`` () = testIntFrom<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

    let inline testInt16From< ^T when ^T : (static member op_Explicit: ^T -> int16) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int16 x @>); Fun = int16 }

    [<Test>]
    let ``int16 int`` () =
      let max, min = int Int16.MaxValue, int Int16.MinValue
      testInt16From<int> [1; max + 1; min - 1]

    [<Test>]
    let ``int16 bigint`` () =
      let max, min = Numerics.BigInteger(int Int16.MaxValue), Numerics.BigInteger(int Int16.MinValue)
      testInt16From<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``int16 char`` () = testInt16From ['a'; Char.MinValue; char Int16.MaxValue + char 1]

    [<Test>]
    let ``int16 float`` () =
      let max, min = float Int16.MaxValue, float Int16.MinValue
      testInt16From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``int16 decimal`` () =
      let max, min = decimal Int16.MaxValue, decimal Int16.MinValue
      testInt16From<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``int16 string`` () = testInt16From<string> ["1"; "32768"; "-32769"; "str"; null]

    let inline testUInt16From< ^T when ^T : (static member op_Explicit: ^T -> uint16) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint16 x @>); Fun = uint16 }

    [<Test>]
    let ``uint16 int`` () =
      let max, min = int UInt16.MaxValue, int UInt16.MinValue
      testUInt16From<int> [1; max + 1; min - 1]
    
    [<Test>]
    let ``uint16 bigint`` () =
      let max, min = Numerics.BigInteger(int UInt16.MaxValue), Numerics.BigInteger(int UInt16.MinValue)
      testUInt16From<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``uint16 char`` () = testUInt16From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``uint16 float`` () =
      let max, min = float UInt16.MaxValue, float UInt16.MinValue
      testUInt16From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``uint16 decimal`` () =
      let max, min = decimal UInt16.MaxValue, decimal UInt16.MinValue
      testUInt16From<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``uint16 string`` () = testUInt16From<string> ["1"; "65536"; "-1"; "str"; null]

    let inline testInt32From< ^T when ^T : (static member op_Explicit: ^T -> int32) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int32 x @>); Fun = int32 }

    [<Test>]
    let ``int32 int`` () = testInt32From<int> [1]

    [<Test>]
    let ``int32 bigint`` () =
      let max, min = Numerics.BigInteger(Int32.MaxValue), Numerics.BigInteger(Int32.MinValue)
      testInt32From<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``int32 char`` () = testInt32From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``int32 float`` () =
      let max, min = float Int32.MaxValue, float Int32.MinValue
      testInt32From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``int32 decimal`` () =
      let max, min = decimal Int32.MaxValue, decimal Int32.MinValue
      testInt32From<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``int32 string`` () = testInt32From<string> ["1"; "2147483648"; "-2147483649"; "str"; null]

    let inline testUInt32From< ^T when ^T : (static member op_Explicit: ^T -> uint32) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ uint32 x @>); Fun = uint32 }

    [<Test>]
    let ``uint32 int`` () =
      let min = int UInt32.MinValue
      testUInt32From<int> [1; min - 1]

    [<Test>]
    let ``uint32 bigint`` () =
      let max, min = Numerics.BigInteger(UInt32.MaxValue), Numerics.BigInteger(UInt32.MinValue)
      testUInt32From<bigint> [1I; max + 1I; min - 1I]
    
    [<Test>]
    let ``uint32 char`` () = testUInt32From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``uint32 float`` () =
      let max, min = float UInt32.MaxValue, float UInt32.MinValue
      testUInt32From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``uint32 decimal`` () =
      let max, min = decimal UInt32.MaxValue, decimal UInt32.MinValue
      testUInt32From<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``uint32 string`` () = testUInt32From<string> ["1"; "4294967296"; "-1"; "str"; null]

    let inline testInt64From< ^T when ^T : (static member op_Explicit: ^T -> int64) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ int64 x @>); Fun = int64 }

    [<Test>]
    let ``int64 int`` () = testInt64From<int> [1]

    [<Test>]
    let ``int64 bigint`` () =
      let max, min = Numerics.BigInteger(Int64.MaxValue), Numerics.BigInteger(Int64.MinValue)
      testInt64From<bigint> [1I; max + 1I; min + 1I]

    [<Test>]
    let ``int64 char`` () = testInt64From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``int64 float`` () =
      let max, min = float Int64.MaxValue, float Int64.MinValue
      testInt64From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``int64 decimal`` () =
      let max, min = decimal Int64.MaxValue, decimal Int64.MinValue
      testInt64From<decimal> [1.0M; max + 1.0M; min - 1.0M]

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
    let ``uint64 bigint`` () =
      let max, min = Numerics.BigInteger(UInt64.MaxValue), Numerics.BigInteger(UInt64.MinValue)
      testUInt64From<bigint> [1I; max + 1I; min - 1I]

    [<Test>]
    let ``uint64 char`` () = testUInt64From ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``uint64 float`` () =
      let max, min = float UInt64.MaxValue, float UInt64.MinValue
      testUInt64From<float> [1.0; max + 1.0; min - 1.0; nan; infinity; -infinity]

    [<Test>]
    let ``uint64 decimal`` () =
      let max, min = decimal UInt64.MaxValue, decimal UInt64.MinValue
      testUInt64From<decimal> [1.0M; max + 1.0M; min - 1.0M]

    [<Test>]
    let ``uint64 string`` () =
      testUInt64From<string> ["1"; "18446744073709551616"; "-18446744073709551617"; "str"; null]

    let inline testNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> nativeint) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ nativeint x @>); Fun = nativeint }

    [<Test>]
    let ``nativeint int`` () = <@ nativeint 0 @> |> check 0n

    [<Test>]
    let ``nativeint char`` () = testNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``nativeint float`` () = testNativeIntFrom<float> [1.0; nan; infinity; -infinity]

    let inline testUNativeIntFrom< ^T when ^T : (static member op_Explicit: ^T -> unativeint) > data =
      test { Data = data; ExprFun = (fun (x: ^T) -> <@ unativeint x @>); Fun = unativeint }

    [<Test>]
    let ``unativeint int`` () =
      <@ unativeint 0 @> |> check 0un
      <@ unativeint -1 @> |> checkExn<_, OverflowException>

    [<Test>]
    let ``unativeint char`` () = testUNativeIntFrom ['a'; Char.MaxValue; Char.MinValue]

    [<Test>]
    let ``unativeint float`` () = testUNativeIntFrom<float> [1.0; nan; infinity; -infinity]
