namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ObjTest =
  type Class (value: int) as this =
    [<DefaultValue>] val mutable public InstanceField : string
    do
      this.InstanceField <- "str"
    member val Value = value with get, set
    static member val SValue = -1 with get, set
    override __.Equals(x) =
      match x with
      | :? Class as other -> value = other.Value
      | _ -> false
    override __.GetHashCode() = value
    override __.ToString() = sprintf "Class(%d)" value

  [<Test>]
  let ``new`` () = <@ new Class(42) @> |> check (new Class(42))

  [<Test>]
  let ``struct default value`` () = <@ System.Guid() @> |> check (System.Guid())

  [<Test>]
  let ``ToString()`` () = <@ (Class(42).ToString()) @> |> check (Class(42).ToString())

  [<Test>]
  let ``property get`` () = <@ (Class(42)).Value @> |> check 42

  [<Test>]
  let ``property set`` () =
    <@ let c = Class(42)
       c.Value <- 10
       c.Value
    @> |> check 10

  [<Test>]
  let ``static property get`` () = <@ Class.SValue @> |> check -1

  [<Test>]
  let ``static property set`` () =
    <@ Class.SValue <- 10
       Class.SValue
    @> |> check 10

  [<Test>]
  let ``field get`` () = <@ let c = Class(10) in c.InstanceField @> |> check "str"

  [<Test>]
  let ``field set`` () =
    <@ let c = Class(10)
       c.InstanceField <- "new str"
       c.InstanceField
    @> |> check "new str"

  [<Test>]
  let ``value type stringify`` () =
    <@ (1).ToString() @> |> check ((1).ToString())
    <@ (true).ToString() @> |> check ((true).ToString())
    <@ "aaa".ToString() @> |> check ("aaa".ToString())
    <@ 'a'.ToString() @> |> check ('a'.ToString())

  [<Test>]
  let ``value type get type`` () =
    <@ (1).GetType() @> |> check ((1).GetType())
    <@ (true).GetType() @> |> check ((true).GetType())
    <@ "aaa".GetType() @> |> check ("aaa".GetType())
    <@ 'a'.GetType() @> |> check ('a'.GetType())

  [<TestCase(1, 1)>]
  [<TestCase(1, 2)>]
  [<TestCase(2, 1)>]
  let ``int compare to`` (a: int, b: int) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))

  [<Test>]
  let ``bool compare to`` ([<Values(true, false)>] a: bool, [<Values(true, false)>] b: bool) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))

  [<TestCase("aaa", "aaa")>]
  [<TestCase("aaa", "bbb")>]
  [<TestCase("bbb", "aaa")>]
  let ``string compare to`` (a: string, b: string) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))

  [<TestCase('a', 'a')>]
  [<TestCase('a', 'b')>]
  [<TestCase('b', 'a')>]
  let ``char compare to`` (a: char, b: char) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))

  [<Test>]
  let ``value type compare to null`` () =
    <@ (1).CompareTo(null) @> |> check ((1).CompareTo(null))
    <@ (true).CompareTo(null) @> |> check ((true).CompareTo(null))
    <@ "aaa".CompareTo(null) @> |> check ("aaa".CompareTo(null))
    <@ 'a'.CompareTo(null) @> |> check ('a'.CompareTo(null))

  [<TestCase(1, 1)>]
  [<TestCase(1, 2)>]
  [<TestCase(2, 1)>]
  let ``int equals`` (a: int, b: int) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))

  [<Test>]
  let ``bool equals`` ([<Values(true, false)>] a: bool, [<Values(true, false)>] b: bool) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))

  [<TestCase("aaa", "aaa")>]
  [<TestCase("aaa", "bbb")>]
  [<TestCase("bbb", "aaa")>]
  let ``string equals`` (a: string, b: string) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))

  [<TestCase('a', 'a')>]
  [<TestCase('a', 'b')>]
  [<TestCase('b', 'a')>]
  let ``char equals`` (a: char, b: char) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))

  [<Test>]
  let ``value type equals null`` () =
    <@ (1).CompareTo(null) @> |> check ((1).CompareTo(null))
    <@ (true).CompareTo(null) @> |> check ((true).CompareTo(null))
    <@ "aaa".CompareTo(null) @> |> check ("aaa".CompareTo(null))
    <@ 'a'.CompareTo(null) @> |> check ('a'.CompareTo(null))

  [<Test>]
  let ``value type get type code`` () =
    <@ (1).GetTypeCode() @> |> check ((1).GetTypeCode())
    <@ (true).GetTypeCode() @> |> check ((true).GetTypeCode())
    <@ "aaa".GetTypeCode() @> |> check ("aaa".GetTypeCode())
    <@ 'a'.GetTypeCode() @> |> check ('a'.GetTypeCode())

  [<Test>]
  let ``value type get hash code`` () =
    <@ (1).GetHashCode() @> |> check ((1).GetHashCode())
    <@ (true).GetHashCode() @> |> check ((true).GetHashCode())
    <@ "aaa".GetHashCode() @> |> check ("aaa".GetHashCode())
    <@ 'a'.GetHashCode() @> |> check ('a'.GetHashCode())
