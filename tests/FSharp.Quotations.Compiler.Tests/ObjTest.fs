namespace FSharp.Quotations.Compiler.Tests

open System
open FSharp.Quotations.Compiler.Tests.CSharp

[<TestModule>]
module ObjTest =
  type Class (value: int) as this =
    [<DefaultValue>] val mutable public InstanceField : string
    do
      this.InstanceField <- "str"
    member val Value = value with get, set
    static member val SValue = -1 with get, set
    member __.VarArgMethod([<ParamArray>] xs: int[]) = xs.Length
    member __.OptArgMethod(x: int, ?y: int) = x + (defaultArg y -1)
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
  let ``variable argument method call`` () =
    <@ let x = Class(42)
       x.VarArgMethod(10, 20, 30) @>
    |> check 3

    // call variable argument method written by C#
    <@ String.Format("{0}{1}", "hoge", 42) @> |> check "hoge42"

  [<Test>]
  let ``optional argument method call`` () =
    <@ let x = Class(42)
       x.OptArgMethod(10) @>
    |> check 9

    // call optional argument method written by C#
    <@ CSharpClass.OptionalArgument(10) @> |> check 9

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
  let ``static field get`` () = <@ CSharpClass.StaticField @> |> check CSharpClass.StaticField

  [<Test>]
  let ``static field set`` () =
    let initialStaticFieldValue = CSharpClass.StaticField
    try
      <@ CSharpClass.StaticField <- initialStaticFieldValue * 2
         CSharpClass.StaticField @>
      |> check (initialStaticFieldValue * 2)
    finally
      CSharpClass.StaticField <- initialStaticFieldValue

  [<Struct>]
  type Struct(i: int) =
    member __.Value = i

  [<Test>]
  let ``copied struct property get`` () =
    <@
      let x = Struct(1)
      let y = x
      y.Value
    @>
    |> check 1

  [<Test>]
  let ``struct property get`` () =
    <@ Struct(1).Value @>
    |> check 1

  [<Test>]
  let ``value type stringify`` () =
    <@ (1).ToString() @> |> check ((1).ToString())
    <@ (true).ToString() @> |> check ((true).ToString())
    <@ 'a'.ToString() @> |> check ('a'.ToString())
    <@ "aaa".ToString() @> |> check ("aaa".ToString())
    <@ (box 1).ToString() @> |> check ((box 1).ToString())
    <@ (box true).ToString() @> |> check ((box true).ToString())
    <@ (box 'a').ToString() @> |> check ((box 'a').ToString())
    <@ (box "aaa").ToString() @> |> check ((box "aaa").ToString())
    <@ Struct(1).ToString() @> |> check (Struct(1).ToString())

  [<Test>]
  let ``value type stringify in lambda`` () =
    <@ (fun (x: int) -> x.ToString()) 42 @> |> check "42"

  [<Test>]
  let ``value type stringify in lambda2`` () =
    <@ let x = 42 in (fun () -> x.ToString()) () @> |> check "42"

  [<Test>]
  let ``value type get type`` () =
    <@ (1).GetType() @> |> check ((1).GetType())
    <@ (true).GetType() @> |> check ((true).GetType())
    <@ 'a'.GetType() @> |> check ('a'.GetType())
    <@ "aaa".GetType() @> |> check ("aaa".GetType())
    <@ (box 1).GetType() @> |> check ((box 1).GetType())
    <@ (box true).GetType() @> |> check ((box true).GetType())
    <@ (box 'a').GetType() @> |> check ((box 'a').GetType())
    <@ (box "aaa").GetType() @> |> check ((box "aaa").GetType())
    <@ Struct(1).GetType() @> |> check (Struct(1).GetType())

  [<TestCase(1, 1)>]
  [<TestCase(1, 2)>]
  [<TestCase(2, 1)>]
  let ``int compare to`` (a: int, b: int) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))
    <@ let a = a :> IComparable<_> in a.CompareTo(b) @> |> check ((a :> IComparable<_>).CompareTo(b))

  [<Test>]
  let ``bool compare to`` ([<Values(true, false)>] a: bool, [<Values(true, false)>] b: bool) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))
    <@ let a = a :> IComparable<_> in a.CompareTo(b) @> |> check ((a :> IComparable<_>).CompareTo(b))

  [<TestCase('a', 'a')>]
  [<TestCase('a', 'b')>]
  [<TestCase('b', 'a')>]
  let ``char compare to`` (a: char, b: char) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))
    <@ let a = a :> IComparable<_> in a.CompareTo(b) @> |> check ((a :> IComparable<_>).CompareTo(b))

  [<TestCase("aaa", "aaa")>]
  [<TestCase("aaa", "bbb")>]
  [<TestCase("bbb", "aaa")>]
  let ``string compare to`` (a: string, b: string) =
    <@ let a = a in a.CompareTo(b) @> |> check (a.CompareTo(b))
    <@ let a = a :> IComparable<_> in a.CompareTo(b) @> |> check ((a :> IComparable<_>).CompareTo(b))

  [<Test>]
  let ``value type compare to null`` () =
    <@ (1).CompareTo(null) @> |> check ((1).CompareTo(null))
    <@ (true).CompareTo(null) @> |> check ((true).CompareTo(null))
    <@ 'a'.CompareTo(null) @> |> check ('a'.CompareTo(null))
    <@ "aaa".CompareTo(null) @> |> check ("aaa".CompareTo(null))
    <@ (1 :> IComparable).CompareTo(null) @> |> check ((1 :> IComparable).CompareTo(null))
    <@ (true :> IComparable).CompareTo(null) @> |> check ((true :> IComparable).CompareTo(null))
    <@ ('a' :> IComparable).CompareTo(null) @> |> check (('a' :> IComparable).CompareTo(null))
    <@ ("aaa" :> IComparable).CompareTo(null) @> |> check (("aaa" :> IComparable).CompareTo(null))

  [<TestCase(1, 1)>]
  [<TestCase(1, 2)>]
  [<TestCase(2, 1)>]
  let ``int equals`` (a: int, b: int) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))
    <@ let a = a in a.Equals(box b) @> |> check (a.Equals(box b))
    <@ let a = a in (box a).Equals(box b) @> |> check ((box a).Equals(box b))

  [<Test>]
  let ``bool equals`` ([<Values(true, false)>] a: bool, [<Values(true, false)>] b: bool) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))
    <@ let a = a in a.Equals(box b) @> |> check (a.Equals(box b))
    <@ let a = a in (box a).Equals(box b) @> |> check ((box a).Equals(box b))

  [<TestCase('a', 'a')>]
  [<TestCase('a', 'b')>]
  [<TestCase('b', 'a')>]
  let ``char equals`` (a: char, b: char) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))
    <@ let a = a in a.Equals(box b) @> |> check (a.Equals(box b))
    <@ let a = a in (box a).Equals(box b) @> |> check ((box a).Equals(box b))

  [<TestCase("aaa", "aaa")>]
  [<TestCase("aaa", "bbb")>]
  [<TestCase("bbb", "aaa")>]
  let ``string equals`` (a: string, b: string) =
    <@ let a = a in a.Equals(b) @> |> check (a.Equals(b))
    <@ let a = a in a.Equals(box b) @> |> check (a.Equals(box b))
    <@ let a = a in (box a).Equals(box b) @> |> check ((box a).Equals(box b))

  [<Test>]
  let ``value type equals null`` () =
    <@ (1).CompareTo(null) @> |> check ((1).CompareTo(null))
    <@ (true).CompareTo(null) @> |> check ((true).CompareTo(null))
    <@ 'a'.CompareTo(null) @> |> check ('a'.CompareTo(null))
    <@ "aaa".CompareTo(null) @> |> check ("aaa".CompareTo(null))

  [<Test>]
  let ``struct equals`` () =
    <@ Struct(1).Equals(Struct(1)) @> |> check (Struct(1).Equals(Struct(1)))

  [<Test>]
  let ``value type get type code`` () =
    <@ (1).GetTypeCode() @> |> check ((1).GetTypeCode())
    <@ (true).GetTypeCode() @> |> check ((true).GetTypeCode())
    <@ 'a'.GetTypeCode() @> |> check ('a'.GetTypeCode())
    <@ "aaa".GetTypeCode() @> |> check ("aaa".GetTypeCode())

  [<Test>]
  let ``value type get hash code`` () =
    <@ (1).GetHashCode() @> |> check ((1).GetHashCode())
    <@ (true).GetHashCode() @> |> check ((true).GetHashCode())
    <@ 'a'.GetHashCode() @> |> check ('a'.GetHashCode())
    <@ "aaa".GetHashCode() @> |> check ("aaa".GetHashCode())
    <@ Struct(1).GetHashCode() @> |> check (Struct(1).GetHashCode())
