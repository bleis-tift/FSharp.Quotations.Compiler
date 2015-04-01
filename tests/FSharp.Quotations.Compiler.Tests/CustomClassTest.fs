namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module CustomClassTest =
  type CustomClass(id: int) =
    member __.Id = id
    override __.Equals(o) =
      match o with
      | :? CustomClass as cc -> cc.Id = id
      | _ -> false
    override __.GetHashCode() = id.GetHashCode()
    override __.ToString() = string id

    static member (~+) (x: CustomClass) = CustomClass(abs x.Id)
    static member (~-) (x: CustomClass) = CustomClass(-x.Id)

    static member (-) (x: CustomClass, y: CustomClass) = CustomClass(x.Id - y.Id)
    static member (-) (x: CustomClass, y: int) = CustomClass(x.Id - y)
    static member (-) (x: int, y: CustomClass) = CustomClass(x - y.Id)

    static member (/) (x: CustomClass, y: CustomClass) = CustomClass(x.Id / y.Id)
    static member (/) (x: CustomClass, y: int) = CustomClass(x.Id / y)
    static member (/) (x: int, y: CustomClass) = CustomClass(x / y.Id)

    static member op_Explicit (x: CustomClass) = byte x.Id

  [<Test>]
  let ``+ CustomClass`` () =
    <@ + (CustomClass(42)) @> |> check (CustomClass(42))
    <@ + (CustomClass(-42)) @> |> check (CustomClass(42))

  [<Test>]
  let ``- CustomClass`` () =
    <@ - (CustomClass(42)) @> |> check (- (CustomClass(42)))

  [<Test>]
  let ``x - y`` () =
    <@ CustomClass(42) - CustomClass(10) @> |> check (CustomClass(32))
    <@ CustomClass(42) - 10 @> |> check (CustomClass(32))
    <@ 42 - CustomClass(10) @> |> check (CustomClass(32))

  [<Test>]
  let ``x / y`` () =
    <@ CustomClass(42) / CustomClass(2) @> |> check (CustomClass(21))
    <@ CustomClass(42) / 2 @> |> check (CustomClass(21))
    <@ 42 / CustomClass(2) @> |> check (CustomClass(21))

  [<Test>]
  let ``byte x`` () =
    <@ byte <| CustomClass(42) @> |> check (42uy)