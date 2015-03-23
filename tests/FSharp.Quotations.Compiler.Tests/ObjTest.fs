namespace FSharp.Quotations.Compiler.Tests

[<TestModule>]
module ObjTest =
  type Class (value: int) =
    member __.Value = value
    override __.Equals(x) =
      match x with
      | :? Class as other -> value = other.Value
      | _ -> false
    override __.GetHashCode() = value
    override __.ToString() = sprintf "Class(%d)" value

  [<Test>]
  let ``new`` () = <@ new Class(42) @> |> check (new Class(42))

  [<Test>]
  let ``ToString()`` () = <@ (Class(42).ToString()) @> |> check (Class(42).ToString())

  [<Test>]
  let ``property get`` () = <@ (Class(42)).Value @> |> check 42