namespace FSharp.Quotations.Compiler.Tests

#nowarn "62"

[<TestModule>]
module StringOpTest =
  [<TestCase("aaa", "bbb")>]
  [<TestCase(null, "bbb")>]
  [<TestCase("aaa", null)>]
  [<TestCase(null, null)>]
  let ``string ^ string`` (x, y) = <@ x ^ y @> |> check (x ^ y)