(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"
#I "../../../bin/FSharp.Quotations.Compiler"

#r "FSharp.Quotations.Compiler.dll"

open Microsoft.FSharp.Quotations
open FSharp.Quotations.Compiler

(**
制限事項
========
このライブラリにはいくつかの制限事項があります。

未実装機能
----------
* `ForIntegerRangeLoop`
* `LetRecursive`
* `NewDelegate`
* `WhileLoop`

未サポート機能
--------------
* `AddressOf`
* `AddressSet`
* `Quote`

技術的な問題
------------
### インライン関数
`NoDynamicInvocationAttribute`付きの他のインライン関数を含むインライン関数は起動できません。
例えば、次のコードは`System.NotSupportedException`を投げます。
*)

// (-)はNoDynamicInvocationAttribute付き
let inline f1 x y = x - y
try
  let expr = <@ f1 20 10 @>
  expr.Execute() |> ignore
with
  :? System.NotSupportedException -> printfn "raised exception."

(**
もちろん、`NoDynamicInvocationAttribute`付きの関数もこのライブラリでは実行できません。
*)

[<NoDynamicInvocationAttribute >]
let inline f2 x = x
try
  let expr = <@ f2 10 @>
  expr.Execute() |> ignore
with
  :? System.NotSupportedException -> printfn "raised exception."

(**
もし`NoDynamicInvocationAttribute`付きの関数を実行したい場合、
`inline`を取るか、手動でインライン化する必要があります。
*)

let expr = <@ 20 - 10 @>
printfn "20 - 10 = %d" (expr.Execute())

(**
他にも、メンバー制約起動式を含むインライン関数をこのライブラリで実行することはできません。
これに関する回避策はありません。

### mutableとtry-with/try-finally
`try-with`と`try-finally`は値を持つ式なので、このライブラリではこれらをラムダ式にラップします。
*)

let tryWithExpr =
  <@ let x = 10
     let res =
       try x with _ -> 20
     res * 2 @>
let compiledTryWithExpr = tryWithExpr.Compile()

(**
これは以下のようにコンパイルされます。
*)

type lambda0(x) =
  inherit FSharpFunc<unit, int>()

  override __.Invoke(_) =
    try x with _ -> 20

let x = 10
let res = lambda0(x).Invoke()
res * 2

(**
`x`はラムダを表すクラスのフィールドとしてコンパイルされます。
そのため、ラムダ式の本体で`x`を書き換えても、ラムダ式の外側には影響を与えることが出来ません。
*)

let letMutableAndTryWithExpr =
  <@ let mutable y = 0
     let res2 =
       try y <- 10; y with _ -> 20
     (y, res2) @>
let compiledLetMutableAndTryWithExpr = letMutableAndTryWithExpr.Compile()

(**
これは、以下のようにコンパイルされます。
*)

type lambda1(y) =
  inherit FSharpFunc<unit, int>()

  member val y = y with get, set
  override this.Invoke(_) =
    try this.y <- 10; y with _ -> 20

let mutable y = 0
let tmp = lambda1(y)
let res2 = tmp.Invoke()
// ラムダ式の起動後に代入すべきだが、現在の実装ではそうなっていない。
// y <- tmp.y
(y, res2)

(**
### Expr.Value
リテラルを持たない型の`Expr.Value`はサポートされていません。
*)

let valueExpr: Expr<System.DateTime> =
  Expr.Value(System.DateTime.UtcNow)
  |> Expr.Cast

(*** define-output: result1 ***)
try
  valueExpr.Execute() |> ignore
with
  e -> printfn "%s" e.Message

(*** include-output: result1 ***)

(**
`Expr.Value`の代わりに、引用式を使ってください。
*)

let codeQuote = <@ System.DateTime.UtcNow @>
(*** define-output: result2 ***)
printfn "%A" (codeQuote.Execute())

(**
上の引用式は`Expr.Value`にならず、`Expr.PropertyGet`になるため、正しく実行できます。
*)
(*** include-output: result2 ***)
