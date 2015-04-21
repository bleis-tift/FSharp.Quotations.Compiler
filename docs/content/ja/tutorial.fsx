(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"
#I "../../../bin/FSharp.Quotations.Compiler"

#r "FSharp.Quotations.Compiler.dll"

(**
チュートリアル
==============
このライブラリは簡単に使えます。

このライブラリを使うためには、`FSharp.Quotations.Compiler`名前空間をオープンしておくといいでしょう。
*)
open FSharp.Quotations.Compiler

(**
型付きの式木
------------
次のような式木を実行したい場合を考えてみます。
*)

let x = 10
let y = 20
let expr = <@ x + y @>

(**
`Execute`メソッドを使うだけでできます。
*)

(*** define-output: result1 ***)
printfn "%d + %d = %d" x y (expr.Execute())

(**
このコードを実行すると以下のように出力されます：
*)
(*** include-output: result1 ***)

(**
もし式木をコンパイルして、あとで実行したい場合は、`Compile`メソッドを使います。
*)

let result = expr.Compile()

(**
`Compile`メソッドを使うのと同じ意味で`ExprCompiler.compile`関数も使えます。
*)

let result2 = expr |> ExprCompiler.compile

(**
`result`を実行するためには、`ExecuteCompiledCode`メソッドを使います。
*)

printfn "%d + %d = %d" x y (result.ExecuteCompiledCode())

(**
型なしの式木
------------
このライブラリは、型なしの式木を直接はサポートしていません。
もし型なしの式木を実行したい場合は、`Expr.Cast`メソッドを使います。
このメソッドはF#の標準ライブラリのメソッドです。
*)

open Microsoft.FSharp.Quotations

// キャストのためのヘルパ関数
let cast<'T> (expr: Expr) : Expr<'T> = expr |> Expr.Cast

let untypedExpr = <@@ x + y @@>
let casted = untypedExpr |> cast<int>
printfn "%d + %d = %d" x y (casted.Execute())
