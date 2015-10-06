(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"
#I "../../../bin/FSharp.Quotations.Compiler"

(**
FSharp.Quotations.Compiler
==========================

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      FSharp.Quotations.Compilerは<a href="https://nuget.org/packages/FSharp.Quotations.Compiler">NuGet</a>からインストールできます:
      <pre>PM> Install-Package FSharp.Quotations.Compiler</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

概要
----
FSharp.Quotations.Compilerは、F#の式木をILにコンパイルして実行するためのライブラリです。

同じようなライブラリには、以下のようなものがあります。

* [FSharp.Quotations.Evaluator](http://fsprojects.github.io/FSharp.Quotations.Evaluator/)
* [QuotationCompiler](https://github.com/eiriktsarpalis/QuotationCompiler)
* [Unquote](https://code.google.com/p/unquote/)

これらのライブラリとFSharp.Quotations.Compilerの大きな違いは、
式木の解析に(再帰ではなく)ループを使っていることです。
そのため、このライブラリはスタックオーバーフローを気にすることなく使えます。

簡単な例
--------

*)
#r "FSharp.Quotations.Compiler.dll"
open FSharp.Quotations.Compiler

(*** define-output: result ***)
let expr = <@ 10 + 20 @>
let res = expr.Compile()
printfn "%d" (res.ExecuteCompiledCode())

(**
このコードを実行すると以下のように出力されます：
*)
(*** include-output: result ***)

(**

サンプルとドキュメント
----------------------
 * [チュートリアル](tutorial.html)には、ライブラリのサンプルがあります。
 * [制限事項](limitations.html)には、このライブラリの制限事項についての説明があります。
 * [APIリファレンス](../reference/index.html)には、このライブラリのすべての型、モジュール、関数についての自動生成されたドキュメントがあります。

コントリビュート
----------------
このプロジェクトは[Github][gh]でホストされています。
[イシューを報告][issues]でき、フォークしてプルリクエストを送ることもできます。


このライブラリは、CC0で公開されており、商用、非商用問わず、変更や再頒布可能です。
より詳しい情報は、Githubリポジトリの[ライセンスファイル][license]をご覧ください。

  [content]: https://github.com/bleis-tift/FSharp.Quotations.Compiler/tree/master/docs/content
  [gh]: https://github.com/bleis-tift/FSharp.Quotations.Compiler
  [issues]: https://github.com/bleis-tift/FSharp.Quotations.Compiler/issues
  [license]: https://github.com/bleis-tift/FSharp.Quotations.Compiler/blob/master/LICENSE.txt
*)
