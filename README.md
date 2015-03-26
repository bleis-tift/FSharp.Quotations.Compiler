[![Issue Stats](http://issuestats.com/github/bleis-tift/FSharp.Quotations.Compiler/badge/issue)](http://issuestats.com/github/bleis-tift/FSharp.Quotations.Compiler)
[![Issue Stats](http://issuestats.com/github/bleis-tift/FSharp.Quotations.Compiler/badge/pr)](http://issuestats.com/github/bleis-tift/FSharp.Quotations.Compiler)

# FSharp.Quotations.Compiler

This library is a compiler for F# expression tree.
This is based on System.Reflection.Emit technology.
    
Read the [Getting started tutorial](http://bleis-tift.github.io/FSharp.Quotations.Compiler/index.html#Getting-started) to learn more.

Documentation: http://bleis-tift.github.io/FSharp.Quotations.Compiler

## Goals
* Never happen StackOverflowException
* Compile time is fast enough
* Evaluate time is fast enough
* Contains tests enough and runs fast enough

Of course, For Fun.

## Limitations
The following exprs are not supported yet.

* `AddressOf`
* `AddressSet`
* `ForIntegerRangeLoop`
* `LetRecursive`
* `NewDelegate`
* `Quote`
* `WhileLoop`

And `Value` is supported only the following types and `null`.

* `unit`
* `bool`
* `int`
* `byte`
* `sbyte`
* `int16`
* `uint16`
* `uint32`
* `int64`
* `uint64`
* `float32`
* `float`
* `char`
* `string`

## Maintainer(s)

- [@bleis-tift](https://github.com/bleis-tift)
