asm.js
======

翻訳してみる。

Working Draft — 18 August 2014
------------------------------

Latest version:   
[http://asmjs.org/spec/latest/](http://asmjs.org/spec/latest/)

Editors:   
David Herman, Mozilla, \<dherman@mozilla.com\>

Luke Wagner, Mozilla, \<luke@mozilla.com\>

Alon Zakai, Mozilla, \<azakai@mozilla.com\>

Abstract
--------

This specification defines **asm.js**, a strict subset of JavaScript that can be used as a low-level, efficient target language for compilers. This sublanguage effectively describes a sandboxed virtual machine for memory-unsafe languages like C or C++. A combination of static and dynamic validation allows JavaScript engines to employ an ahead-of-time (AOT) optimizing compilation strategy for valid asm.js code.

この仕様は、** asm.js**、低レベル、コンパイラのための効率的なターゲット言語として使用することができるJavaScriptの厳密なサブセットを定義します。このサブ言語は、効果的にCやC++のようなメモリ·危険な言語用にサンドボックス仮想マシンを説明しています。静的および動的検証の組み合わせは、JavaScriptエンジンが有効なasm.jsコードの実行前（AOT）最適化コンパイルの戦略を採用することができます。

Status
------

This specification is working towards a candidate draft for asm.js version 1. Mozilla's SpiderMonkey JavaScript engine provides an optimizing implementation of this draft.

この仕様はMozillaのSpiderMonkeyのJavaScriptエンジンがこの案の最適化実装を提供しasm.jsバージョン1のドラフト案に向けて取り組んでいます。

### Changelog

-   **18 August 2014**
    -   better "putting it all together" example
-   **23 July 2014**
    -   formatting cleanups
    -   added variadic function types to the Global Types section
-   **22 July 2014**
    -   clarified formal structure with explicit validation rule names
    -   moved function table validation from annotations section to validation section
    -   separated `case` and `default` validation rules
    -   eliminated unused expected case type parameter
    -   corrected type checks to subtype checks in *AdditiveExpression*, *BitwiseXORExpression*, *BitwiseANDExpression*, *BitwiseORExpression*, and *ConditionalExpression*
-   **8 July 2014**
    -   minor editorial bugfixes
    -   non-function foreign imports are `mut`
    -   tightened the language on linking restrictions
-   **7 July 2014**
    -   added 32-bit floating point types
    -   renamed `doublish` to [`double?`](#double-2) for symmetry with [`float?`](#float-2)
    -   separated heap access checking into a separate validation section
    -   separated load and store types for heap views
    -   added `Math.fround` and singleton `fround` type
    -   added a Float Coercions section
    -   added uncoerced *CallExpression* nodes to *Expression* for float coercions
    -   added float coercions to initializers, return type annotations, and legal function calls
    -   added restriction preventing float coercions of FFI calls
    -   added [`float`](#float-1) support for operators and `Math` functions
    -   added [`float`](#float-1) to legal result types for *ConditionalExpression*
    -   added variadic `Math.min` and `Math.max`
    -   eliminated the allowance for 1-byte views to elide their index shift (to future-proof for large heaps)
    -   simplified and generalized link-time restrictions on heap size
-   **12 December 2013**
    -   return type of `Math.abs` is [`signed`](#signed-0)
-   **11 October 2013**
    -   [`unsigned`](#unsigned-0) is not an [`extern`](#extern-0) type
    -   added missing `!` operator to *UnaryExpression* operators
    -   added note about `~~` to *Unary Operators* section
    -   added note about parenthesis agnosticism to *Syntax* section
    -   added note about ASI to *Syntax* section
    -   added `-NumericLiteral` cases everywhere
    -   function calls require explicit coercions
    -   eliminated type `unknown`, which is no longer needed
    -   return type of integer `%` is [`intish`](#intish-0)

Table of Contents
-----------------

1.  [1 Introduction](#introduction)
2.  [2 Types](#types)
    1.  [2.1 Value Types](#value-types)
        1.  [2.1.1 void](#void)
        2.  [2.1.2 double](#double)
        3.  [2.1.3 signed](#signed)
        4.  [2.1.4 unsigned](#unsigned)
        5.  [2.1.5 int](#int)
        6.  [2.1.6 fixnum](#fixnum)
        7.  [2.1.7 intish](#intish)
        8.  [2.1.8 double?](#double-0)
        9.  [2.1.9 float](#float)
        10. [2.1.10 float?](#float-0)
        11. [2.1.11 floatish](#floatish)
        12. [2.1.12 extern](#extern)

    2.  [2.2 Global Types](#global-types)

3.  [3 Environments](#environments)
    1.  [3.1 Global Environment](#global-environment)
    2.  [3.2 Variable Environment](#variable-environment)
    3.  [3.3 Environment Lookup](#environment-lookup)

4.  [4 Syntax](#syntax)
5.  [5 Annotations](#annotations)
    1.  [5.1 Parameter Type Annotations](#parameter-type-annotations)
    2.  [5.2 Return Type Annotations](#return-type-annotations)
    3.  [5.3 Function Type Annotations](#function-type-annotations)
    4.  [5.4 Variable Type Annotations](#variable-type-annotations)
    5.  [5.5 Global Variable Type Annotations](#global-variable-type-annotations)
    6.  [5.6 Function Table Types](#function-table-types)

6.  [6 Validation Rules](#validation-rules)
    1.  [6.1 *ValidateModule*(*f*)](#validatemodule-f)
    2.  [6.2 *ValidateExport*(Δ, *s*)](#validateexport-s)
    3.  [6.3 *ValidateFunctionTable*(Δ, *s*)](#validatefunctiontable-s)
    4.  [6.4 *ValidateFunction*(Δ, *f*)](#validatefunction-f)
    5.  [6.5 *ValidateStatement*(Δ, Γ, τ, *s*)](#validatestatement-s)
        1.  [6.5.1 Block](#block)
        2.  [6.5.2 ExpressionStatement](#expressionstatement)
        3.  [6.5.3 EmptyStatement](#emptystatement)
        4.  [6.5.4 IfStatement](#ifstatement)
        5.  [6.5.5 ReturnStatement](#returnstatement)
        6.  [6.5.6 IterationStatement](#iterationstatement)
        7.  [6.5.7 BreakStatement](#breakstatement)
        8.  [6.5.8 ContinueStatement](#continuestatement)
        9.  [6.5.9 LabelledStatement](#labelledstatement)
        10. [6.5.10 SwitchStatement](#switchstatement)

    6.  [6.6 *ValidateCase*(Δ, Γ, τ, *c*)](#validatecase-c)
    7.  [6.7 *ValidateDefault*(Δ, Γ, τ, *d*)](#validatedefault-d)
    8.  [6.8 *ValidateExpression*(Δ, Γ, *e*)](#validateexpression-e)
        1.  [6.8.1 Expression](#expression)
        2.  [6.8.2 NumericLiteral](#numericliteral)
        3.  [6.8.3 Identifier](#identifier)
        4.  [6.8.4 CallExpression](#callexpression)
        5.  [6.8.5 MemberExpression](#memberexpression)
        6.  [6.8.6 AssignmentExpression](#assignmentexpression)
        7.  [6.8.7 UnaryExpression](#unaryexpression)
        8.  [6.8.8 MultiplicativeExpression](#multiplicativeexpression)
        9.  [6.8.9 AdditiveExpression](#additiveexpression)
        10. [6.8.10 ShiftExpression](#shiftexpression)
        11. [6.8.11 RelationalExpression](#relationalexpression)
        12. [6.8.12 EqualityExpression](#equalityexpression)
        13. [6.8.13 BitwiseANDExpression](#bitwiseandexpression)
        14. [6.8.14 BitwiseXORExpression](#bitwisexorexpression)
        15. [6.8.15 BitwiseORExpression](#bitwiseorexpression)
        16. [6.8.16 ConditionalExpression](#conditionalexpression)
        17. [6.8.17 Parenthesized Expression](#parenthesized-expression)

    9.  [6.9 *ValidateCall*(Δ, Γ, τ, *e*)](#validatecall-e)
    10. [6.10 *ValidateHeapAccess*(Δ, Γ, *e*)](#validateheapaccess-e)
    11. [6.11 *ValidateFloatCoercion*(Δ, Γ, *e*)](#validatefloatcoercion-e)

7.  [7 Linking](#linking-0)
8.  [8 Operators](#operators)
    1.  [8.1 Unary Operators](#unary-operators)
    2.  [8.2 Binary Operators](#binary-operators)

9.  [9 Standard Library](#standard-library)
10. [10 Heap View Types](#heap-view-types)
11. [Acknowledgements](#acknowledgements)

1 Introduction
--------------

This specification defines asm.js, a strict subset of JavaScript that can be used as a low-level, efficient target language for compilers. The asm.js language provides an abstraction similar to the C/C++ virtual machine: a large binary heap with efficient loads and stores, integer and floating-point arithmetic, first-order function definitions, and function pointers.

この仕様はasm.js、低レベル、コンパイラのための効率的なターゲット言語として使用することができるJavaScriptの厳密なサブセットを定義します。効率的なロードとストア、整数と浮動小数点演算、一次関数定義、および関数ポインタを持つ大規模なバイナリヒープ：asm.js言語はC / C++の仮想マシンに似て抽象化を提供します。

### Programming Model

The asm.js programming model is built around integer and floating-point arithmetic and a virtual heap represented as a [typed array](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays). While JavaScript does not directly provide constructs for dealing with integers, they can be emulated using two tricks:

asm.jsプログラミングモデルは、整数と浮動小数点演算と[typed array](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays)として表される仮想ヒープを中心に構築されている。 JavaScriptが直接整数を扱うための構造を提供していませんが、彼らは2トリックを使用してエミュレートすることができます:

-   integer loads and stores can be performed using the typed arrays API; and
-   integer arithmetic is equivalent to the composition of JavaScript's floating-point arithmetic operators with the integer coercions performed by the bitwise operators.

- 整数ロードとストアは型付けされた配列のAPIを使用して行うことができる。と
- 整数演算はビット単位の演算子によって実行される整数の型変換ではJavaScriptの浮動小数点算術演算子の構成と同等である。

As an example of the former, if we have an [Int32Array](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/Int32Array) view of the heap called `HEAP32`, then we can load the 32-bit integer at byte offset `p`:

我々は `HEAP32`と呼ばれるヒープの[Int32Array](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/Int32Array)ビューを持っている場合、前者の例として、我々はバイトオフセット`p`での32ビット整数をロードすることができます：


    HEAP32[p >> 2]|0


The shift converts the byte offset to a 32-bit element offset, and the bitwise coercion ensures that an out-of-bounds access is coerced from `undefined` back to an integer.

シフトは、オフセット、32ビット要素にバイトオフセットを変換して、ビット単位の強制は、領域外アクセスの返り値を `undefined` から整数に強制されることを保証します。


As an example of integer arithmetic, addition can be performed by taking two integer values, adding them with the built-in addition operator, and coercing the result back to an integer via the bitwise or operator:

整数演算の一例として、さらにはそれらを追加つの整数値をとることによって行うことができる加算演算子を内蔵しており、ビット単位またはオペレータを経由して整数に結果を強要。

    (x+y)|0

This programming model is directly inspired by the techniques pioneered by the [Emscripten](http://emscripten.org) and [Mandreel](http://mandreel.com) compilers.


このプログラミング·モデルは、直接 [Emscripten](http://emscripten.org) と [Mandreel](http://mandreel.com) コンパイラによって開拓された技術にインスパイアされています。

### Validation

The asm.js sub-language is defined by a [static type system](#validation-rules) that can be checked at JavaScript parse time. Validation of asm.js code is designed to be "pay-as-you-go" in that it is never performed on code that does not request it. An asm.js module requests validation by means of a special [prologue directive](http://ecma-international.org/ecma-262/5.1/#sec-14.1), similar to that of ECMAScript Edition 5's [strict mode](http://ecma-international.org/ecma-262/5.1/#sec-10.1.1):

    function MyAsmModule() {
        "use asm";
        // module body
    }

This explicit directive allows JavaScript engines to avoid performing pointless and potentially costly validation on other JavaScript code, and to report validation errors in developer consoles only where relevant.

### Ahead-Of-Time Compilation

Because asm.js is a strict subset of JavaScript, this specification only defines the validation logic—the execution semantics is simply that of JavaScript. However, validated asm.js is amenable to ahead-of-time (AOT) compilation. Moreover, the code generated by an AOT compiler can be quite efficient, featuring:

-   unboxed representations of integers and floating-point numbers;
-   absence of runtime type checks;
-   absence of garbage collection; and
-   efficient heap loads and stores (with implementation strategies varying by platform).

Code that fails to validate must fall back to execution by traditional means, e.g., interpretation and/or just-in-time (JIT) compilation.

### Linking

Using an asm.js module requires calling its function to obtain an object containing the module's exports; this is known as linking. An asm.js module can also be given access to standard libraries and custom JavaScript functions through linking. An AOT implementation must perform certain [dynamic checks](#linking-0) to check compile-time assumptions about the linked libraries in order to make use of the compiled code.

This figure depicts a simple architecture of an AOT implementation that otherwise employs a simple interpreter. If either dynamic or static validation fails, the implementation must fall back to the interpreter. But if both validations succeed, calling the module exports executes the binary executable code generated by AOT compilation.

[![](aot.png)](aot.png)

### External Code and Data

Within an asm.js module, all code is fully statically typed and limited to the very restrictive asm.js dialect. However, it is possible to interact with recognized standard JavaScript libraries and even custom dynamic JavaScript functions.

An asm.js module can take up to three optional parameters, providing access to external JavaScript code and data:

-   a standard library object, providing access to a limited subset of the JavaScript [standard libraries](#standard-library);
-   a foreign function interface (FFI), providing access to custom external JavaScript functions; and
-   a heap buffer, providing a single [`ArrayBuffer`](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/ArrayBuffer) to act as the asm.js heap.

These objects allow asm.js to call into external JavaScript (and to share its heap buffer with external JavaScript). Conversely, the exports object returned from the module allows external JavaScript to call into asm.js.

So in the general case, an asm.js module declaration looks like:

    function MyAsmModule(stdlib, foreign, heap) {
        "use asm";

        // module body...

        return {
            export1: f1,
            export2: f2,
            // ...
        };
    }

Function parameters in asm.js are provided a type annotation by means of an explicit coercion on function entry:

    function geometricMean(start, end) {
      start = start|0; // start has type int
      end = end|0;     // end has type int
      return +exp(+logSum(start, end) / +((end - start)|0));
    }

These annotations serve two purposes: first, to provide the function's type signature so that the validator can enforce that all calls to the function are well-typed; second, to ensure that even if the function is exported and called by external JavaScript, its arguments are dynamically coerced to the expected type. This ensures that an AOT implementation can use unboxed value representations, knowing that once the dynamic coercions have completed, the function body never needs any runtime type checks.

### Putting It All Together

The following is a small but complete example of an asm.js module.

    function GeometricMean(stdlib, foreign, buffer) {
      "use asm";

      var exp = stdlib.Math.exp;
      var log = stdlib.Math.log;
      var values = new stdlib.Float64Array(buffer);

      function logSum(start, end) {
        start = start|0;
        end = end|0;

        var sum = 0.0, p = 0, q = 0;

        // asm.js forces byte addressing of the heap by requiring shifting by 3
        for (p = start << 3, q = end << 3; (p|0) < (q|0); p = (p + 8)|0) {
          sum = sum + +log(values[p>>3]);
        }

        return +sum;
      }

      function geometricMean(start, end) {
        start = start|0;
        end = end|0;

        return +exp(+logSum(start, end) / +((end - start)|0));
      }

      return { geometricMean: geometricMean };
    }

In a JavaScript engine that supports AOT compilation of asm.js, calling the module on a proper global object and heap buffer would link the exports object to use the statically compiled functions.

    var heap = new ArrayBuffer(0x10000);          // 64k heap
    init(heap, START, END);                       // fill a region with input values
    var fast = GeometricMean(window, null, heap); // produce exports object linked to AOT-compiled code
    fast.geometricMean(START, END);               // computes geometric mean of input values

By contrast, calling the module on a standard library object containing something other than the true `Math.exp` or `Math.log` would fail to produce AOT-compiled code:

    var bogusGlobal = {
      Math: {
        exp: function(x) { return x; },
        log: function(x) { return x; }
      },
      Float64Array: Float64Array
    };

    var slow = GeometricMean(bogusGlobal, null, heap); // produces purely-interpreted/JITted version
    console.log(slow.geometricMean(START, END));       // computes bizarro-geometric mean thanks to bogusGlobal

2 Types
-------

Validation of an asm.js module relies on a static type system that classifies and constrains the syntax. This section defines the types used by the validation logic.

### 2.1 Value Types

Validation in asm.js limits JavaScript programs to only use operations that can be mapped closely to efficient data representations and machine operations of modern architectures, such as 32-bit integers and integer arithmetic.

The types of asm.js values are inter-related by a subtyping relation, which can be represented pictorially:

[![](subtypes.png)](subtypes.png)

The light boxes represent arbitrary JavaScript values that may flow freely between asm.js code and external JavaScript code.

The dark boxes represent types that are disallowed from escaping into external (i.e., non-asm.js) JavaScript code. (These values can be given efficient, unboxed representations in optimized asm.js implementations that would be unsound if they were allowed to escape.)

The meta-variables σ and τ are used to stand for value types.

#### 2.1.1 void

The `void` type is the type of functions that are not supposed to return any useful value. As JavaScript functions, they produce the `undefined` value, but asm.js code is not allowed to make use of this value; functions with return type [`void`](#void-0) can only be called for effect.

#### 2.1.2 double

The `double` type is the type of ordinary JavaScript double-precision floating-point numbers.

#### 2.1.3 signed

The `signed` type is the type of signed 32-bit integers. While there is no direct concept of integers in JavaScript, 32-bit integers can be represented as doubles, and integer operations can be performed with JavaScript arithmetic, relational, and bitwise operators.

#### 2.1.4 unsigned

The `unsigned` type is the type of unsigned 32-bit integers. Again, these are not a first-class concept in JavaScript, but can be represented as floating-point numbers.

#### 2.1.5 int

The `int` type is the type of 32-bit integers where the signedness is not known. In asm.js, the type of a variable never has a known signedness. This allows them to be compiled as 32-bit integer registers and memory words. However, this representation creates an overlap between signed and unsigned numbers that causes an ambiguity in determining which JavaScript number they represent. For example, the bit pattern `0xffffffff` could represent 4294967295 or -1, depending on the signedness. For this reason, values of the [`int`](#int-0) type are disallowed from escaping into external (non-asm.js) JavaScript code.

#### 2.1.6 fixnum

The `fixnum` type is the type of integers in the range [0, 2<sup>31</sup>)—that is, the range of integers such that an unboxed 32-bit representation has the same value whether it is interpreted as signed or unsigned.

#### 2.1.7 intish <a name="intish-0"></a>

Even though JavaScript only supports floating-point arithmetic, most operations can simulate integer arithmetic by coercing their result to an integer. For example, adding two integers may overflow beyond the 32-bit range, but coercing the result back to an integer produces the same 32-bit integer as integer addition in, say, C.

The `intish` type represents the result of a JavaScript integer operation that must be coerced back to an integer with an explicit coercion ([*ToInt32*](http://ecma-international.org/ecma-262/5.1/#sec-9.5) for signed integers and [*ToUint32*](http://ecma-international.org/ecma-262/5.1/#sec-9.6) for unsigned integers). Validation requires all [`intish`](#intish-0) values to be immediately passed to an operator or standard library that performs the appropriate coercion or else dropped via an expression statement. This way, each integer operation can be compiled directly to machine operations.

The one operator that does not support this approach is multiplication. (Multiplying two large integers can result in a large enough double that some lower bits of precision are lost.) So asm.js does not support applying the multiplication operator to integer operands. Instead, the proposed [`Math.imul`](https://mail.mozilla.org/pipermail/es-discuss/2012-November/026126.html) function is recommended as the proper means of implementing integer multiplication.


JavaScriptが唯一の浮動小数点演算をサポートしているにもかかわらず、ほとんどの操作は、整数に彼らの結果を強要することにより整数演算をシミュレートすることができます。
例えば、32ビットの範囲を超えてオーバーフローすることができる2つの整数を追加するが、整数に結果を強要すると、C.、たとえば、整数加算と同じ32ビットの整数を生成する

`intish`タイプは、明示的な強制（[*ToInt32*](http://ecma-international.org/ecma-262/5.1/#sec-9.5) バック整数に強制変換されなければならないJavaScriptの整数演算の結果を表している)符号付き整数と [*ToUint32*](http://ecma-international.org/ecma-262/5.1/#sec-9.6) 用の符号なし整数の場合）。
検証は、すべての[`intish`](＃intish-0)の値がすぐに適切な強制を行い、あるいは式文を経由してドロップされたオペレータや標準ライブラリに渡すことが必要です。このように、各整数演算は、機械の動作に直接コンパイルすることができる。

このアプローチをサポートしていない1演算子は乗算である。 asm.jsがオペランドを整数に乗算演算子を適用してサポートしていないので（二つの大きな整数を乗算すると、精度のいくつかの下位ビットが失われていることを倍増する。十分な大きさになることができます）。その代わりに、提案された [`Math.imul`](https://mail.mozilla.org/pipermail/es-discuss/2012-November/026126.html) 関数は、整数の乗算を実現する適切な手段として推奨されている。

#### 2.1.8 double?

The `double?` type represents operations that are expected to produce a [`double`](#double-1) but may also produce `undefined`, and so must be coerced back to a number via [*ToNumber*](http://ecma-international.org/ecma-262/5.1/#sec-9.3). Specifically, reading out of bounds from a typed array produces `undefined`.

#### 2.1.9 float

The `float` type is the type of 32-bit floating-point numbers.

#### 2.1.10 float?

The `float?` type represents operations that are expected to produce a [`float`](#float-1) but, similar to [`double?`](#double-2), may also produce `undefined` and so must be coerced back to a 32-bit floating point number via [*fround*](http://people.mozilla.org/~jorendorff/es6-draft.html#sec-math.fround). Specifically, reading out of bounds from a typed array produces `undefined`.

#### 2.1.11 floatish

Similar to integers, JavaScript can almost support 32-bit floating-point arithmetic, but requires extra coercions to properly emulate the 32-bit semantics. As proved in *[When is double rounding innocuous?](http://dl.acm.org/citation.cfm?id=221334)* (Figueroa 1995), both the 32- and 64-bit versions of standard arithmetic operations produce equivalent results when given 32-bit inputs and coerced to 32-bit outputs.

The `floatish` type, like [`intish`](#intish-0), represents the result of a JavaScript 32-bit floating-point operations that must be coerced back to a 32-bit floating-point value with an explicit [*fround*](http://people.mozilla.org/~jorendorff/es6-draft.html#sec-math.fround) coercion. Validation requires all [`floatish`](#floatish-0) values to be immediately passed to an operator or standard library that performs the appropriate coercion or else dropped via an expression statement. This way, each 32-bit floating-point operation can be [compiled directly to machine operations](https://blog.mozilla.org/javascript/2013/11/07/efficient-float32-arithmetic-in-javascript/).

#### 2.1.12 extern

The abstract `extern` type represents the root of all types that can escape back into external JavaScript—in other words, the light boxes in the above diagram.

### 2.2 Global Types

Variables and functions defined at the top-level scope of an asm.js module can have additional types beyond the [value types](#value-types). These include:

-   value types τ;
-   [`ArrayBufferView`](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/ArrayBufferView) types `IntnArray`, `UintnArray`, and `FloatnArray`;
-   function types ((σ, …) → τ) ∧ … ∧ ((σ′, …) → τ′);
-   variadic function types ((σ, σ`…`) → τ) ∧ … ∧ ((σ′, σ′`…`) → τ′);
-   function table types ((σ, …) → τ)[*n*];
-   the special type `fround` of `Math.fround`; and
-   the FFI function type `Function`.

The "∧" notation for function types serves to represent overloaded functions and operators. For example, the [`Math.abs` function](#standard-library) is overloaded to accept either integers or floating-point numbers, and returns a different type in each case. Similarly, many of the [operators](#operators) have overloaded types.

The meta-variable γ is used to stand for global types.

3 Environments
--------------

Validating an asm.js module depends on tracking contextual information about the set of definitions and variables in scope. This section defines the environments used by the validation logic.

### 3.1 Global Environment

An asm.js module is validated in the context of a global environment. The global environment maps each global variable to its type as well as indicating whether the variable is mutable:

{ *x* : (`mut|imm`) γ, … }

The meta-variable Δ is used to stand for a global environment.

### 3.2 Variable Environment

In addition to the [global environment](#global-environment-0), each function body in an asm.js module is validated in the context of a variable environment. The variable environment maps each function parameter and local variable to its value type:

{ *x* : τ, … }

The meta-variable Γ is used to stand for a variable environment.

### 3.3 Environment Lookup

Looking up a variable's type

*Lookup*(Δ, Γ, *x*)

is defined by:

-   τ if *x* : τ occurs in Γ;
-   γ if *x* does not occur in Γ and *x* : `mut` γ or *x* : `imm` γ occurs in Δ

If *x* does not occur in either environment then the *Lookup* function has no result.

4 Syntax
--------

Validation of an asm.js module is specified by reference to the [ECMAScript grammar](http://ecma-international.org/ecma-262/5.1/#sec-A), but conceptually operates at the level of abstract syntax. In particular, an asm.js validator must obey the following rules:

-   Empty statements (`;`) are always ignored, whether in the top level of a module or inside an asm.js function body.
-   No variables bound anywhere in an asm.js module (whether in the module function parameter list, global variable declarations, asm.js function names, asm.js function parameters, or local variable declarations) may have the name `eval` or `arguments`.
-   Where it would otherwise parse equivalently in JavaScript, parentheses are meaningless. Even where the specification matches on specific productions of *Expression* such as literals, the source may contain extra meaningless parentheses without affecting validation.
-   Automatic semicolon insertion is respected. An asm.js source file may omit semicolons wherever JavaScript allows them to be omitted.

These rules are otherwise left implicit in the rest of the specification.

5 Annotations
-------------

All variables in asm.js are explicitly annotated with type information so that their type can be statically enforced by validation.

### 5.1 Parameter Type Annotations

Every parameter in an asm.js function is provided with an explicit type annotation in the form of a coercion. This coercion serves two purposes: the first is to make the parameter type statically apparent for validation; the second is to ensure that if the function is exported, the arguments dynamically provided by external JavaScript callers are coerced to the expected type. For example, a bitwise OR coercion annotates a parameter as having type [`int`](#int-0):

    function add1(x) {
        x = x|0; // x : int
        return (x+1)|0;
    }

In an AOT implementation, the body of the function can be implemented fully optimized, and the function can be given two entry points: an internal entry point for asm.js callers, which are statically known to provide the proper type, and an external dynamic entry point for JavaScript callers, which must perform the full coercions (which might involve arbitrary JavaScript computation, e.g., via implicit calls to `valueOf`).

There are three recognized parameter type annotations:

*x:Identifier* `=` *x:Identifier*`|0;`
 *x:Identifier* `=` `+`*x:Identifier*`;`
 *x:Identifier* `=` *f:Identifier*`(`*x:Identifier*`);`

The first form annotates a parameter as type [`int`](#int-0), the second as type [`double`](#double-1), and the third as type [`float`](#float-1). In the latter case, *Lookup*(Δ, Γ, *f*) must be `fround`.

### 5.2 Return Type Annotations

An asm.js function's *formal return type* is determined by the last statement in the function body, which for non-[`void`](#void-0) functions is required to be a *ReturnStatement*. This distinguished return statement may take one of five forms:

`return +`*e:Expression*`;`
 `return `*e:Expression*`|0;`
 `return `*n:*`-`?*NumericLiteral*`;`
 `return `*f:Identifier*`(`*arg:Expression*`);`
 `return;`

The first form has return type [`double`](#double-1). The second has type [`signed`](#signed-0). The third has return type [`double`](#double-1) if *n* is composed of a floating-point literal, i.e., a numeric literal with the character `.` in its source; alternatively, if *n* is composed of an integer literal and has its value in the range [-2<sup>31</sup>, 2<sup>31</sup>), the return statement has return type [`signed`](#signed-0). The fourth form has return type [`float`](#float-1), and the fifth has return type [`void`](#void-0).

If the last statement in the function body is not a *ReturnStatement*, or if the function body has no non-empty statements (other than the initial declarations and coercions—see [Function Declarations](#function-declarations)), the function's return type is [`void`](#void-0).

### 5.3 Function Type Annotations

The type of a function declaration

`function` *f:Identifier*`(`*x:Identifier*…`) {`
 `    `(*x:Identifier* `=` *AssignmentExpression*`;`)…
 `    `(`var `(*y:Identifier* `=` (`-`?*NumericLiteral* | *Identifier*`(``-`?*NumericLiteral*`)`))`,`…)…
 `    `*body:Statement*…
 `}`

is (σ,…) → τ where σ,… are the types of the parameters, as provided by the [parameter type annotations](#parameter-type-annotations), and τ is the formal return type, as provided by the [return type annotation](#return-type-annotations). The variable *f* is stored in the [global environment](#global-environment) with type `imm` (σ,…) → τ.

### 5.4 Variable Type Annotations

The types of variable declarations are determined by their initializer, which may take one of two forms:

*n:*`-`?*NumericLiteral*
 *f:Identifier*`(`*n:*`-`?*NumericLiteral*`)`

In the first case, the variable type is [`double`](#double-1) if *n*'s source contains the character `.`; otherwise *n* may be an integer literal in the range [-2<sup>31</sup>, 2<sup>32</sup>), in which case the variable type is [`int`](#int-0).

In the second case, the variable type is [`float`](#float-1). *Lookup*(Δ, Γ, *f*) must be `fround` and *n* must be a floating-point literal with the character `.` in its source.

### 5.5 Global Variable Type Annotations

A global variable declaration is a *VariableStatement* node in one of several allowed forms. Validating global variable annotations takes a Δ as input and produces as output a new Δ′ by adding the variable binding to Δ.

A global program variable is initialized to a literal:

`var` *x:Identifier* `=` *n:*`-`?*NumericLiteral*`;`
 `var` *x:Identifier* `=` *f:Identifier*`(`*n:*`-`?*NumericLiteral*`);`

The global variable *x* is stored in the [global environment](#global-environment) with type `mut` τ, where τ is determined in the same way as local [variable type annotations](#variable-type-annotations).

A standard library import is of one of the following two forms:

`var` *x:Identifier* `=` *stdlib:Identifier*`.`*y:Identifier*`;`
 `var` *x:Identifier* `=` *stdlib:Identifier*`.Math.`*y:Identifier*`;`

The variable *stdlib* must match the first parameter of the [module declaration](#modules). The global variable *x* is stored in the [global environment](#global-environment) with type `imm` γ, where γ is the type of library *y* or `Math.`*y* as specified by the [standard library types](#standard-library).

A foreign import is of one of the following three forms:

`var` *x:Identifier* `=` *foreign:Identifier*`.`*y:Identifier*`;`
 `var` *x:Identifier* `=` *foreign:Identifier*`.`*y:Identifier*`|0;`
 `var` *x:Identifier* `=` `+`*foreign:Identifier*`.`*y:Identifier*`;`

The variable *foreign* must match the second parameter of the [module declaration](#modules). The global variable *x* is stored in the [global environment](#global-environment) with type `imm Function` for the first form, `mut int` for the second, and `mut double` for the third.

A global heap view is of the following form:

`var` *x:Identifier* `= new` *stdlib:Identifier*`.`*view:Identifier*`(`*heap:Identifier*`);`

The variable *stdlib* must match the first parameter of the [module declaration](#modules) and the variable *heap* must match the third. The identifier *view* must be one of the standard [`ArrayBufferView`](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/ArrayBufferView) type names. The global variable *x* is stored in the [global environment](#global-environment) with type `imm` *view*.

### 5.6 Function Table Types

A function table is a *VariableStatement* of the form:

`var` *x:Identifier*` = [`*f<sub>0</sub>:Identifier*`,` *f:Identifier*`,`…`];`

The function table *x* is stored in the [global environment](#global-environment) with type `imm` ((σ,…) → τ)[*n*] where (σ,…) → τ is the type of *f* in the global environment and *n* is the length of the array literal.

6 Validation Rules
------------------

To ensure that a JavaScript function is a proper asm.js module, it must first be statically validated. This section specifies the validation rules. The rules operate on JavaScript abstract syntax, i.e., the output of a JavaScript parser. The non-terminals refer to parse nodes defined by productions in the [ECMAScript grammar](http://ecma-international.org/ecma-262/5.1/#sec-A), but note that the asm.js validator only accepts a subset of legal JavaScript programs.

The result of a validation operation is either a success, indicating that a parse node is statically valid asm.js, or a failure, indicating that the parse node is statically invalid asm.js.

### 6.1 *ValidateModule*(*f*)

The *ValidateModule* rule validates an asm.js module, which is either a *FunctionDeclaration* or *FunctionExpression* node.

Validating a module of the form

`function` *f:Identifier<sub>opt</sub>*`(`(*stdlib:Identifier*`(,` *foreign:Identifier*(`,` *heap:Identifier*)<sub>*opt*</sub>)<sub>*opt*</sub>)<sub>*opt*</sub>`) {     "use asm";`
 `    `*var:VariableStatement*…
 `    `*fun:FunctionDeclaration*…
 `    `*table:VariableStatement*…
 `    `*exports:ReturnStatement*
 `}`

succeeds if:

-   *f*, *stdlib*, *foreign*, *heap*, and the *var*, *fun*, and *table* variables are all mutually distinct;
-   the global environment Δ is constructed in three stages:
    1.  the [global declarations](#global-declarations) are validated in an empty initial environment Δ<sub>0</sub>, producing a new global environment Δ<sub>1</sub>;
    2.  the types from the [function type annotations](#function-type-annotations) in the *fun* declarations are extracted using Δ<sub>1</sub>, and then added to Δ<sub>1</sub> to produce Δ<sub>2</sub>;
    3.  the types of the [function tables](#function-table-types) in the *table* declarations are extracted using Δ<sub>2</sub>, and their types are added to Δ<sub>2</sub> to produce the completed global type environment Δ.
-   for each *fun* declaration, [*ValidateFunction*](#validatefunction-f) succeeds with environment Δ;
-   for each *table* declaration, [*ValidateFunctionTable*](#validatefunctiontable-s) succeeds with environment Δ; and
-   [*ValidateExport*](#validateexport-s) succeeds for *exports* with environment Δ.

### 6.2 *ValidateExport*(Δ, *s*)

The *ValidateExport* rule validates an asm.js module's export declaration. An export declaration is a *ReturnStatement* returning either a single asm.js function or an object literal exporting multiple asm.js functions.

Validating an export declaration node

`return` `{` (*x:Identifier* `:` *f:Identifier*)`,`… ` };`

succeeds if for each *f*, Δ(f) = `imm` γ where γ is a function type (σ,…) → τ.

Validating an export declaration node

`return` *f:Identifier*`;`

succeeds if Δ(*f*) = `imm` γ where γ is a function type (σ,…) → τ.

### 6.3 *ValidateFunctionTable*(Δ, *s*)

The *ValidateFunctionTable* rule validates an asm.js module's function table declaration. A function table declaration is a *VariableStatement* binding an identifier to an array literal.

Validating a function table of the form

`var` *x:Identifier*` = [`*f:Identifier*`,`…`];`

succeeds if:

-   the length *n* of the array literal is 2<sup>*m*</sup> for some *m* ≥ 0;
-   Δ(*x*) = `imm` ((σ,…) → τ)[*n*]; and
-   for each *f*, Δ(*f*) = (σ,…) → τ.

### 6.4 *ValidateFunction*(Δ, *f*)

The *ValidateFunction* rule validates an asm.js function declaration, which is a *FunctionDeclaration* node.

Validating a function declaration of the form

`function` *f:Identifier*`(`*x:Identifier*`,`…`) {`
 `    `(*x:Identifier* `=` *AssignmentExpression*`;`)…
 `    `(`var `(*y:Identifier* `=` (`-`?*NumericLiteral* | *Identifier*`(``-`?*NumericLiteral*`)`))`,`…)…
 `    `*body:Statement*…
 `}`

succeeds if:

-   Δ(f) = `imm` (σ,…) → τ;
-   the *x* and *y* variables are all mutually distinct;
-   the variable environment Γ is constructed by mapping each parameter *x* to its corresponding [parameter type annotation](#parameter-type-annotations) (annotations must appear in the same order as the parameters) and each local variable *y* to its [variable type annotation](#variable-type-annotations);
-   for each *body* statement, [*ValidateStatement*](#validatestatement-s) succeeds with environments Δ and Γ and expected return type τ.

### 6.5 *ValidateStatement*(Δ, Γ, τ, *s*)

The *ValidateStatement* rule validates an asm.js statement. Each statement is validated in the context of a [global environment](#global-environment-0) Δ, a [variable environment](#variable-environment-0) Γ, and an expected return type τ. Unless otherwise explicitly stated, a recursive validation of a subterm uses the same context as its containing term.

#### 6.5.1 Block

Validating a *Block* statement node

`{` *stmt:Statement*… `}`

succeeds if [*ValidateStatement*](#validatestatement-s) succeeds for each *stmt*.

#### 6.5.2 ExpressionStatement

Validating an *ExpressionStatement* node

*cexpr:CallExpression* `;`

succeeds if [*ValidateCall*](#validatecall-e) succeeds for *cexpr* with actual return type [`void`](#void-0).

Validating an *ExpressionStatement* node

*expr:Expression* `;`

succeeds if [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with some type σ.

#### 6.5.3 EmptyStatement

Validating an *EmptyStatement* node always succeeds.

#### 6.5.4 IfStatement

Validating an *IfStatement* node

`if (` *expr:Expression* `)` *stmt<sub>1</sub>:Statement* `else` *stmt<sub>2</sub>:Statement*

succeeds if [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with a subtype of [`int`](#int-0) and [*ValidateStatement*](#validatestatement-s) succeeds for *stmt<sub>1</sub>* and *stmt<sub>2</sub>*.

Validating an *IfStatement* node

`if (` *expr:Expression* `)` *stmt:Statement*

succeeds if [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with a subtype of [`int`](#int-0) and [*ValidateStatement*](#validatestatement-s) succeeds for *stmt*.

#### 6.5.5 ReturnStatement

Validating a *ReturnStatement* node

`return` *expr:Expression* `;`

succeeds if [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with a subtype of the expected return type τ.

Validating a *ReturnStatement* node

`return ;`

succeeds if the expected return type τ is [`void`](#void-0).

#### 6.5.6 IterationStatement

Validating an *IterationStatement* node

`while (` *expr:Expression* `)` *stmt:Statement*

succeeds if [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with a subtype of [`int`](#int-0) and [*ValidateStatement*](#validatestatement-s) succeeds for *stmt*.

Validating an *IterationStatement* node

`do` *stmt:Statement* `while (` *expr:Expression* `) ;`

succeeds if [*ValidateStatement*](#validatestatement-s) succeeds for *stmt* and [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with a subtype of [`int`](#int-0).

Validate an *IterationStatement* node

`for (` *init:ExpressionNoIn<sub>opt</sub>* `;` *test:Expression<sub>opt</sub>* `;` *update:Expression<sub>opt</sub>* `)` *body:Statement*

succeeds if:

-   [*ValidateExpression*](#validateexpression-e) succeeds for *init* (if present);
-   [*ValidateExpression*](#validateexpression-e) succeeds for *test* with a subtype of [`int`](#int-0) (if present);
-   [*ValidateExpression*](#validateexpression-e) succeeds for *update* (if present); and
-   [*ValidateStatement*](#validatestatement-s) succeeds for *body*.

#### 6.5.7 BreakStatement

Validating a *BreakStatement* node

`break` *Identifier<sub>opt</sub>* `;`

always succeeds.

#### 6.5.8 ContinueStatement

Validating a *ContinueStatement* node

`continue` *Identifier<sub>opt</sub>* `;`

always succeeds.

#### 6.5.9 LabelledStatement

Validating a *LabelledStatement* node

*Identifier* `:` *body:Statement*

succeeds if [*ValidateStatement*](#validatestatement-s) succeeds for *body*.

#### 6.5.10 SwitchStatement

Validating a *SwitchStatement* node

`switch (` *test:Expression* `)` `{` *case:CaseClause*… *default:DefaultClause<sub>opt</sub>* `}`

succeeds if

-   [*ValidateExpression*](#validateexpression-e) succeeds for *test* with a subtype of [`signed`](#signed-0);
-   [*ValidateCase*](#validatecase-c) succeeds for each *case*;
-   each *case* value is distinct;
-   the difference between the maximum and minimum *case* values is less than 2<sup>31</sup>; and
-   [*ValidateDefault*](#validatedefault-d) succeeds for *default*.

### 6.6 *ValidateCase*(Δ, Γ, τ, *c*)

Cases in a `switch` block are validated in the context of a [global environment](#global-environment-0) Δ, a [variable environment](#variable-environment-0) Γ, and an expected return type τ. Unless otherwise explicitly stated, a recursive validation of a subterm uses the same context as its containing term.

Validating a *CaseClause* node

`case` *n:*`-`?*NumericLiteral* `:` *stmt:Statement*…

succeeds if

-   the source of *n* does not contain a `.` character;
-   *n* is in the range [-2<sup>31</sup>, 2<sup>31</sup>); and
-   [*ValidateStatement*](#validatestatement-s) succeeds for each *stmt*.

### 6.7 *ValidateDefault*(Δ, Γ, τ, *d*)

The default case in a `switch` block is validated in the context of a [global environment](#global-environment-0) Δ, a [variable environment](#variable-environment-0) Γ, and an expected return type τ. Unless otherwise explicitly stated, a recursive validation of a subterm uses the same context as its containing term.

Validating a *DefaultClause* node

`default :` *stmt:Statement*…

succeeds if [*ValidateStatement*](#validatestatement-s) succeeds for each *stmt*.

### 6.8 *ValidateExpression*(Δ, Γ, *e*)

Each expression is validated in the context of a [global environment](#global-environment-0) Δ and a [variable environment](#variable-environment-0) Γ, and validation produces the type of the expression as a result. Unless otherwise explicitly stated, a recursive validation of a subterm uses the same context as its containing term.

#### 6.8.1 Expression

Validating an *Expression* node

*expr<sub>1</sub>:AssignmentExpression* `,` … `,` *expr<sub>n</sub>:AssignmentExpression*

succeeds with type τ if for every *i* \< *n*, one of the following conditions holds:

-   *expr<sub>i</sub>* is a *CallExpression* and [*ValidateCall*](#validatecall-e) succeeds with actual return type [`void`](#void-0); or
-   [*ValidateExpression*](#validateexpression-e) succeeds for *expr<sub>i</sub>* with some type σ;

and [*ValidateExpression*](#validateexpression-e) succeeds for *expr<sub>n</sub>* with type τ.

#### 6.8.2 NumericLiteral

Validating a *NumericLiteral* node

-   succeeds with type [`double`](#double-1) if the source contains a `.` character; or validates as type [`double`](#double-1);
-   succeeds with type [`fixnum`](#fixnum-0) if the source does not contain a `.` character and its numeric value is in the range [0, 2<sup>31</sup>); or
-   succeeds with type [`unsigned`](#unsigned-0) if the source does not contain a `.` character and its numeric value is in the range [2<sup>31</sup>, 2<sup>32</sup>).

Note that the case of negative integer constants is handled under [*UnaryExpression*](#unaryexpression).

Note that integer literals outside the range [0, 2<sup>32</sup>) are invalid, i.e., fail to validate.

#### 6.8.3 Identifier

Validating an *Identifier* node

*x:Identifier*

succeeds with type τ if *Lookup*(Δ, Γ, *x*) = τ.

#### 6.8.4 CallExpression

Validating a *CallExpression* node succeeds with type [`float`](#float-1) if [*ValidateFloatCoercion*](#validatefloatcoercion-e) succeeds.

#### 6.8.5 MemberExpression

Validating a *MemberExpression* node succeeds with type τ if [*ValidateHeapAccess*](#validateheapaccess-e) succeeds with load type τ.

#### 6.8.6 AssignmentExpression

Validating an *AssignmentExpression* node

*x:Identifier* `=` *expr:AssignmentExpression*

succeeds with type τ if [*ValidateExpression*](#validateexpression-e) succeeds for the nested *AssignmentExpression* with type τ and one of the following two conditions holds:

-   *x* is bound in Γ as a supertype of τ; or
-   *x* is not bound in Γ and is bound to a mutable supertype of τ in Δ.

Validating an *AssignmentExpression* node

*lhs:MemberExpression* `=` *rhs:AssignmentExpression*

succeeds with type τ if [*ValidateExpression*](#validateexpression-e) succeeds for *rhs* with type τ and [*ValidateHeapAccess*](#validateheapaccess-e) succeeds for *lhs* with τ as one of its legal store types.

#### 6.8.7 UnaryExpression

Validating a *UnaryExpression* node of the form

`-`*NumericLiteral*

succeeds with type [`signed`](#signed-0) if the *NumericLiteral* source does not contain a `.` character and the numeric value of the expression is in the range [-2<sup>31</sup>, 0).

Validating a *UnaryExpression* node of the form

`+`*cexpr:CallExpression*

succeeds with type [`double`](#double-1) if [*ValidateCall*](#validatecall-e) succeeds for *cexpr* with actual return type [`double`](#double-1).

Validating a *UnaryExpression* node of the form

*op:*(`+`|`-`|`~`|`!`)*arg:UnaryExpression*

succeeds with type τ if the type of *op* is … ∧ (σ) → τ ∧ … and [*ValidateExpression*](#validateexpression-e) succeeds with a subtype of σ.

Validating a *UnaryExpression* node of the form

`~~`*arg:UnaryExpression*

succeeds with type [`signed`](#signed-0) if [*ValidateExpression*](#validateexpression-e) succeeds for *arg* with a subtype of either [`double`](#double-1) or [`float?`](#float-2).

#### 6.8.8 MultiplicativeExpression

Validating a *MultiplicativeExpression* node

*lhs:MultiplicativeExpression* *op:*(`*`|`/`|`%`) *rhs:UnaryExpression*

succeeds with type τ if:

-   the [binary operator type](#binary-operators) of *op* is … ∧ (σ<sub>1</sub>, σ<sub>2</sub>) → τ ∧ …;
-   [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* with a subtype of σ<sub>1</sub>; and
-   [*ValidateExpression*](#validateexpression-e) succeeds for *rhs* with a subtype of σ<sub>2</sub>.

Validating a *MultiplicativeExpression* node

*expr:MultiplicativeExpression* `*` *n:*`-`?*NumericLiteral*
 *n:*`-`?*NumericLiteral* `*` *expr:UnaryExpression*

succeeds with type [`intish`](#intish-0) if the source of *n* does not contain a `.` character and -2<sup>20</sup> \< *n* \< 2<sup>20</sup> and [*ValidateExpression*](#validateexpression-e)expr with a subtype of [`int`](#int-0).

#### 6.8.9 AdditiveExpression

Validating an *AdditiveExpression* node

*expr<sub>1</sub>* (`+`|`-`) … (`+`|`-`) *expr<sub>n</sub>*

succeeds with type [`intish`](#intish-0) if:

-   [*ValidateExpression*](#validateexpression-e) succeeds for each *expr<sub>i</sub>* with a subtype of [`int`](#int-0);
-   *n* ≤ 2<sup>20</sup>.

Otherwise, validating an *AdditiveExpression* node

*lhs:AdditiveExpression* *op:*(`+`|`-`) *rhs:MultiplicativeExpression*

succeeds with type [`double`](#double-1) if:

-   the [binary operator type](#binary-operators) of *op* is (σ<sub>1</sub>, σ<sub>2</sub>) → [`double`](#double-1);
-   [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* with a subtype of σ<sub>1</sub>; and
-   [*ValidateExpression*](#validateexpression-e) succeeds for *rhs* with a subtype of σ<sub>2</sub>.

#### 6.8.10 ShiftExpression

Validating a *ShiftExpression* node

*lhs:ShiftExpression* *op:*(`<<`|`>>`|`>>>`) *rhs:AdditiveExpression*

succeeds with type τ if

-   the [binary operator type](#binary-operators) of *op* is … ∧ (σ<sub>1</sub>, σ<sub>2</sub>) → τ ∧ …;
-   [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* with a subtype of σ<sub>1</sub>; and
-   [*ValidateExpression*](#validateexpression-e) succeeds for *rhs* with a subtype of σ<sub>2</sub>.

#### 6.8.11 RelationalExpression

Validating a *RelationalExpression* node

*lhs:RelationalExpression* *op:*(`<`|`>`|`<=`|`>=`) *rhs:ShiftExpression*

succeeds with type τ if

-   the [binary operator type](#binary-operators) of *op* is … ∧ (σ<sub>1</sub>, σ<sub>2</sub>) → τ ∧ …;
-   [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* with a subtype of σ<sub>1</sub>; and
-   [*ValidateExpression*](#validateexpression-e) succeeds for *rhs* with a subtype of σ<sub>2</sub>.

#### 6.8.12 EqualityExpression

Validating an *EqualityExpression* node

*lhs:EqualityExpression* *op:*(`==`|`!=`) *rhs:RelationalExpression*

succeeds with type τ if

-   the [binary operator type](#binary-operators) of *op* is … ∧ (σ<sub>1</sub>, σ<sub>2</sub>) → τ ∧ …;
-   [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* with a subtype of σ<sub>1</sub>; and
-   [*ValidateExpression*](#validateexpression-e) succeeds for *rhs* with a subtype of σ<sub>2</sub>.

#### 6.8.13 BitwiseANDExpression

Validating a *BitwiseANDExpression* node

*lhs:BitwiseANDExpression* `&` *rhs:EqualityExpression*

succeeds with type [`signed`](#signed-0) if [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* and *rhs* with a subtype of [`intish`](#intish-0).

#### 6.8.14 BitwiseXORExpression

Validating a *BitwiseXORExpression* node

*lhs:BitwiseXORExpression* `^` *rhs:BitwiseANDExpression*

succeeds with type [`signed`](#signed-0) if [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* and *rhs* with a subtype of [`intish`](#intish-0).

#### 6.8.15 BitwiseORExpression

Validating a *BitwiseORExpression* node

*cexpr:CallExpression* `|0`

succeeds with type [`signed`](#signed-0) if [*ValidateCall*](#validatecall-e) succeeds for *cexpr* with actual return type [`signed`](#signed-0).

Validating a *BitwiseORExpression* node

*lhs:BitwiseORExpression* `|` *rhs:BitwiseXORExpression*

succeeds with type [`signed`](#signed-0) if [*ValidateExpression*](#validateexpression-e) succeeds for *lhs* and *rhs* with a subtype of [`intish`](#intish-0).

#### 6.8.16 ConditionalExpression

Validating a *ConditionalExpression* node

*test:BitwiseORExpression* `?` *cons:AssignmentExpression* `:` *alt:AssignmentExpression*

succeeds with type τ if:

-   τ is one of [`int`](#int-0), [`double`](#double-1), or [`float`](#float-1);
-   [*ValidateExpression*](#validateexpression-e) succeeds for *test* with a subtype of [`int`](#int-0);
-   [*ValidateExpression*](#validateexpression-e) succeeds for *cons* and *alt* with subtypes of τ.

#### 6.8.17 Parenthesized Expression

Validating a parenthesized expression node

`(` *expr:Expression* `)`

succeeds with type τ if [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with type τ.

### 6.9 *ValidateCall*(Δ, Γ, τ, *e*)

Each function call expression is validated in the context of a global environment Δ and a variable environment Γ, and validates against an *actual return type* τ, which was provided from the context in which the function call appears. A recursive validation of a subterm uses the same context as its containing term.

Validating a *CallExpression* node

*f:Identifier*`(`*arg:Expression*`,`…`)`

with actual return type τ succeeds if one of the following conditions holds:

-   [*ValidateFloatCoercion*](#validatefloatcoercion-e) succeeds for the node;
-   *Lookup*(Δ, Γ, *f*) = … ∧ (σ,…) → τ ∧ … and [*ValidateExpression*](#validateexpression-e) succeeds for each *arg* with a subtype of its corresponding σ; or
-   *Lookup*(Δ, Γ, *f*) = … ∧ (σ<sub>1</sub>,…,σ<sub>*n*</sub>,σ`…`) → τ ∧ … and [*ValidateExpression*](#validateexpression-e) succeeds for the first *n* *arg<sub>i</sub>* expressions with subtypes of their corresponding σ<sub>*i*</sub> and the remaining *arg* expressions with subtypes of σ.

Alternatively, validating the *CallExpression* succeeds with any actual return type τ other than [`float`](#float-1) if *Lookup*(Δ, Γ, *f*) = `Function` and [*ValidateExpression*](#validateexpression-e) succeeds for each *arg* with a subtype of [`extern`](#extern-0).

Validating a *CallExpression* node

*x:Identifier*`[`*index:Expression*` & `*n:*`-`?*NumericLiteral*`](`*arg:Expression*`,`…`)`

succeeds with actual return type τ if:

-   the source of *n* does not contain a `.` character;
-   *Lookup*(Δ, Γ, *x*) = ((σ,…) → τ)[*n+1*];
-   [*ValidateExpression*](#validateexpression-e) succeeds for *index* with a subtype of [`intish`](#intish-0); and
-   [*ValidateExpression*](#validateexpression-e) succeeds for each *arg* with a subtype of its corresponding σ.

### 6.10 *ValidateHeapAccess*(Δ, Γ, *e*)

Each heap access expression is validated in the context of a global environment Δ and a variable environment Γ, and validation produces a *load type* as well as a set of legal *store types* as a result. These types are determined by the [heap view types](#heap-view-types) corresponding to each [`ArrayBufferView`](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/ArrayBufferView) type.

Validating a *MemberExpression* node

*x:Identifier*`[`*n:*`-`?*NumericLiteral*`]`

succeeds with load type σ and store types { τ, … } if:

-   *Lookup*(Δ, Γ, *x*) = *view* where *view* is an [`ArrayBufferView`](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/ArrayBufferView) type;
-   the load type of *view* is σ;
-   the store types of *view* are { τ, … };
-   the source of *n* does not contain a `.` character;
-   0 ≤ *n* \< 2<sup>32</sup>.

Validating a *MemberExpression* node

*x:Identifier*`[`*expr:Expression* `>>` *n:*`-`?*NumericLiteral*`]`

succeeds with load type σ and store types { τ, … } if:

-   *Lookup*(Δ, Γ, *x*) = *view* where *view* is an [`ArrayBufferView`](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/ArrayBufferView) type;
-   the element size of *view* is *bytes*;
-   the load type of *view* is σ;
-   the store types of *view* are { τ, … };
-   [*ValidateExpression*](#validateexpression-e) succeeds for *expr* with type [`intish`](#intish-0);
-   the source of *n* does not contain a `.` character;
-   *n* = log<sub>2</sub>(*bytes*).

### 6.11 *ValidateFloatCoercion*(Δ, Γ, *e*)

A call to the `fround` coercion is validated in the context of a global environment Δ and a variable environment Γ and validates as the type [`float`](#float-1).

Validating a *CallExpression* node

*f:Identifier*`(`*cexpr:CallExpression*`)`

succeeds with type [`float`](#float-1) if *Lookup*(Δ, Γ, *f*) = `fround` and [*ValidateCall*](#validatecall-e) succeeds for *cexpr* with actual return type [`float`](#float-1).

Alternatively, validating a *CallExpression* node

*f:Identifier*`(`*arg:Expression*`)`

succeeds with type [`float`](#float-1) if *Lookup*(Δ, Γ, *f*) = `fround` and [*ValidateExpression*](#validateexpression-e) succeeds for *arg* with type τ, where τ is a subtype of [`floatish`](#floatish-0), [`double?`](#double-2), [`signed`](#signed-0), or [`unsigned`](#unsigned-0).

7 Linking
---------

An AOT implementation of asm.js must perform some internal dynamic checks at link time to be able to safely generate AOT-compiled exports. If any of the dynamic checks fails, the result of linking cannot be an AOT-compiled module. The dynamically checked invariants are:

-   control must reach the module's `return` statement without throwing;
-   all property access must resolve to data properties;
-   the *heap* object (if provided) must be an instance of [`ArrayBuffer`](https://developer.mozilla.org/en-US/docs/JavaScript/Typed_arrays/ArrayBuffer);
-   the *heap* object's `byteLength` must be either 2<sup>*n*</sup> for *n* in [12, 24) or 2<sup>24</sup> · *n* for *n* ≥ 1;
-   all globals taken from the *stdlib* object must be the [SameValue](http://ecma-international.org/ecma-262/5.1/#sec-9.12) as the corresponding [standard library](http://ecma-international.org/ecma-262/5.1/#sec-15) of the same name.

If any of these conditions is not met, AOT compilation may produce invalid results so the engine should fall back to an interpreted or JIT-compiled implementation.

8 Operators
-----------

### 8.1 Unary Operators

Unary Operator

Type

`+`

([`signed`](#signed-0)) → [`double`](#double-1) ∧
 ([`unsigned`](#unsigned-0)) → [`double`](#double-1) ∧
 ([`double?`](#double-2)) → [`double`](#double-1) ∧
 ([`float?`](#float-2)) → [`double`](#double-1)

`-`

([`int`](#int-0)) → [`intish`](#intish-0) ∧
 ([`double?`](#double-2)) → [`double`](#double-1) ∧
 ([`float?`](#float-2)) → [`floatish`](#floatish-0)

`~`

([`intish`](#intish-0)) → [`signed`](#signed-0)

`!`

([`int`](#int-0)) → [`int`](#int-0)

Note that the special combined operator `~~` may be used as a coercion from [`double`](#double-1) or [`float?`](#float-2) to [`signed`](#signed-0); see [Unary Expressions](#unaryexpression).

### 8.2 Binary Operators

Binary Operator

Type

`+`

([`double`](#double-1), [`double`](#double-1)) → [`double`](#double-1) ∧
 ([`float?`](#float-2), [`float?`](#float-2)) → [`floatish`](#floatish-0)

`-`

([`double?`](#double-2), [`double?`](#double-2)) → [`double`](#double-1) ∧
 ([`float?`](#float-2), [`float?`](#float-2)) → [`floatish`](#floatish-0)

`*`

([`double?`](#double-2), [`double?`](#double-2)) → [`double`](#double-1) ∧
 ([`float?`](#float-2), [`float?`](#float-2)) → [`floatish`](#floatish-0)

`/`

([`signed`](#signed-0), [`signed`](#signed-0)) → [`intish`](#intish-0) ∧
 ([`unsigned`](#unsigned-0), [`unsigned`](#unsigned-0)) → [`intish`](#intish-0) ∧
 ([`double?`](#double-2), [`double?`](#double-2)) → [`double`](#double-1) ∧
 ([`float?`](#float-2), [`float?`](#float-2)) → [`floatish`](#floatish-0)

`%`

([`signed`](#signed-0), [`signed`](#signed-0)) → [`intish`](#intish-0) ∧
 ([`unsigned`](#unsigned-0), [`unsigned`](#unsigned-0)) → [`intish`](#intish-0) ∧
 ([`double?`](#double-2), [`double?`](#double-2)) → [`double`](#double-1)

`|`, `&`, `^`, `<<`, `>>`

([`intish`](#intish-0), [`intish`](#intish-0)) → [`signed`](#signed-0)

`>>>`

([`intish`](#intish-0), [`intish`](#intish-0)) → [`unsigned`](#unsigned-0)

`<`, `<=`, `>`, `>=`, `==`, `!=`

([`signed`](#signed-0), [`signed`](#signed-0)) → [`int`](#int-0) ∧
 ([`unsigned`](#unsigned-0), [`unsigned`](#unsigned-0)) → [`int`](#int-0) ∧
 ([`double`](#double-1), [`double`](#double-1)) → [`int`](#int-0) ∧
 ([`float`](#float-1), [`float`](#float-1)) → [`int`](#int-0)

9 Standard Library
------------------

Standard Library

Type

`Infinity`
 `NaN`

[`double`](#double-1)

`Math.acos`
 `Math.asin`
 `Math.atan`
 `Math.cos`
 `Math.sin`
 `Math.tan`
 `Math.exp`
 `Math.log`

([`double?`](#double-2)) → [`double`](#double-1)

`Math.ceil`
 `Math.floor`
 `Math.sqrt`

([`double?`](#double-2)) → [`double`](#double-1) ∧
 ([`float?`](#float-2)) → [`float`](#float-1)

`Math.abs`

([`signed`](#signed-0)) → [`signed`](#signed-0) ∧
 ([`double?`](#double-2)) → [`double`](#double-1) ∧
 ([`float?`](#float-2)) → [`float`](#float-1)

`Math.min`
 `Math.max`

([`int`](#int-0), [`int`](#int-0)`…`) → [`signed`](#signed-0) ∧
 ([`double`](#double-1), [`double`](#double-1)`…`) → [`double`](#double-1)

`Math.atan2`
 `Math.pow`

([`double?`](#double-2), [`double?`](#double-2)) → [`double`](#double-1)

`Math.imul`

([`int`](#int-0), [`int`](#int-0)) → [`signed`](#signed-0)

`Math.fround`

`fround`

`Math.E`
 `Math.LN10`
 `Math.LN2`
 `Math.LOG2E`
 `Math.LOG10E`
 `Math.PI`
 `Math.SQRT1_2`
 `Math.SQRT2`

[`double`](#double-1)

10 Heap View Types
------------------

View Type

Element Size (Bytes)

Load Type

Store Types

`Uint8Array`

1

[`intish`](#intish-0)

[`intish`](#intish-0)

`Int8Array`

1

[`intish`](#intish-0)

[`intish`](#intish-0)

`Uint16Array`

2

[`intish`](#intish-0)

[`intish`](#intish-0)

`Int16Array`

2

[`intish`](#intish-0)

[`intish`](#intish-0)

`Uint32Array`

4

[`intish`](#intish-0)

[`intish`](#intish-0)

`Int32Array`

4

[`intish`](#intish-0)

[`intish`](#intish-0)

`Float32Array`

4

[`float?`](#float-2)

[`floatish`](#floatish-0), [`double?`](#double-2)

`Float64Array`

8

[`double?`](#double-2)

[`float?`](#float-2), [`double?`](#double-2)

Acknowledgements
----------------

Thanks to Martin Best, Brendan Eich, Andrew McCreight, and Vlad Vukićević for feedback and encouragement.

Thanks to Benjamin Bouvier, Douglas Crosher, and Dan Gohman for contributions to the design and implementation, particularly for [`float`](#float-1).

Thanks to Jesse Ruderman and C. Scott Ananian for bug reports.

Thanks to Michael Bebenita for diagrams.
