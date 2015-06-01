# ATSLIB / libats / ML / string

このパッケージで宣言された関数は、関数型プログラミングでCスタイルの文字列を処理するために主にあります。

* [itoa](#itoa)
* [string\_sing](#string_sing)
* [string\_copy](#string_copy)
* [string\_make\_substring](#string_make_substring)
* [string\_append](#string_append)
* [stringlst\_concat](#stringlst_concat)
* [string\_explode](#string_explode)
* [string\_implode](#string_implode)
* [string\_tabulate](#string_tabulate)
* [string\_foreach](#string_foreach)
* [オーバーロードのシンボル](#オーバーロードのシンボル)
* [+](#+)

参考URL: [libats_ML_string.dats](https://github.com/githwxi/ATS-Postiats/blob/master/doc/EXAMPLE/ATSLIB/libats_ML_string.dats)

## <a name="itoa"></a>itoa

### 概要

```
fun{
} itoa (x: int):<> string
```

### 説明

この関数は、整数を文字列表現に変換するためのものです。

### 例

```
// itoa.dats
// patscc itoa.dats -DATS_MEMALLOC_LIBC -latslib -o itoa

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"
implement main0 () = {
  val ()  = print(itoa(10)+"\n")
}
```

### 結果

```
10
```


## <a name="string_sing"></a>string\_sing

### 概要

```
fun{
} string_sing (c: charNZ):<> string
```

### 説明

null以外の文字を指定すると、この関数は、文字からなるシングルトン文字列を返します。

## <a name="string_copy"></a>string\_copy

### 概要

```
fun{
} string_copy (x: NSH(string)):<> string
```

### 説明

文字列が与えられると、この関数はそのコピーを返します。

### 例

次のコードをテストstring_copy実際に指定された文字列のコピーを返します。

```
// string_copy.dats
// patscc string_copy.dats -DATS_MEMALLOC_LIBC -latslib -o string_copy

#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"
implement main0 () = { 
  val str = "abcdefg" 
  val ()  = println!(string_copy(str))
}
```

### 出力結果

```
abcdefg
```

## <a name="string_make_substring"></a>string\_make\_substring

### 概要

```
fun{
} string_make_substring
  (x: NSH(string), st: size_t, ln: size_t):<> string
```

### 説明

長さnと整数STとLNの文字列strを考えると、この関数は文字列str [ST]、STR [ST + 1]からなる文字列を返します...、STR [ST +分（N-ST、LN） - 1]番目は、nよりも小さい場合。それ以外の場合は、空の文字列を返します。

### 例

次のコードは、string\_make\_substringの簡単な利用になります：

```
// string_make_substring.dats
// patscc string_make_substring.dats -DATS_MEMALLOC_LIBC -latslib -o string_make_substring
#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

implement main0() = {
  val str = "abcdefg"
  val str2 = string_make_substring (str, g1int2uint(0), string_length(str))
  val () = assertloc (str = str2)
  val () = println!(str2)
}
```

### 結果

```
abcdefg
```

## <a name="string_append"></a>string\_append

### 概要

```
fun{
} string_append
  (x1: NSH(string), x2: NSH(string)):<> string
```

### 説明

シンボルオーバーロードこの機能、+は、与えられた二つの文字列の連結を返します。

### 例

次のコードは、文字列を結合する一般的な方法を示しています。

```
// string_append.dats
// patscc string_append.dats -DATS_MEMALLOC_LIBC -latslib -o string_append
#include "share/atspre_staload.hats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"

implement main0() = {
  val Hello = "H"+"e"+"l"+"l"+"o"
  val () = print (Hello + ", world!\n")
}
```

なお、式の評価 「H」+「e」+「l」+「l」+「o」 以下のすべての中間の文字列が"H"、 "He"、 "Hel"、および"Hell"、生成評価が文字列 "Hello" を返したら、ガーベジになります。

### 結果

```
Hello, world!
```

## <a name="stringlst_concat"></a>stringlst\_concat

### 概要

```
fun{
} stringlst_concat (xs: list0 (string)):<> string
```

### 説明

文字列のリストのXSを考えると、この関数は、xsの連結を返します[0]、XS [1]、,,,, nはXSと表記XSの長さであるXS [N-1]は、[i]の要素を指し、 XSのI。

### 例

次のコードはシングルトン文字列のリストを連結した文字列「こんにちは」を形成する方法を示しています。

```
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/string.sats"

implement main0 () = {
  val Hello = stringlst_concat ((list0)$arrpsz{string}("H","e","l","l","o"))
  val () = print (Hello + ", world!\n")
}
```

を呼び出していることに注意してくださいstringlst_concatは、文字列「こんにちは」を構築することなく、任意の中間部分文字列を生成します。

## <a name="string_explode"></a>string\_explode

### 概要

```
fun{
} string_explode (x: string):<> list0 (char)
```

### 説明

長さnの文字列strを考えると、この関数は、STRからなるリストを返す[0]、STR [1]、...、とstr [N-1]は、各STR [I]は、STR Iにcharに指します。
例

次のコードは、呼び出し元から取得したリストの長さかどうかをチェックし string_explodeを与えられた文字列には文字列の長さに等しいです。

```
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/string.sats"

implement main () = {
  val str = "abcdefg"
  val cs = string_explode (str)
  val () = assertloc (string_length (str) = g0i2u(list0_length (cs)))
}
```

## <a name="string_implode"></a>string\_implode

### 概要

```
fun{
} string_implode (cs: list0 (char)):<> string
```

### 説明

文字のリストcsのを考えると、この機能は、CSは...、[1]、[0] Csからなる文字列を返し、nはCSと各CSの長さであるCS [N​​-1]は、[i]が指し私はCSのcharします。CSにはヌル文字がない場合のみ、返される文字列の長さがnであることに注意してください。
例

次のコードは、呼び出し元から取得した文字列かどうかをチェックし string_implodeの呼び出しから返されたリストに 
string_explode与えられた文字列には、指定した文字列に等しいです：

```
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/string.sats"
implement main () = {
  val str = "abcdefg"
  val cs = string_explode (str)
  val str2 = string_implode (cs)
  val () = assertloc (str = str2)
}
```

## <a name="string_tabulate"></a>string\_tabulate

### 概要

```
fun string_tabulate
  (n: size_t, f: (size_t) -<cloref1> charNZ): string
```

## string_foreach

### 概要

```
fun string_foreach (x: string, f: cfun (char, void)): void
```

### 説明

この関数は最初の引数（文字列）を横断し、検出した各文字の第二引数（クロージャ関数）に適用されます。

### 例

次のコードは、標準出力チャネルに指定された文字列を出力します。

```
// string_foreach.dats
// patscc string_foreach.dats -DATS_MEMALLOC_LIBC -latslib -o string_foreach
staload "libats/ML/SATS/string.sats"

implement main0 () = { 
  val str = "abcdefg" 
  val ()  = string_foreach(str, lam(c) => print_char(c)) 
  val ()  = print_newline()
}
```

### 結果

```
abcdeg
```

## <a name="オーバーロードのシンボル"></a>オーバーロードのシンボル

## <a name="+"></a>+

### 概要

```
overload + with string_append of 0
```


