# oUnit

oUnitを使ってみます。

http://ymotongpoo.appspot.com/ounit_ja/user_guide.html

## インストール

```
opam install omake
opam install ounit
```

## OMake

```
omake --install
```

### `OMakefile`

```
.PHONY: all install clean test

USE_OCAMLFIND = true

OCAMLPACKS[] =
    oUnit

# OCamlGeneratedFiles(parser.ml lexer.ml)

FILES[] =
    foo
PROGRAM = foo

.DEFAULT: $(OCamlProgram $(PROGRAM), $(FILES))
  ./$(PROGRAM)

PROGRAM2 = test
FILES2[] =
    test

test: $(OCamlProgram $(PROGRAM2), $(FILES2) $(FILES))
  ./$(PROGRAM2)

clean:
  rm -rf .omakedb* *.cache *.omc *.o *.cm* $(PROGRAM) $(PROGRAM2) *.opt
```

### foo.ml

```
(* The functions we wish to test *)
let unity x = x;;
let funix ()= 0;;
let fgeneric () = failwith "Not implemented";;
```

### test.ml

```
open OUnit

let _ =
  run_test_tt_main ("suite">:::[
    "test1">:: begin fun() ->
      assert_equal 100 (Foo.unity 100)
    end;
    "test2">:: begin fun() ->
      assert_equal "x" (Foo.unity "x")
    end
  ])
```

## 実行

コンパイル＆実行

```
omake
```

テスト

```
omake test
```

クリーン

```
omake clean
```

