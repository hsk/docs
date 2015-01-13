# スタートATS

  簡単なATSの入門ページ

  インストールは出来てる事前提で、色々な機能を普通に使ってみる話。


## Hello World!

  ```
  // hello.dats
  implement main0() = {
  	val () = println!("Hello World!")
  }
  ```

  ```
  $ patscc hello.dats 
  $ ./a.out
  Hello World!
  ```

  a.outを消しておきましょう。

  ```
  $ rm a.out
  ```

## Makefile

  Makefileを作ってビルドを自動化しましょう。

  ```
  hello:
  	patscc -o hello hello.dats

  clean:
  	rm *_dats.c hello
  ```

  make helloと入力する事でビルド出来て、./helloで実行出来ます。

  ```
  $ make hello
  $ ./hello
  Hello World!
  ```

  make cleanで不要なファイルを消しましょう。

  ```
  $ make clean
  ```

## 整数

  intを使ってみましょう。

  ```
  // int.dats

  implement main0() = {
  	val v:int = 10
  	val () = println!(v)
  }
  ```

  Makefileに以下のように書き加えます。

  ```
  int:
  	patscc -o int int.dats

  clean:
  	rm *_dats.c hello int
  ```

  コンパイル&実行してみましょう。

  ```
  $ make int
  $ ./int
  10
  ```

  10と表示されました。

## 型推論

  ATSは型推論があるので:intを消しても動きます。

  ```
  // int2.dats

  implement main0() = {
  	val v = 10
  	val () = println!(v)
  }
  ```

  Makefileはいい感じに書き加えて、実行してみましょう。

  ```
  $ make int2
  $ ./int2
  10
  ```
