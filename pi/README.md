# π計算を書いてみる

数理科学的バグ撲滅方法論のすすめITpro
第11回　クロージャによる超軽量並行プロセスの簡単実装法

http://itpro.nikkeibp.co.jp/article/COLUMN/20070612/274231/

を見ながら、π計算を、ScalaとPHPとRubyで書いて、オブジェクト指向化してみました。

## ファイル一覧

- pi.scala
    - Scalaに途中まで移植して力つきたもの(fibの型を書くのが面倒になったw)
- pi.php
    - Scala版をPHPに移植
- pi2.php
    - send,recv関数をオブジェクト化
- pi3.php
    - CActor,ServActor,XActor,FibActorクラスを作成
- pi4.php
    - SendersとReceiversのsend,recvをメソッドの多態性を使うようにした
- pi5.php
    - act,reactで書けるようにした
- pi.rb
    - PHP版をrbに移植
- pi2.rb
    - send,recv関数をオブジェクト化
- pi3.rb
    - CActor,ServActor,XActor,FibActorクラスを作成
- pi4.rb
    - SendersとReceiversのsend,recvをメソッドの多態性を使うようにした
- pi5.rb
    - act,reactで書けるようにした
