# jasc

jasmin fast assembler


jasc はocamlで作成中のwindowsでもビルドが楽に出来る、jasmin形式のjvmバイトコードアセンブラです。
aa.j ファイルをコンパイルしてaa.classファイルを作成出来ます。

元ファイルをHaxeのjavalibから派生して、
とにかく他のライブラリへの依存を減らしてます。

今はまだ全然動きませんが、そのうち動くようになるはずです。

## TODO

- [x] a.javaを動かす
	- [x] constの中味が表示されない
	- [x] コンパイル結果が固定になってる

- [x] jasmcのテストを持って来て潰す。
	- [x] コンスタントな値
	- [x] インターフェイス
	- [x] long
