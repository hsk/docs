pandoc title.txt \
  Home.md \
  1.intro.md \
  2.一般化.md \
  3.メモリ管理ミスがある不完全な一般化.md \
  4.レベルによる効率的な一般化.md \
  5.さらに効率的なレベルベースの​​一般化.md \
  6.OCaml内レベル一般化.md \
  7.型リージョン.md \
  8.レベルの発見.md \
  9.新鮮な型変数の生成.md \
  a.真の一般化の複雑さ.md \
  -o generalization.epub \
  --epub-stylesheet=epub.css --smart
open generalization.epub
