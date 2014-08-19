# 12 Conclusions 結論

We have presented a complete Haskell program that implements a type checker for the Haskell language.

私たちは Haskell 言語の型チェックを実装する完全な Haskell プログラムを発表しました。

In the process, we have clarified certain aspects of the current design, as well as identifying some ambiguities in the existing, informal specification.

プロセスでは、現在の非公式の仕様のいくつかのあいまいさを識別するだけでなく、現在の設計の特定の側面を明らかにしました。
The type checker has been developed, type-checked, and tested using the ``Haskell 98 mode'' of Hugs 98 [ Jones & Peterson, 1999].

型チェックは開発、型チェックと Hugs 98 [ジョーンズ & Peterson、1999年] の「Haskell 98 モード」を使用してテストをされています。

The full program includes many additional functions, not shown in this paper, to ease the task of testing, debugging, and displaying results.

完全なプログラムにはテスト、デバッグ、および結果の表示のタスクを容易にするこの論文では表示されません、多くの追加機能が含まれています。

We have also translated several large Haskell programs-including the Standard Prelude, the Maybe and List libraries, and the source code for the type checker itself-into the representations described in Section 11, and successfully passed these through the type checker.

我々 はいくつか大きな Haskell プログラム-を含む標準の前奏曲、多分翻訳も自体型チェッカーのソース コードおよびリスト ライブラリ-記述表現にセクション 11 そして首尾よく渡された型チェッカーを介してこれら。

As a result of these and other experiments we have good evidence that the type checker is working as intended, and in accordance with the expectations of Haskell programmers.

これらと他の実験の結果は意図したとおり、Haskell プログラマの期待に応じて型チェックが実行されている良い証拠を持っています。

We believe that this typechecker can play a useful role, both as a formal specification for the Haskell type system, and as a testbed for experimenting with future extensions.

我々 はこの typechecker が、Haskell の型システムの形式的仕様記述と将来的な拡張の実験のためのテストベッドとして有用な役割を再生することができますと信じています。
