#include <stdlib.h>
#include "smlsharp.h"
#include "splay.h"

int cmp(void *a, void *b)
{
	if ((int)a < (int)b) return -1;
	else if ((int) a > (int)b) return 1;
	else return 0;
}

int main()
{
	// ツリー情報
	sml_tree_t tree;

	void *value;
	int i;
	int data[8] = {53, 30, 0, 16, 38, 3, 57, 94};


	// ルートは空です。
	tree.root = NULL;
	// 比較関数を設定
	tree.cmp = cmp;
	// アロケータと
	tree.alloc = malloc;
	// フリー関数をセットします。
	tree.free = free;

	// 8回ループで
	for (i = 0; i < 8; i++) {
		// データを読み込んでツリーに追加します。
		sml_tree_insert(&tree, (void*)data[i]);
		printf("insert %d\n", data[i]);

		sml_tree_dump(tree.root);
	}

/*

53をインサート

insert 53
53

30をインサートする

insert 30
30
  53

0をインサートドンドン手前にただのリスト構造か？

insert 0
0
  30
    53

16をインサートすると、ソートされてるのかな。

insert 16
  0
16
  30
    53


38をインサートすると、ソートされてますな。

insert 38
      0
    16
  30
38
  53

3を入れてもソートされてるツリーが出来てます。！

insert 3
  0
3
    16
  30
    38
      53

こういうデータ構造なのか。


insert 57
        0
      3
        16
    30
      38
  53
57

なんか、バランスしてない例なのかなんなのか。

insert 94
          0
        3
          16
      30
        38
    53
  57
94
*/

	for (i = 0; i < 8; i++) {
		value = sml_tree_find(&tree, (void*)data[i]);
		printf("find %d : %d\n", data[i], (int)value);
		sml_tree_dump(tree.root);
	}

/*

53を探すと

find 53 : 53
      0
    3
      16
  30
    38
53
  57
    94

30を探すと、なんか、形が変わってます。

find 30 : 30
    0
  3
    16
30
    38
  53
    57
      94

0を探すとまた変わった。

find 0 : 0
0
  3
      16
    30
        38
      53
        57
          94

ドンドン変わる。検索した物が一番上に来るキャッシュ効くような構造なのか。

find 16 : 16
    0
  3
16
  30
      38
    53
      57
        94
find 38 : 38
        0
      3
    16
  30
38
  53
    57
      94
find 3 : 3
  0
3
    16
  30
    38
      53
        57
          94
find 57 : 57
      0
    3
      16
  30
      38
    53
57
  94
find 94 : 94
        0
      3
        16
    30
        38
      53
  57
94
*/

	for (i = 0; i < 8; i++) {
		value = sml_tree_delete(&tree, (void*)data[i]);
		printf("delete %d : %d\n", data[i], (int)value);
		sml_tree_dump(tree.root);
	}

/*

削除すると

delete 53 : 53
      0
    3
      16
  30
38
  57
    94

消えてます。

delete 30 : 30
    0
  3
16
  38
    57
      94

消した近くがトップに来るんですね。

delete 0 : 0
3
  16
    38
      57
        94
delete 16 : 16
3
  38
    57
      94
delete 38 : 38
3
  57
    94
delete 3 : 3
57
  94
delete 57 : 57
94
delete 94 : 94

全部消えました。
*/

	return 0;
}
