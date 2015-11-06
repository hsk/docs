#シンプルなGCのC言語実装

- [mark\_and\_sweep](mark_and_sweep)は単純なCだけで実装
- [mark_and_sweep_compiler](mark_and_sweep_compiler) 単純なCのランタイムを使用するコンパイラをOCamlで作成します(作成中)
- [frame_map](frame_map)はフレームマップ付きのGCをC(x86\_64 only)
- [new_world](new_world)はObjective-CのARCに似たGCの方式
- [bitmap_gc](bitmap_gc)は単純なBitmapGCをCだけで実装
