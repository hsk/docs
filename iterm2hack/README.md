# iTerm2 hack

## 1. Introduction

iTerm2 はカッコイイMacのターミナルアプリです。
コマンドラインツールを使う場合は、iTerm2を使うとより便利でカッコイイのですが、よりかっこ良くしてみようというのがこのコンテンツです。

TEDを見てて、GUIが変わるとかやってたわけですが、直ぐ使えるわけではありません。
今直ぐ、かっこ良くしたGUIを使いたいんじゃオラ！って思うわけです。
どうやってかっこ良くするかのアイディアは昔からありました。
背景を透明に近付けた場合に、文字の周りを背景色で塗るのです。
要するに、朝のテレビ時計の表示のように、城文字の後ろに黒い枠を付けてやる事で、背景が透明でも見やすくする事で、背景を透過した状態で使いやすくするのです。

FlashのAIRでそのようなエディタを作ってみた事はありました。
しかし、普段使えるわけではありません。Javaでも作った事はあります。
でも、普段使うにはイマイチ。
ああ、ターミナルを弄れたら。ああ、今ならやれるんじゃないかな。
やってみよう。おお、出来たぞ！カッコいいぞ！！
ってことです。
ターミナルをかっこ良くするだけで、様々なアプリケーションに対応出来ます。
当たり前だけど、vimやemacsがかっこ良くなる。その他の作業もかっこいい！
ムービーを背景で再生させながら、作業する事も出来ます。

## 2. iTermの弄り方

xcodeがインストールしてあれば、以下のコマンドでビルドする事が出来ます。

```
git clone https://github.com/gnachman/iTerm2.git
cd iTerm2
make run
```

cloneして、make runするだけです。

## 3. diff

以下の変更を加えましょう。

PTYTextViewがテキストビューで、背景のドロップシャドーがなかったことにします。

```
diff --git a/sources/PTYTextView.m b/sources/PTYTextView.m
index afba57f..49ede1d 100644
--- a/sources/PTYTextView.m
+++ b/sources/PTYTextView.m
@@ -520,6 +520,8 @@ static const int kDragThreshold = 3;
                                             owner:self
                                          userInfo:nil] autorelease];
         [self addTrackingArea:trackingArea];
+        [[self window] setHasShadow:YES];
+        [[self window] setHasShadow:NO];
     }
 }
```

描画周りは、iTermTextDrawingHelperにまとまっています。
このなかで、文字を描画する箇所を書き換えて上げます。

``` 
diff --git a/sources/iTermTextDrawingHelper.m b/sources/iTermTextDrawingHelper.m
index 3f75f4f..37678f7 100644
--- a/sources/iTermTextDrawingHelper.m
+++ b/sources/iTermTextDrawingHelper.m
@@ -730,9 +730,6 @@ static const int kBadgeRightMargin = 10;
     CGContextSetFillColorSpace(ctx, [[currentRun->attrs.color colorSpace] CGColorSpace]);
     int componentCount = [currentRun->attrs.color numberOfComponents];
 
-    CGFloat components[componentCount];
-    [currentRun->attrs.color getComponents:components];
-    CGContextSetFillColor(ctx, components);
 
     double y = initialPoint.y + _cellSize.height + currentRun->attrs.fontInfo.baselineOffset;
     int x = initialPoint.x + currentRun->x;
@@ -741,11 +738,51 @@ static const int kBadgeRightMargin = 10;
     if (currentRun->attrs.fakeItalic) {
         m21 = 0.2;
     }
+    void *advances = CRunGetAdvances(currentRun);
+
+    NSColor* back = [self defaultBackgroundColor];
+
+    if(
+        fabs([back redComponent] - [currentRun->attrs.color redComponent]) < 0.0001 ||
+        fabs([back greenComponent] - [currentRun->attrs.color greenComponent]) < 0.0001 ||
+        fabs([back blueComponent] - [currentRun->attrs.color blueComponent]) < 0.0001
+        ) {
+        back = [self defaultTextColor];
+    }
+
+    CGFloat components[componentCount];
+    [[back colorWithAlphaComponent:1] getComponents:components];
+    CGContextSetFillColor(ctx, components);
+
+    for(int i = -1; i <= 1; i++) {
+        CGContextSetTextMatrix(ctx, CGAffineTransformMake(1.0,  0.0,
+                                                          m21, -1.0,
+                                                          x+i, y-2));
+        CGContextShowGlyphsWithAdvances(ctx, glyphs, advances, length);
+        CGContextSetTextMatrix(ctx, CGAffineTransformMake(1.0,  0.0,
+                                                          m21, -1.0,
+                                                          x+i, y+2));
+        CGContextShowGlyphsWithAdvances(ctx, glyphs, advances, length);
+    }
+
+    for(int i = -1; i <= 1; i++) {
+        CGContextSetTextMatrix(ctx, CGAffineTransformMake(1.0,  0.0,
+                                                          m21, -1.0,
+                                                          x-2, y+i));
+        CGContextShowGlyphsWithAdvances(ctx, glyphs, advances, length);
+        CGContextSetTextMatrix(ctx, CGAffineTransformMake(1.0,  0.0,
+                                                          m21, -1.0,
+                                                          x+2, y+i));
+        CGContextShowGlyphsWithAdvances(ctx, glyphs, advances, length);
+    }
+
+
+    [currentRun->attrs.color getComponents:components];
+    CGContextSetFillColor(ctx, components);
+
     CGContextSetTextMatrix(ctx, CGAffineTransformMake(1.0,  0.0,
                                                       m21, -1.0,
                                                       x, y));
-
-    void *advances = CRunGetAdvances(currentRun);
     CGContextShowGlyphsWithAdvances(ctx, glyphs, advances, length);
 
     if (currentRun->attrs.fakeBold) {
```


さ、以上の作業で、かっこ良くなります。

```
make run
```

で実行してうまく行ったら、Dockに追加して、
設定画面で好みの色に変えて、カッコ良くして未来を感じましょう！！

## 4. Future work

このアイディアを普段使うエディタにも適用したい！
SublimeTextエディタは残念ながら、オープンソースではありません。
しかし、似たような Atom や Adobe Branckets は、オープンソース！
NW.js (node-webkit) は透明化ウィンドウが可能！
ということは、きっと普段使うエディタもかっこ良く出来るはず！
やってみるしかありません。

## 5. Conclusion

iTerm2を改造するアイディアを考え、iTerm2をビルドする方法を紹介し、改造しました。
それではみなさん、楽しいハックライフを!

## 6. References

- iTerm2
    https://github.com/gnachman/iTerm2
- RoundTransparentWindow
    https://developer.apple.com/library/mac/samplecode/RoundTransparentWindow/Introduction/Intro.html
