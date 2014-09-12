<?php
class Chan {}
class Senders extends Chan {
  
  function __construct($a) {
    $this->a = $a;
  }
}
class Receivers extends Chan {
  function __construct($a) {
    $this->a = $a;
  }
}
class Ref {
  function __construct($ref) {
    $this->ref = $ref;
  }
}

function newc() {
  return new Ref(new Senders(array()));
}

/**
 * 通信チャンネルyに値xを送る
 * @param Ref(Chan) y
 * @param A x
 */
function send($y, $x) {
  switch(true) {
    case $y->ref instanceof Senders: // 受信プロセスがない
      $y->ref->a[] = $x;
      break;
    case count($y->ref->a) == 0: // 同上
      // キューの後に値を付け加える
      $y->ref = new Senders(array($x));
      break;
    default: // 受信プロセスがある
      $f = array_shift($y->ref->a); // １つ取り出す。
      $f($x);
  }
}


/**
 * 通信チャンネルyから値を受信し，関数fを適用する
 * @param Ref(Chan) $y
 * @param function $f
 */
function recv($y, $f) {
  switch(true) {
    case $y->ref instanceof Receivers: // 値がない
      $y->ref->a[] = $f;
      break;
    case count($y->ref->a) == 0:
      $y->ref = new Receivers(array($f));
      break;
    default: // 値がある
      $x = array_shift($y->ref->a);
      // 一つだけ(x)を取り出して残り(ss)は戻す
      $y->ref = new Senders($y->ref->a);
      // 取り出した値を受信プロセスに渡す
      $f($x);
  }
}
  
$x = newc();
send($x, 3);
recv($x, function($y){ echo $y."\n"; });

// 新しい通信チャンネルcを作る
$c = newc();

// プロセスrepeat ()を再帰で定義
function repeat() {
  global $c;
  // cから整数iを受信
  recv($c, function($i) {
    // iを画面に表示
    printf ("%d\n", $i);
    // またrepeat()自身を生成
    repeat();
  });
}

// 最初のrepeat()を生成
repeat();

// cに1を送信
send($c, 1);

// cに2を送信
send($c, 2);

// 何度でも送信できる
send($c, 3);

// サーバーがリクエストを受け付けるための
// 通信チャンネルservcを作る
$servc = newc();

// サーバー・プロセスserv ()を再帰で定義
function serv() {
  global $servc;
  // servcから整数iと，返信のための
  // 通信チャンネルrepcの組を受け取る
  recv($servc, function($a) {
    // repcにiの2乗を返す
    send($a["repc"], $a["i"] * $a["i"]);
    // serv自身を再び生成
    serv();
  });
}

// サーバー・プロセスを起動
serv();

// 返信のためのチャンネルrを作る
$r = newc();

// サーバーに整数123とrを送る
send($servc, array("i"=>123, "repc"=>$r));

// rから答えの整数jを受け取り表示
recv($r, function($j){
  printf("%d\n", $j);
});
// サーバー・プロセスは何回でも
// 呼び出すことができる
send($servc, array("i"=>45, "repc"=>$r));

recv($r, function($j) {
  printf("%d\n", $j);
});

// サーバーがリクエストを受け付けるための
// 通信チャンネルfibcを作る
$fibc = newc();

// フィボナッチ・サーバーfib ()を定義
function fib() {
  global $fibc;
  // fibcから引数nと，返値を送るための
  // 通信チャンネルrepcの組を受け取る
  recv($fibc, function($a) { // (n, repc) =>
    global $fibc;
    // またfib ()自身を生成しておく
    fib();
    if($a["i"] <= 1) {
      // iが1以下ならiを返信
      send($a["repc"], $a["i"]);
    } else {
      // フィボナッチ・サーバー自身を利用して
      // 引数がn-1とn-2の場合の返値を計算
      $repc1 = newc();
      $repc2 = newc();
      send($fibc, array("i"=>$a["i"] - 1, "repc"=>$repc1));
      send($fibc, array("i"=>$a["i"] - 2, "repc"=>$repc2));
      recv($repc1, function($rep1)use(&$a,&$repc2) {
        recv($repc2, function($rep2)use(&$a,$rep1) {
          // 二つの返値を足してrepcに返信
          send($a["repc"], $rep1 + $rep2);
        });
      });
    }
  });
}

// フィボナッチ・サーバーを起動
fib();

// 返値を受け取るための通信チャンネルrを作る
$r = newc();
// 引数とrを送信
send($fibc, array("i"=>10, "repc"=>$r));
// rから返値mを受け取って表示
recv($r,function($m){
  printf("fib(10) = %d\n", $m);
});

