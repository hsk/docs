package PI

sealed trait Chan[A]
sealed case class Senders[A](a:List[A]) extends Chan[A]
sealed case class Receivers[A](a:List[(A)=>Unit]) extends Chan[A]
sealed case class Ref[A](var ref:A)


object Main extends App {

  def newc[A]():Ref[Chan[A]] = Ref(Senders(List[A]()))


  // 通信チャンネルyに値xを送る
  def send[A](y:Ref[Chan[A]], x:A) {
    y.ref match {
    case Senders(ss) => // 受信プロセスがない
      // キューの後に値を付け加える
      y.ref = Senders(ss ::: List(x))
    case Receivers(List()) => // 同上
      y.ref = Senders(List(x))
    case Receivers(f :: rs) => // 受信プロセスがある
      // 一つ(f)を取り出して残り(rs)は戻す
      y.ref = Receivers(rs)
      // 取り出した受信プロセスに値を渡す
      f(x)
    }
  }

  // 通信チャンネルyから値を受信し，関数fを適用する
  def recv[A](y:Ref[Chan[A]])(f:(A)=>Unit) {
    y.ref match {
    case Receivers(rs) => // 値がない
      // ブロックした受信プロセスをキューに追加
      y.ref = Receivers(rs ::: List(f))
    case Senders(List()) => // 同上
      y.ref = Receivers(List(f))
    case Senders(x :: ss) => // 値がある
      // 一つだけ(x)を取り出して残り(ss)は戻す
      y.ref = Senders(ss)
      // 取り出した値を受信プロセスに渡す
      f(x)
    }
  }
  
  val x = newc[Int]()
  send(x, 3)
  recv(x) {y =>
    println(y)
  }
  
  // 新しい通信チャンネルcを作る
  val c = newc[Int]()

  // プロセスrepeat ()を再帰で定義
  def repeat() {
    // cから整数iを受信
    recv(c){ i =>
      // iを画面に表示
      println(i)
      // またrepeat()自身を生成
      repeat()
    }
  }

  // 最初のrepeat()を生成
  repeat()

  // cに1を送信
  send(c, 1)

  // cに2を送信
  send(c, 2)

  // 何度でも送信できる
  send(c, 3)


  // サーバーがリクエストを受け付けるための
  // 通信チャンネルservcを作る
  val servc = newc[(Int,Ref[Chan[Int]])]()

  // サーバー・プロセスserv ()を再帰で定義
  def serv() {
    // servcから整数iと，返信のための
    // 通信チャンネルrepcの組を受け取る
    recv(servc){case (i, repc) =>
      // repcにiの2乗を返す
      send[Int](repc, i * i)
      // serv自身を再び生成
      serv()
    }
  }

  // サーバー・プロセスを起動
  serv()

  // 返信のためのチャンネルrを作る
  val r = newc[Int]()

  // サーバーに整数123とrを送る
  send(servc, (123, r))

  // rから答えの整数jを受け取り表示
  recv(r){ j =>
    println(j)
  }
  // サーバー・プロセスは何回でも
  // 呼び出すことができる
  send(servc, (45, r))

  recv(r){ j =>
    println(j)
  }

  // サーバーがリクエストを受け付けるための
  // 通信チャンネルfibcを作る
  val fibc = newc()

  // フィボナッチ・サーバーfib ()を定義
  def fib() {
    // fibcから引数nと，返値を送るための
    // 通信チャンネルrepcの組を受け取る
    recv(fibc) {case (n, repc) =>
      // またfib ()自身を生成しておく
      fib()
      if(n <= 1)
        // nが1以下ならnを返信
        send(repc, n)
      else {
        // フィボナッチ・サーバー自身を利用して
        // 引数がn-1とn-2の場合の返値を計算
        val repc1 = newc ()
        val repc2 = newc ()
        send(fibc, (n - 1, repc1))
        send(fibc, (n - 2, repc2))
        recv(repc1) {rep1 =>
          recv(repc2) {rep2 =>
            // 二つの返値を足してrepcに返信
            send(repc, rep1 + rep2)
          }
        }
      }
    }
  }

  // フィボナッチ・サーバーを起動
  fib()

  // 返値を受け取るための通信チャンネルrを作る
  val r = newc()
  // 引数とrを送信
  send(fibc, (10, r))
  // rから返値mを受け取って表示
  recv(r){ m =>
    println("fib(10) = " + m)
  }

}
