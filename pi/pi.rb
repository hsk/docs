class Senders
  attr_accessor :a
  def initialize(a)
    @a = a
  end
end

class Receivers
  attr_accessor :a
  def initialize(a)
    @a = a
  end
end

class Ref
  attr_accessor :ref
  def initialize(ref)
    @ref = ref
  end
end

def newc()
  Ref.new(Senders.new([]))
end

#
# 通信チャンネルyに値xを送る
# @param Ref(Chan) y
# @param A x
#
def send(y, x)
  case(true)
    when y.ref.is_a?(Senders) then # 受信プロセスがない
      y.ref.a << x
    when y.ref.a.length == 0 then # 同上
      # キューの後に値を付け加える
      y.ref = Senders.new([x])
    else # 受信プロセスがある
      y.ref.a.shift.call(x) # １つ取り出す。
  end
end

#
# 通信チャンネルyから値を受信し，関数fを適用する
# @param Ref(Chan) $y
# @param function $f
#
def recv(y, &f)
  case(true)
    when y.ref.is_a?(Receivers) then # 値がない
      y.ref.a << f
    when y.ref.a.length == 0 then
      y.ref = Receivers.new([f])
    else # 値がある
      # 一つだけ(x)を取り出して残り(ss)は戻す
      # 取り出した値を受信プロセスに渡す
      f.call(y.ref.a.shift)
  end
end
  
x = newc()

send(x, 3)
recv(x) do |y|
  printf("%d\n", y)
end

# 新しい通信チャンネルcを作る
$c = newc()

# プロセスrepeat ()を再帰で定義
def repeat()
  # cから整数iを受信
  recv($c) do |i|
    # iを画面に表示
    printf("%d\n", i)
    # またrepeat()自身を生成
    repeat()
  end
end

# 最初のrepeat()を生成
repeat()

# cに1を送信
send($c, 1)

# cに2を送信
send($c, 2)

# 何度でも送信できる
send($c, 3)

# サーバーがリクエストを受け付けるための
# 通信チャンネルservcを作る
$servc = newc()

# サーバー・プロセスserv ()を再帰で定義
def serv()
  # servcから整数iと，返信のための
  # 通信チャンネルrepcの組を受け取る
  recv($servc) do |a|
    p(a["i"])
    # repcにiの2乗を返す
    send(a["repc"], a["i"] * a["i"])
    # serv自身を再び生成
    serv()
  end
end

# サーバー・プロセスを起動
serv()

# 返信のためのチャンネルrを作る
r = newc()

# サーバーに整数123とrを送る
send($servc, {"i"=>123, "repc"=>r})

# rから答えの整数jを受け取り表示
recv(r) do |j|
  printf("%d\n", j)
end
# サーバー・プロセスは何回でも
# 呼び出すことができる
send($servc, {"i"=>45, "repc"=>r})

recv(r) do |j|
  printf("%d\n", j)
end

# サーバーがリクエストを受け付けるための
# 通信チャンネルfibcを作る
$fibc = newc()

# フィボナッチ・サーバーfib ()を定義
def fib()
  # fibcから引数nと，返値を送るための
  # 通信チャンネルrepcの組を受け取る
  recv($fibc) do |a|
    # またfib ()自身を生成しておく
    fib()
    if(a["i"] <= 1)
      # iが1以下ならiを返信
      send(a["repc"], a["i"])
    else
      # フィボナッチ・サーバー自身を利用して
      # 引数がn-1とn-2の場合の返値を計算
      repc1 = newc()
      repc2 = newc()
      send($fibc, {"i"=>a["i"] - 1, "repc"=>repc1})
      send($fibc, {"i"=>a["i"] - 2, "repc"=>repc2})
      recv(repc1) do |rep1|
        recv(repc2) do |rep2|
          # 二つの返値を足してrepcに返信
          send(a["repc"], rep1 + rep2)
        end
      end
    end
  end
end

# フィボナッチ・サーバーを起動
fib()

# 返値を受け取るための通信チャンネルrを作る
r = newc()
# 引数とrを送信
send($fibc, {"i"=>10, "repc"=>r})

# rから返値mを受け取って表示
recv(r) do |m|
  printf("fib(10) = %d\n", m)
end

