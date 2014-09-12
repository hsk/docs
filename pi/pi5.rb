class Senders
  attr_accessor :a
  def initialize(a)
    @a = a
  end

  # 受信プロセスがない
  def send(x)
    a << x
    self
  end

  #
  # 通信チャンネルyから値を受信し，関数fを適用する
  #
  def recv(f)
    if a.length == 0 then
      Receivers.new([f])
    else # 値がある
      # 一つだけ(x)を取り出して残り(ss)は戻す
      # 取り出した値を受信プロセスに渡す
      f.call(a.shift)
      self
    end
  end
  
end

class Receivers
  attr_accessor :a
  def initialize(a)
    @a = a
  end

  def send(x)
    if a.length == 0 then # 同上
      # キューの後に値を付け加える
      Senders.new([x])
    else # 受信プロセスがある
      a.shift.call(x) # １つ取り出す。
      self
    end
  end

  def recv(f)
    @a << f
    self
  end
 
end

class Actor
  attr_accessor :ref
  def initialize()
    @ref = Senders.new([])
    react
  end
  
  #
  # 通信チャンネルyに値xを送る
  #
  def send(x)
    @ref = @ref.send(x)
  end

  def recv(&f)
    @ref = @ref.recv(f)
  end
  def react
    recv(&method(:act))
  end
end
class NActor < Actor
  def react
  end
end

class XActor < Actor
  def act(y)
    printf("%d\n", y)
  end
end
  
x = XActor.new()

x.send(3)

class CActor < Actor
  
  # cから整数iを受信
  def act(i)
    # iを画面に表示
    printf("%d\n", i)
    # またreact自身を生成
    react
  end
end

# 新しい通信チャンネルcを作る
c = CActor.new()

# cに1を送信
c.send(1)

# cに2を送信
c.send(2)

# 何度でも送信できる
c.send(3)


class ServActor < Actor

  # サーバー・プロセスserv ()を再帰で定義
  def act(a)
    # servcから整数iと，返信のための
    # 通信チャンネルrepcの組を受け取る
    # repcにiの2乗を返す
    a["repc"].send(a["i"] * a["i"])
    # react自身を再び生成
    react
  end
 
end

# サーバーがリクエストを受け付けるための
# 通信チャンネルservcを作る
servc = ServActor.new()

# 返信のためのチャンネルrを作る
r = CActor.new()

# サーバーに整数123とrを送る
servc.send({"i"=>123, "repc"=>r})

# サーバー・プロセスは何回でも
# 呼び出すことができる
servc.send({"i"=>45, "repc"=>r})

class FibActor < Actor

  # フィボナッチ・サーバーfib ()を定義
  def act(a)
    # fibcから引数nと，返値を送るための
    # 通信チャンネルrepcの組を受け取る
    # またfib ()自身を生成しておく
    react
    if(a["i"] <= 1)
      # iが1以下ならiを返信
      a["repc"].send(a["i"])
    else
      # フィボナッチ・サーバー自身を利用して
      # 引数がn-1とn-2の場合の返値を計算
      repc1 = NActor.new()
      repc2 = NActor.new()
      repc1.recv do |rep1|
        repc2.recv do |rep2|
          # 二つの返値を足してrepcに返信
          a["repc"].send(rep1 + rep2)
        end
      end
      send({"i"=>a["i"] - 1, "repc"=>repc1})
      send({"i"=>a["i"] - 2, "repc"=>repc2})
    end
  end
end

# サーバーがリクエストを受け付けるための
# 通信チャンネルfibcを作る
fibc = FibActor.new()

# 返値を受け取るための通信チャンネルrを作る

r = NActor.new()
# 引数とrを送信
fibc.send({"i"=>10, "repc"=>r})

r.recv do |i|
  printf("fib(10)=%d\n", i)
end

