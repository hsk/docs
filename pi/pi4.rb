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

end
  
x = Actor.new()

x.send(3)
x.recv do |y|
  printf("%d\n", y)
end


class CActor < Actor
  
  def initialize()
    super()
    # 最初のrepeat()を生成
    repeat()
  end
  
  # プロセスrepeat ()を再帰で定義
  def repeat()
    # cから整数iを受信
    recv do |i|
      # iを画面に表示
      printf("%d\n", i)
      # またrepeat()自身を生成
      repeat()
    end
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

  def initialize()
    super()
    # サーバー・プロセスを起動
    serv()
  end

  # サーバー・プロセスserv ()を再帰で定義
  def serv()
    # servcから整数iと，返信のための
    # 通信チャンネルrepcの組を受け取る
    recv do |a|
      # repcにiの2乗を返す
      a["repc"].send(a["i"] * a["i"])
      # serv自身を再び生成
      serv()
    end
  end
  
end

# サーバーがリクエストを受け付けるための
# 通信チャンネルservcを作る
servc = ServActor.new()
 
# 返信のためのチャンネルrを作る
r = Actor.new()

# サーバーに整数123とrを送る
servc.send({"i"=>123, "repc"=>r})

# rから答えの整数jを受け取り表示
r.recv do |j|
  printf("%d\n", j)
end
# サーバー・プロセスは何回でも
# 呼び出すことができる
servc.send({"i"=>45, "repc"=>r})

r.recv do |j|
  printf("%d\n", j)
end

class FibActor < Actor

  def initialize()
    super()
    # フィボナッチ・サーバーを起動
    fib()
  end


  # フィボナッチ・サーバーfib ()を定義
  def fib()
    # fibcから引数nと，返値を送るための
    # 通信チャンネルrepcの組を受け取る
    recv do |a|
      # またfib ()自身を生成しておく
      fib()
      if(a["i"] <= 1)
        # iが1以下ならiを返信
        a["repc"].send(a["i"])
      else
        # フィボナッチ・サーバー自身を利用して
        # 引数がn-1とn-2の場合の返値を計算
        repc1 = Actor.new()
        repc2 = Actor.new()
        send({"i"=>a["i"] - 1, "repc"=>repc1})
        send({"i"=>a["i"] - 2, "repc"=>repc2})
        repc1.recv do |rep1|
          repc2.recv do |rep2|
            # 二つの返値を足してrepcに返信
            a["repc"].send(rep1 + rep2)
          end
        end
      end
    end
  end
end

# サーバーがリクエストを受け付けるための
# 通信チャンネルfibcを作る
fibc = Actor.new()

# 返値を受け取るための通信チャンネルrを作る
r = Actor.new()
# 引数とrを送信
fibc.send({"i"=>10, "repc"=>r})

# rから返値mを受け取って表示
r.recv do |m|
  printf("fib(10) = %d\n", m)
end

