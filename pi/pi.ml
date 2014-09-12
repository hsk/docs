type 'a chan =
  | Senders of 'a list
  | Receivers of ('a -> unit) list

let newc () = ref (Senders [])

(* 通信チャンネルyに値xを送る *)
let send y x =
  match !y with
  | Senders ss -> (* 受信プロセスがない *)
    (* キューの後に値を付け加える *)
    y := Senders(ss @ [x])
  | Receivers [] -> (* 同上 *)
    y := Senders [x]
  | Receivers(f :: rs) -> (* 受信プロセスがある *)
    (* 一つ(f)を取り出して残り(rs)は戻す *)
    y := Receivers rs;
    (* 取り出した受信プロセスに値を渡す *)
    f x

(* 通信チャンネルyから値を受信し，関数fを適用する *)
let recv y f =
  match !y with
  | Receivers rs -> (* 値がない *)
      (* ブロックした受信プロセスをキューに追加 *)
      y := Receivers(rs @ [f])
  | Senders [] -> (* 同上 *)
      y := Receivers [f]
  | Senders(x :: ss) -> (* 値がある *)
      (* 一つだけ(x)を取り出して残り(ss)は戻す *)
      y := Senders ss;
      (* 取り出した値を受信プロセスに渡す *)
      f x

let _ =
  let x = newc() in
  send x 3;
  recv x begin fun y ->
    Printf.printf "%d\n" y
  end

let _ =
  (* 新しい通信チャンネルcを作る *)
  let c = newc () in
  (* プロセスrepeat ()を再帰で定義 *)
  let rec repeat () =
    (* cから整数iを受信 *)
    recv c begin fun i ->
      (* iを画面に表示 *)
      Printf.printf "%d\n" i;
      (* またrepeat ()自身を生成 *)
      repeat ()
    end
  in
  (* 最初のrepeat ()を生成 *)
  repeat ();

  (* cに1を送信 *)
  send c 1;

  (* cに2を送信 *)
  send c 2;

  (* 何度でも送信できる *)
  send c 3

