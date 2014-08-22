# 2 前提知識

この章では、module Preを見ていきます。

OCamlでは以下のような形でモジュールを作成出来ます。

	module モジュール名 = struct
		...
	end

OCamlのモジュールはC++のネームスペースのような物で、名前空間を分けて扱う事が出来るようにする物です。
モジュールはopen を使う事で、モジュール名を省略して使う事が出来るようになります。

オリジナルのOCamlのソースでは、モジュールは使用せずに、１ソースのみで記述されています。

そのため、非常に大きなプログラムに感じるかもしれません。
大きなプログラムを把握する良い方法の１つがモジュール化です。
幸い、Typing Haskell In Haskellでも、モジュール化したバージョンも存在しています。
それに合わせて、モジュール化することで、分かりやすくしました。

この章では、PreモジュールとIdモジュールの２つのモジュールを作成します。


## Preモジュール

2章の前提となる関数をPreモジュールとして記述します。

	module Pre = struct
	...
	end

このようなモジュールを作って、中に色々な関数を作っていきましょう。

### union

	  (* 和集合 *)
	  let union (xs: 'a list) (ys: 'a list):'a list =
	    List.filter begin fun x ->
	      not (List.mem x ys)
	    end xs @ ys

#### 使用例

	let _ =
	  let a = [5;4;3;2;1] in
	  let b = [4;5;6;7] in
	  let ab = Pre.union a b in
	  Printf.printf "union a b = %s\n" (Pre.show_int_list ab);

### intersect

	  (* 積集合 *)
	  let intersect (xs: 'a list) (ys: 'a list): 'a list =
	    List.filter begin fun x ->
	      List.mem x ys
	    end xs

#### 使用例

	let _ =
	  let a = [5;4;3;2;1] in
	  let b = [4;5;6;7] in
	  let ab = Pre.intersect a b in
	  Printf.printf "intersect a b = %s\n" (Pre.show_int_list ab);

### nub

	  (* リストをセットにする。要素が１つずつにまとめる *)
	  let nub (xs : 'a list): 'a list =
	    List.fold_left begin fun ys y ->
	      if List.mem y ys
	      then ys
	      else y :: ys
	    end [] xs

#### 使用例

	let _ =
	  let a = [1;1;2;2;3;4;5;1] in
	  let a = Pre.nub a in
	  Printf.printf "nub a b = %s\n" (Pre.show_int_list a);

### isEmpty

	  (* 空チェック *)
	  let isEmpty(xs: 'a list):bool =
	    begin match xs with
	      | [] -> true
	      | _ -> false
	    end

#### 使用例

	let _ =
	  let a = [] in
	  let a = Pre.isEmpty a in
	  Printf.printf "isEmpty = %b\n" a;

	  let a = [1] in
	  let a = Pre.isEmpty a in
	  Printf.printf "isEmpty = %b\n" a;

### fold_left1

	  (* たぶんこれは、reduceじゃないのかな *)
	  let fold_left1 (f:'a -> 'a -> 'a) (xs:'a list): 'a = 
	    begin match xs with
	      | [] -> invalid_arg "empty list"
	      | [x] -> x
	      | x :: xs -> List.fold_left f x xs
	    end

#### 使用例

	let _ =
	  let a = [1;2;3;4;5;6;7;8;9;10] in
	  let a = Pre.fold_left1 begin fun a b ->
	    a + b
	  end a in
	  Printf.printf "fold_left1 = %d\n" a;

### deleteFirst

	  (* リスト内の最初の1個目のxを削除する *)
	  let rec deleteFirst (x:'a) (ys:'a list): 'a list = 
	    begin match ys with
	      | [] -> []
	      | y :: ys ->
	        if x = y then ys
	        else y :: deleteFirst x ys
	    end

#### 使用例

	let _ =
	  let a = [1;2;3;4;3;4;5] in
	  let a = Pre.deleteFirst 3 a in
	  Printf.printf "deleteFirst = %s\n" (Pre.show_int_list a);


### diff

	  (* 最初のリストから2番目のリストの要素を消す *)
	  let rec diff (xs:'a list) (ys:'a list): 'a list =
	    begin match ys with
	      | [] -> xs
	      | y :: ys -> diff (deleteFirst y xs) ys
	    end

#### 使用例

	let _ =
	  let a = [1;2;3;4] in
	  let b = [3;4;5;6] in
	  let a = Pre.diff a b in
	  Printf.printf "diff = %s\n" (Pre.show_int_list a);

	  let a = [1;2;3;4;3;4] in
	  let b = [3;4;5;6;4] in
	  let a = Pre.diff a b in
	  Printf.printf "diff = %s\n" (Pre.show_int_list a);

### split3

	  (*3つの多値を持っているリストを３つのリストに分割する *)
	  let split3 (xs:('a * 'b * 'c)list):('a list * 'b list * 'c list) =
	    begin
	      let rec loop ((ws:'a list), (xs:'b list), (ys:'c list))
	        (zs:('a * 'b * 'c)list):('a list * 'b list * 'c list) =
	        begin match zs with
	          | [] -> (List.rev ws, List.rev xs, List.rev ys)
	          | (w, x, y) :: zs -> loop (w :: ws, x :: xs, y :: ys) zs
	        end
	    in
	      loop ([], [], []) xs
	    end

	  let show_list show sep xs =
	    begin
	      let rec loop xs =
	        begin match xs with
	          | [] -> ""
	          | [x] -> show x
	          | x::xs -> show x ^ sep ^ loop xs
	        end
	    in
	      Printf.sprintf "[%s]" (loop xs)
	    end
	  let show_int_list xs =
	    show_list string_of_int "; " xs
	end

#### 使用例

	let _ =

	  let a1 = [1,10,100;2,20,200;3,30,300] in
	  let a,b,c = Pre.split3 a1 in
	  Printf.printf "split3 = %s;%s;%s\n"
	    (Pre.show_int_list a)
	    (Pre.show_int_list b)
	    (Pre.show_int_list c);

## Idモジュール

	module Id = struct
		...
	end

### type id

	  type id = string

### enumId関数

	  (* 数値に対するidを取得する *)
	  let enumId (n:int) : id =
	    "v" ^ string_of_int n

#### 使用例

	let _ =
	  Printf.printf "%s\n" (Id.enumId 1)
