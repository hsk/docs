
(* 2 Preliminaries *)
(* 2 予備知識 *)
module Pre = struct
  (* 和集合 *)
  let union (xs: 'a list) (ys: 'a list):'a list =
    List.filter begin fun x ->
      not (List.mem x ys)
    end xs @ ys

  (* 積集合 *)
  let intersect (xs: 'a list) (ys: 'a list): 'a list =
    List.filter begin fun x ->
      List.mem x ys
    end xs

  (* リストをセットにする。要素が１つずつにまとめる *)
  let nub (xs : 'a list): 'a list =
    List.fold_left begin fun ys y ->
      if List.mem y ys
      then ys
      else y :: ys
    end [] xs

  (* 空チェック *)
  let isEmpty(xs: 'a list):bool =
    begin match xs with
      | [] -> true
      | _ -> false
    end

  (* たぶんこれは、reduceじゃないのかな *)
  let fold_left1 (f:'a -> 'a -> 'a) (xs:'a list): 'a = 
    begin match xs with
      | [] -> invalid_arg "empty list"
      | [x] -> x
      | x :: xs -> List.fold_left f x xs
    end

  (* リスト内の最初の1個目のxを削除する *)
  let rec deleteFirst (x:'a) (ys:'a list): 'a list = 
    begin match ys with
      | [] -> []
      | y :: ys ->
        if x = y then ys
        else y :: deleteFirst x ys
    end
  (* 最初のリストから2番目のリストの要素を消す *)
  let rec diff (xs:'a list) (ys:'a list): 'a list =
    begin match ys with
      | [] -> xs
      | y :: ys -> diff (deleteFirst y xs) ys
    end

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

let _ =
  let a = [5;4;3;2;1] in
  let b = [4;5;6;7] in
  let ab = Pre.union a b in
  Printf.printf "union a b = %s\n" (Pre.show_int_list ab);
  let ab = Pre.intersect a b in
  Printf.printf "intersect a b = %s\n" (Pre.show_int_list ab);
  let a = [1;1;2;2;3;4;5;1] in
  let a = Pre.nub a in
  Printf.printf "nub a b = %s\n" (Pre.show_int_list a);
  let a = [] in
  let a = Pre.isEmpty a in
  Printf.printf "isEmpty = %b\n" a;

  let a = [1] in
  let a = Pre.isEmpty a in
  Printf.printf "isEmpty = %b\n" a;

  let a = [1;2;3;4;5;6;7;8;9;10] in
  let a = Pre.fold_left1 begin fun a b ->
    a + b
  end a in
  Printf.printf "fold_left1 = %d\n" a;

  let a = [1;2;3;4;3;4;5] in
  let a = Pre.deleteFirst 3 a in
  Printf.printf "deleteFirst = %s\n" (Pre.show_int_list a);

  let a = [1;2;3;4] in
  let b = [3;4;5;6] in
  let a = Pre.diff a b in
  Printf.printf "diff = %s\n" (Pre.show_int_list a);

  let a = [1;2;3;4;3;4] in
  let b = [3;4;5;6;4] in
  let a = Pre.diff a b in
  Printf.printf "diff = %s\n" (Pre.show_int_list a);

  let a1 = [1,10,100;2,20,200;3,30,300] in
  let a,b,c = Pre.split3 a1 in
  Printf.printf "split3 = %s;%s;%s\n"
    (Pre.show_int_list a)
    (Pre.show_int_list b)
    (Pre.show_int_list c);
