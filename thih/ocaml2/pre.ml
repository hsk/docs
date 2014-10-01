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
  
  let ls =  List.fold_left begin fun ys x ->
      if List.mem x ys
      then ys
      else x :: ys
    end [] xs
  in
  List.rev ls

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

(* 空チェック *)
let isEmpty(xs: 'a list):bool =
  begin match xs with
    | [] -> true
    | _ -> false
  end
(* たぶんこれは、reduceじゃないのかな *)
let fold_left1 (f:'a -> 'a -> 'a) (xs:'a list): 'a = 
  match xs with
  | [] -> invalid_arg "empty list"
  | [x] -> x
  | x :: xs -> List.fold_left f x xs

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
  let rec loop ((ws:'a list), (xs:'b list), (ys:'c list))
    (zs:('a * 'b * 'c)list):('a list * 'b list * 'c list) =
    begin match zs with
      | [] -> (List.rev ws, List.rev xs, List.rev ys)
      | (w, x, y) :: zs -> loop (w :: ws, x :: xs, y :: ys) zs
    end
  in
  loop ([], [], []) xs

(*|

    >>> open Pre;;

## union

    >>> (Pre.union [1;2] [2; 3]);;
    - : int list = [1; 2; 3]

## intersect

    >>> (Pre.intersect [1;2] [2; 3]) ;;
    - : int list = [2]

## union and intersect

    >>> let a = [5;4;3;2;1];;
    val a : int list = [5; 4; 3; 2; 1]

    >>> let b = [4;5;6;7];;
    val b : int list = [4; 5; 6; 7]

    >>> let ab = Pre.union a b;;
    val ab : int list = [3; 2; 1; 4; 5; 6; 7]
    
    >>> ab;;
    - : int list = [3; 2; 1; 4; 5; 6; 7]

    >>> let ab = Pre.intersect a b;;
    val ab : int list = [5; 4]

## nub

    >>> let a = [1;1;2;2;3;4;5;1];;
    val a : int list = [1; 1; 2; 2; 3; 4; 5; 1]

    >>> let a2 = Pre.nub a;;
    val a2 : int list = [1; 2; 3; 4; 5]

## is empty 1

    >>> let a = [];;
    val a : 'a list = []

    >>> Pre.isEmpty a;;
    - : bool = true

## is empty 2

    >>> let a = [1];;
    val a : int list = [1]

    >>> Pre.isEmpty a;;
    - : bool = false

## fold_left1

    >>> let a = [1;2;3;4;5;6;7;8;9;10];;
    val a : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

    >>> let a = Pre.fold_left1 begin fun a b -> a + b end a;;
    val a : int = 55

## deleteFirst

    >>> let a = [1;2;3;4;3;4;5] ;;
    val a : int list = [1; 2; 3; 4; 3; 4; 5]

    >>> let a = Pre.deleteFirst 3 a ;;
    val a : int list = [1; 2; 4; 3; 4; 5]

## diff

    >>> let a = [1;2;3;4];;
    val a : int list = [1; 2; 3; 4]

    >>> let b = [3;4;5;6];;
    val b : int list = [3; 4; 5; 6]

    >>> let a = Pre.diff a b;;
    val a : int list = [1; 2]

## diff 2

    >>> let a = [1;2;3;4;3;4];;
    val a : int list = [1; 2; 3; 4; 3; 4]

    >>> let b = [3;4;5;6;4];;
    val b : int list = [3; 4; 5; 6; 4]

    >>> let a = Pre.diff a b;;
    val a : int list = [1; 2; 3]

## split3

    >>> let a1 = [1,10,100;2,20,200;3,30,300];;
    val a1 : (int * int * int) list = [(1, 10, 100); (2, 20, 200); (3, 30, 300)]

    >>> let (a,_,_) = Pre.split3 a1;;
    val a : int list = [1; 2; 3]

    >>> let (_,b,_) = Pre.split3 a1;;
    val b : int list = [10; 20; 30]

    >>> let (_,_,c) = Pre.split3 a1;;
    val c : int list = [100; 200; 300]

    >>> a ;;
    - : int list = [1; 2; 3]

    >>> b;;
    - : int list = [10; 20; 30]

    >>> c;;
    - : int list = [100; 200; 300]

*)
