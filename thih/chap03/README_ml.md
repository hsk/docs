# 3 カインド

## Kindモジュール

	module Kind = struct
	...
	end

### type kind

	  type kind =
	    | Star
	    | Kfun of kind * kind

### show関数

	  let rec show (k:kind):string =
	    begin match k with
	      | Star -> "*"
	      | Kfun(Kfun _ as k1,k2) -> Printf.sprintf "(%s) -> %s" (show k1) (show k2) 
	      | Kfun(k1,k2) -> Printf.sprintf "%s -> %s" (show k1) (show k2) 
	    end

#### 使用例

	let _ =
	  let star = Kind.Star in
	  let kfun1 = Kind.Kfun(Kind.Star, Kind.Star) in
	  let kfun2 = Kind.Kfun(Kind.Star, Kind.Kfun(Kind.Star, Kind.Star)) in
	  let kfun3 = Kind.Kfun(Kind.Star, Kind.Kfun(Kind.Star, Kind.Kfun(Kind.Star, Kind.Star))) in
	  let kfun4 = Kind.Kfun(Kind.Kfun(Kind.Star, Star), Kind.Kfun(Kind.Star, Star)) in
	  let kfun5 = Kind.Kfun(Kind.Star,Kind.Kfun(Kind.Kfun(Kind.Star, Star), Star)) in
	  Printf.printf "star = %s\n" (Kind.show star);
	  Printf.printf "kfun1 = %s\n" (Kind.show kfun1);
	  Printf.printf "kfun2 = %s\n" (Kind.show kfun2);
	  Printf.printf "kfun3 = %s\n" (Kind.show kfun3);
	  Printf.printf "kfun4 = %s\n" (Kind.show kfun4);
	  Printf.printf "kfun5 = %s\n" (Kind.show kfun5);
