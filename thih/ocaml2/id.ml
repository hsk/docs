type id = string

(* 数値に対するidを取得する *)
let enumId (n:int) : id =
"v" ^ string_of_int n

(*|
# Id

	>>> open Id;;
	
## enumId 1

    >>> Id.enumId 1;;
	- : Id.id = "v1"

## enumId 33
    >>> Id.enumId 33;;
	- : Id.id = "v33"

*)
