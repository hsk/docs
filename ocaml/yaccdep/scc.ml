module Int = struct                       
   type t = int                                              
   let compare x y = if x < y then -1 else if x > y then 1 else 0
end

module M = Map.Make(String)
module S = Set.Make(String)

let mkMap ls =
  List.fold_left (fun m (k,v) -> M.add k v m ) M.empty ls

let mkSet ls =
  List.fold_left (fun m v -> S.add v m ) S.empty ls

let mkMap2 ls =
  List.fold_left (fun m (k,v) -> M.add k (mkSet v) m ) M.empty ls

let print_map rG =
  List.iter(fun (k,l) -> 
    Printf.printf "%s -> [" k;
    List.iter(fun l -> 
      Printf.printf "%s," l
    ) (S.elements l);
    Printf.printf "]\n"

  ) (M.bindings rG);
  Printf.printf "\n"


let scc g =

  let rg g =

    M.fold (fun k set map ->
      let map2 = 
        if M.mem k map
        then map
        else M.add k S.empty map
      in
      S.fold (fun v map ->
        let s = if M.mem v map
          then S.add k (M.find v map)
          else S.singleton k
        in
        M.add v s map
      ) set map2
    ) g M.empty
  in
  let rG = rg g in

  Printf.printf "rg=\n";
  print_map rG;

  let rec dfs g ls r =
    List.fold_left (fun (g,r) x ->
        if not (M.mem x g) then
          (g,r)
        else
          let (g3,r3) = dfs (M.remove x g) (S.elements (S.remove x (M.find x g)))  r in
          (g3,x::r3)
    ) (g,r) ls
  in
  let (map,vs) = dfs g (List.map (fun (v, _) -> v) (M.bindings g)) [] in

  Printf.printf "vs=";
  List.iter(fun v-> Printf.printf "%s," v) vs;
  Printf.printf "\n";

  let rec rdfs g v ls =
    if not (M.mem v g) then (g, ls)
    else
      S.fold (
        fun  v (rg,ls) -> rdfs rg v ls
      ) (M.find v g) (M.remove v g, v :: ls)
  in
  let (u2,ls) = List.fold_left
    (fun (rg,ls) v ->
        let (rg2,l) = rdfs rg v [] in
        (rg2, if(l=[]) then ls else l::ls)
    ) (rG,[]) vs
  in
  ls
(*
let _ =
  let map = mkMap2 [
      "0",[];
      "1",["2"];
      "2",["1"];
      "3",["4"];
      "4",[]
    ]
  in
  let ls = scc(map) in
  List.iter(fun l -> 
    List.iter(fun l -> 
      Printf.printf "%s," l
    ) l;
    Printf.printf "\n"

  ) ls
*)