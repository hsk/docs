open Printf

module VM = struct

  type pair = {mutable head:obj; mutable tail:obj}
  and obj = Int of int | Pair of pair
  module OSet = Set.Make(struct type t = obj let compare a b = (Obj.magic a) - (Obj.magic b) end)

  type vm = {
    mutable stack : obj list;
    mutable objs: obj list;
    mutable num : int;
    mutable max : int;
  }

  let newVM () = {stack=[];objs=[];num=0;max=8}

  let push(vm, value) =
    vm.stack <- value::vm.stack

  let pop vm =
    match vm.stack with
    | [] -> assert false
    | x::xs -> vm.stack<-xs; x

  let gc vm =
    let rec mark set obj = 
      if OSet.mem obj set then set
      else
        match (OSet.add obj set, obj) with
        | (set,Pair{head;tail}) ->
          mark (mark set head) tail
        | (set,_) -> set
    in
    let markAll vm = List.fold_left mark OSet.empty vm.stack in
    let sweep set vm =
      vm.objs <-
        List.fold_left (fun ls obj ->
          if OSet.mem obj set then obj::ls else (vm.num <- vm.num - 1; ls)
        ) [] vm.objs
    in
    let num = vm.num in
    let set = markAll(vm) in
    sweep set vm;
    vm.max <- vm.num * 2;
    printf "Collected %d objects, %d remaining.\n" (num - vm.num) vm.num

  let newObject(vm, obj) =
    if (vm.num = vm.max) then gc vm;
    vm.objs <- obj :: vm.objs;
    vm.num <- vm.num + 1;
    obj

  let pushInt(vm, intValue) =
    push(vm, newObject(vm, Int(intValue)))

  let pushPair(vm) =
    let tail = pop vm in
    let head = pop vm in
    let obj = newObject(vm, Pair{head; tail}) in
    push(vm, obj); obj

  let freeVM vm =
    vm.stack <- [];
    gc(vm)
end

open VM

let _ =
  let _ =
    printf("Test 1: Objects on stack are preserved.\n");
    let vm = newVM() in
    pushInt(vm, 1);
    pushInt(vm, 2);
    gc(vm);
    assert(vm.num = 2);
    freeVM(vm)
  in
  let _ =
    printf("Test 2: Unreached objects are collected.\n");
    let vm = newVM() in
    pushInt(vm, 1);
    pushInt(vm, 2);
    ignore (pop vm);
    ignore (pop vm);

    gc(vm);
    assert(vm.num = 0);
    freeVM(vm)
  in
  let _ =
    printf("Test 3: Reach nested objects.\n");
    let vm = newVM() in
    pushInt(vm, 1);
    pushInt(vm, 2);
    ignore (pushPair vm);
    pushInt(vm, 3);
    pushInt(vm, 4);
    ignore (pushPair vm);
    ignore (pushPair vm);

    gc(vm);
    assert(vm.num = 7);
    freeVM(vm)
  in
  let _ =
    printf("Test 4: Handle cycles.\n");
    let vm = newVM() in
    pushInt(vm, 1);
    pushInt(vm, 2);
    let a = pushPair(vm) in
    pushInt(vm, 3);
    pushInt(vm, 4);
    let b = pushPair(vm) in
    begin match a with
    | Pair pair -> pair.tail <- b;
    | _ -> assert false
    end;
    begin match b with
    | Pair pair -> pair.tail <- a;
    | _ -> assert false
    end;
    gc(vm);
    assert(vm.num = 4);
    freeVM(vm)
  (*
  in
  let _ =
    printf("Performance Test.\n");
    let vm = newVM() in
    
    for i = 0 to 1000 - 1 do 
      for j = 0 to 20 - 1 do
        pushInt(vm, i)
      done;
      for j = 0 to 20 - 1 do
        pop(vm)
      done
    done;
    freeVM(vm)
  *)
  in ()

