## OCamlあたりで作る。

今のプログラムをOCaml化してみましょう。

```
open Printf

module VM = struct
  type obj = {tag:tag; mutable marked: bool}
  and pair = {mutable head:obj; mutable tail:obj}
  and tag = Int of int | Pair of pair

  type vm = {
    mutable stack : obj list;
    mutable objs: obj list;
    mutable num : int;
    mutable max : int;
  }

  let newVM () = {stack=[];objs=[];num=0;max=8}

  let push(vm, value) =
    vm.stack <- value::vm.stack

  let pop(vm) =
    match vm.stack with
    | [] -> assert false
    | x::xs -> vm.stack<-xs; x

  let gc vm =
    let rec mark = function
      | {marked=true} -> ()
      | obj ->
        obj.marked <- true;
        match obj.tag with
        | Pair{head;tail} -> mark(head); mark(tail)
        | _ -> ()
    in
    let markAll vm = List.iter mark vm.stack in
    let sweep vm =
      vm.objs <-
        List.fold_left (fun ls -> function
          | {marked=false} -> vm.num <- vm.num - 1; ls
          | obj -> obj.marked <- false; obj::ls
        ) [] vm.objs
    in
    let num = vm.num in
    markAll(vm);
    sweep(vm);
    vm.max <- vm.num * 2;
    printf "Collected %d objects, %d remaining.\n" (num - vm.num) vm.num

  let newObject(vm, tag) =
    if (vm.num = vm.max) then gc vm;
    let obj = {tag=tag; marked=false} in
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
    begin match a.tag with
    | Pair pair -> pair.tail <- b;
    | _ -> assert false
    end;
    begin match b.tag with
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
```
