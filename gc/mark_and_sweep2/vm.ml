open Printf


type c =
  | CNop
  | CPush of int
  | CPair
  | CHead
  | CTail
  | CAdd
  | CMul
  | CStore of int
  | CLoad of int
  | CJmp of int
  | CBne of int
  | CRet
  | CCall of int * int * int
  | CHalt
  | CPrint

type obj = {tag:tag; mutable marked: bool}
and pair = {mutable head:obj; mutable tail:obj}
and tag = Int of int | Pair of pair | Nil

type vm = {
  mutable stack : obj array;
  mutable objs: obj list;
  mutable num : int;
  mutable max : int;
  mutable codes: c array;
  mutable pc : int;
  mutable sp : int;
  mutable bp : int;
}
let nil = {tag=Nil; marked=false}
let newVM codes = {stack=Array.make 10000 nil;objs=[];num=0;max=8;codes=codes;pc=0;sp=0;bp=0;}

let push(vm, value) =
  vm.stack.(vm.sp) <- value;
  vm.sp <- vm.sp + 1

let pop(vm) =
  vm.sp <- vm.sp - 1;
  vm.stack.(vm.sp)

let gc vm =
  let rec mark = function
    | {marked=true} -> ()
    | obj ->
      obj.marked <- true;
      match obj.tag with
      | Pair{head;tail} -> mark(head); mark(tail)
      | _ -> ()
  in
  let markAll vm =
    for i = 0 to vm.sp - 1 do
      mark vm.stack.(i)
    done;
  in
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

let popInt(vm) =
    begin match pop vm with
      | {tag=Int i1} -> i1
      | _ -> assert false
    end

let freeVM vm =
  vm.sp <- 0;
  gc(vm)

exception Shutdown

let rec sprint_v = function
| {tag=Nil} -> sprintf "nil"
| {tag=Int i} -> sprintf "%d" i
| {tag=Pair{head;tail}} -> sprintf "(%s, %s)" (sprint_v head) (sprint_v tail)

let print_v v = printf "%s\n" (sprint_v v)

let run codes =
  printf "start vm\n";
  let vm = newVM codes in
  vm.codes <- codes;
  let rec loop () =
    printf("loop vm.sp=%d bp=%d pc=%d\n") vm.sp vm.bp vm.pc;
    let c = vm.codes.(vm.pc) in
    vm.pc <- vm.pc + 1;

    if c = CHalt then popInt(vm) else begin
      begin match c with
      | CNop -> ()
      | CPush(i) -> pushInt(vm, i)
      | CPair -> ignore(pushPair(vm))
      | CHead ->
        begin match pop(vm) with
          | {tag=Pair{head;tail}} ->
            ignore(push(vm, head))
          | _ -> assert false
        end
      | CTail ->
        begin match pop(vm) with
          | {tag=Pair{head;tail}} ->
            ignore(push(vm, tail))
          | _ -> assert false
        end

      | CAdd ->
        let i1 = popInt(vm) in
        let i2 = popInt(vm) in
        pushInt(vm, i1 + i2)
      | CMul ->
        let i1 = popInt(vm) in
        let i2 = popInt(vm) in
        pushInt(vm, i1 * i2)
      | CStore i ->
        printf("cstore\n");
        vm.stack.(vm.bp + i) <- pop vm
      | CLoad i ->
        printf("cload\n");
        let v = vm.stack.(vm.bp + i) in
        print_v v;
        push(vm, v)
      | CJmp i -> vm.pc <- i
      | CBne i -> if popInt(vm) = 0 then vm.pc <- i else ()
      | CRet ->
        printf "ret\n";
        let rc = pop vm in
        vm.pc <- popInt vm;
        vm.bp <- popInt vm;
        vm.sp <- popInt vm;
        push (vm, rc);
      | CCall(func, arg_size, local_size)->
        printf "ccall(%d,%d,%d)\n" func arg_size local_size;
        let bp = vm.sp - arg_size in
        vm.sp <- vm.sp + local_size;
        pushInt(vm, bp);
        pushInt(vm, vm.bp);
        pushInt(vm, vm.pc);
        vm.bp <- bp;
        vm.pc <- func
      | CPrint ->
        printf("cprint\n");
        print_v(pop(vm))
      | _ -> assert false
      end;
      loop()
    end
  in
    let r = loop() in
    freeVM(vm);
    printf "end vm\n";
    r
