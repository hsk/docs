open Vm
open Printf

let _ =
  let _ =
    printf("Test 1: Objects on stack are preserved.\n");
    let vm = newVM [||] in
    pushInt(vm, 1);
    pushInt(vm, 2);
    gc(vm);
    assert(vm.num = 2);
    freeVM(vm);
  in
  let _ =
    printf("Test 2: Unreached objects are collected.\n");
    let vm = newVM [||] in
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
    let vm = newVM [||] in
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
    let vm = newVM [||] in
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
    let vm = newVM [||] in
    
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
  in
  let _ =
    printf("run VM\n");
    assert (1 = run [|
      CPush 1;
      CHalt;
    |]);
    assert (3 = run [|
      CPush 1;
      CPush 2;
      CAdd;
      CHalt;
    |]);
    assert (200 = run [|
      CPush 10;
      CPush 20;
      CMul;
      CHalt;
    |]);
    assert (111 = run [|
      CCall(2,0,0);
      CHalt;
      CPush 111;
      CRet;
    |]);
    assert (55555 = run [|
      CCall(2,0,1);
      CHalt;
      CPush 55555;
      CStore 0;
      CLoad 0;
      CRet;
    |]);
    assert (55555 = run [|
      CCall(2,0,2);
      CHalt;
      CPush 55555;
      CStore 1;
      CLoad 1;
      CRet;
    |]);
    assert (123 = run [|
      CPush 123;
      CCall(4,0,1);
      CPrint;
      CHalt;
      CPush 55555;
      CStore 0;
      CLoad 0;
      CRet;
    |]);

    assert (123 = run [|
      CPush 123;
      CCall(3,1,0);
      CHalt;
      CLoad 0;
      CRet;
    |]);

    assert (300 = run [|
      CPush 100;
      CPush 200;
      CCall(4,2,0);
      CHalt;
      CLoad 0;
      CLoad 1;
      CAdd;
      CRet;
    |]);

    assert (300 = run [|
      CPush 100;
      CPush 200;
      CCall(4,2,1);
      CHalt;
      CLoad 0;
      CLoad 1;
      CAdd;
      CStore 2;
      CLoad 2;
      CRet;
    |]);

    assert (300 = run [|
      CPush 100;
      CPush 200;
      CCall(4,2,1);
      CHalt;
      CLoad 0;
      CLoad 1;
      CAdd;
      CStore 2;
      CLoad 2;
      CRet;
    |]);

    assert (123 = run [|
      CPush 1;
      CPush 2;
      CAdd;
      CPrint;
      CPush 10;
      CCall(7,1,0);
      CHalt;

      CLoad 0;
      CPush 20;
      CAdd;
      CPrint;
      CPush 123;
      CRet;
    |]);

    assert (1 = run [|
      CPush 100;
      CPush 200;
      CPair;
      CPrint;
      CPush 1;
      CHalt;
    |]);

    assert (100 = run [|
      CPush 100;
      CPush 200;
      CPair;
      CHead;
      CHalt;
    |]);

    assert (200 = run [|
      CPush 100;
      CPush 200;
      CPair;
      CTail;
      CHalt;
    |]);

    assert (200 = run [|
      CCall(2,0,1);
      CHalt;
      CPush 100;
      CPush 200;
      CPair;
      CTail;
      CRet;
    |]);

    assert (200 = run [|
      CCall(2,0,1);
      CHalt;
      CPush 100;
      CPush 200;
      CPair;
      CStore 0;
      CLoad 0;
      CTail;
      CRet;
    |]);

    assert (300 = run [|
      CCall(2,0,1);
      CHalt;
      CPush 100;
      CPush 200;
      CPair;
      CStore 0;
      CLoad 0;
      CTail;
      CLoad 0;
      CHead;
      CAdd;
      CRet;
    |]);

    assert (300 = run [|
      CPush 100;
      CPush 200;
      CPair;
      CCall(5,1,1);
      CHalt;
      CLoad 0;
      CTail;
      CLoad 0;
      CHead;
      CAdd;
      CRet;
    |]);

    assert (100 = run [|
      CPush 100;
      CNop;
      CHalt;
    |]);

    assert (11 = run [|
      CJmp 1;
      CPush 11;
      CHalt;
    |]);

    assert (100 = run [|
      CPush 100;
      CJmp 3;
      CPush 11;
      CHalt;
    |]);

    assert (22 = run [|
      CPush 0;
      CBne 4;
      CPush 11;
      CJmp 5;
      CPush 22;
      CHalt;
    |]);

    assert (11 = run [|
      CPush 111;
      CBne 4;
      CPush 11;
      CJmp 5;
      CPush 22;
      CHalt;
    |]);
    (* nested call test *)
    assert (1 = run [|
      CCall(2,0,0);
      CHalt;
      CPush 1;
      CRet;
    |]);
    (* nested call test *)

    printf "--------- test nested param 1 1;\n";
    assert (30 = run [|
      CCall(2,0,0);
      CHalt;
      CCall(4,0,0);
      CRet;
      CPush 30;
      CRet;
    |]);

    printf "--------- test nested param 1 1;\n";
    assert (30 = run [|
      CCall(2,0,0);
      CHalt;
      CPush 20;
      CCall(5,1,0);
      CRet;
      CPush 30;
      CRet;
    |]);

    printf "--------- test 2;\n";
    (* nested call test *)
    assert (30 = run [|
      CPush 20;
      CPush 20;
      CCall(4,2,0);
      CHalt;
      CPush 30;
      CRet;
    |]);

    printf "--------- test 3;\n";
    (* nested call test *)
    assert (30 = run [|
      CCall(2,0,0);
      CHalt;
      CPush 20;
      CPush 20;
      CCall(6,2,0);
      CRet;
      CPush 30;
      CRet;
    |]);
    printf "--------- test 4;\n";
    (* nested call test *)
    assert (50 = run [|
      CCall(2,0,0);
      CHalt;
      CPush 20;
      CPush 30;
      CCall(6,2,0);
      CRet;
      CLoad 1;
      CLoad 0;
      CAdd;
      CRet;
    |]);

    printf "--------- test 5;\n";
    (* nested call test *)
    assert (50 = run [|
      CNop;
      CNop;
      CCall(4,0,0);
      CHalt;
      CPush 20;
      CPush 30;
      CCall(8,2,0);
      CRet;
      CLoad 1;
      CLoad 0;
      CAdd;
      CRet;
    |]);
    printf "--------- test 6;\n";
    (* nested call test *)
    assert (50 = run [|
      CPush 20;
      CPush 10;
      CCall(4,2,0);
      CHalt;
      CPush 20;
      CPush 30;
      CCall(8,2,0);
      CRet;
      CLoad 1;
      CLoad 0;
      CAdd;
      CRet;
    |]);

    printf "--------- test 6;\n";
    (* nested call test *)
    assert (30 = run [|
      CPush 20;
      CPush 10;
      CCall(4,2,0);
      CHalt;
      CLoad 0;
      CLoad 1;
      CCall(8,2,0);
      CRet;
      CLoad 1;
      CLoad 0;
      CAdd;
      CRet;
    |]);

    printf("test ok\n");
  in ()