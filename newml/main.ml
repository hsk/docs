let parse input =
  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in inp;
  ast  



let to_string ast =
  Gen_ml.print_prog Format.std_formatter ast

let parse_str str =
  let lexbuf = Lexing.from_string str in
  Parser.prog Lexer.token lexbuf
  

let trans input output =
  let ast = parse input in

  let out = open_out output in
  Gen_ml.print_prog (Format.formatter_of_out_channel out) ast;
  close_out out

let _ =
  if (Array.length Sys.argv) > 2 then
    trans Sys.argv.(1) Sys.argv.(2)
  else
    trans "test.ml1" "test.ml"

(*|

    >>> open Ast;;
    
    >>> TEmpty;;
    - : Ast.t = TEmpty

    >>> EEmpty;;
    - : Ast.e = EEmpty

    >>> EInt 1;;
    - : Ast.e = EInt 1

    >>> open Parser;;
    
    >>> open Lexer;;
    
    
    >>> #load "parser.cmo";;
    
    >>> open Main;;
    
    >>> #load "main.cmo";;
    
    >>> let ast = parse "test.ml1";;
    val ast : Ast.prog = Prog [SOpen "Printf"; SOpen "List"; SExp (ELet ("a", TEmpty, EInt 1)); SExp (ELet ("b", Ty "int", EBin (EInt 1, "+", EInt 2))); SExp (ECall (EVar "printf", [EString "\"%d\\n\""; EBin (EVar "a", "+", EVar "b")])); SExp (ECall (EVar "printf", [EIf (EBin (EVar "a", "<", EInt 1), EString "\"a\\n\"", EString "\"b1\\n\"")])); SExp (EIf (EBin (EVar "a", "<", EInt 10), EBlock [ECall (EVar "printf", [EString "\"b2\\n\""])], EEmpty)); SExp (EIf (EBin (EVar "a", ">", EInt 10), ECall (EVar "printf", [EString "\"a\\n\""]), ECall (EVar "printf", [EString "\"b3\\n\""]))); SExp (ELet ("add1", TFun (Ty "int", Ty "int"), EFun ([EVar "a"], TEmpty, EBlock [EBin (EVar "a", "+", EInt 1)]))); SExp (ELet ("c", TEmpty, ECall (EVar "add1", [EInt 10]))); SExp (ECall (EVar "printf", [EString "\"10+1=%d\\n\""; EVar "c"])); SExp (ECall (EVar "printf", [EString "\"10+1=%d\\n\""; ECall (EVar "add1", [EInt 10])])); SExp (ELet ("add", TEmpty, EFun ([EVar "a"; EVar "b"], TEmpty, EBlock [EBin (EVar "a", "+", EVar "b")]))); SExp (ELet ("add3", TFun (Ty "int", TFun (Ty "int", TFun (Ty "int", Ty "int"))), EFun ([EVar "a"; EVar "b"; EVar "c"], TEmpty, EBlock [EBin (EBin (EVar "a", "+", EVar "b"), "+", EVar "c")]))); SExp (ELet ("addt", TFun (TTuple [Ty "int"; Ty "int"], Ty "int"), EFun ([EBin (EVar "a", ",", EVar "b")], TEmpty, EBlock [EBin (EVar "a", "+", EVar "b")]))); SExp (ELetRec ("fib", TEmpty, EPFun [EFun ([EInt 0], TEmpty, EBlock [EInt 1]); EFun ([EInt 1], TEmpty, EBlock [EInt 1]); EFun ([EVar "n"], TEmpty, EBlock [EBin (ECall (EVar "fib", [EBin (EVar "n", "-", EInt 2)]), "+", ECall (EVar "fib", [EBin (EVar "n", "-", EInt 1)]))])])); SExp (ELetRec ("fib2", TEmpty, EFun ([EVar "n"], TEmpty, EBlock [EMatch (EVar "n", [EFun ([EInt 0], TEmpty, EBlock [...]); ...]); ...]))); ...]

    >>> to_string ast;;
    open Printf;; open List;; let a = 1;; let (b:int) = (1 + 2);; printf("%d\n") ((a + b));; printf((if (a < 1) then ("a\n")else("b1\n")));; (if (a < 10) then (printf("b2\n"); ););; (if (a > 10) then (printf("a\n"))else(printf("b3\n")));; let (add1:(int)->(int)) = (fun ( a) -> (a + 1); );; let c = add1(10);; printf("10+1=%d\n") (c);; printf("10+1=%d\n") (add1(10));; let add = (fun ( a) ( b) -> (a + b); );; let (add3:(int)->((int)->((int)->(int)))) = (fun ( a) ( b) ( c) -> ((a + b) + c); );; let (addt:((int * int))->(int)) = (fun ( (a , b)) -> (a + b); );; let rec fib = (function | ( 0) -> (1; )| ( 1) -> (1; )| ( n) -> ((fib((n - 2)) + fib((n - 1))); ));; let rec fib2 = (fun ( n) -> (match n with | ( 0) -> (1; )| ( 1) -> (1; )| ( n) -> ((fib2((n - 2)) + fib2((n - 1))); )); );; let rec llor = (function | ( (0 , 0)) -> ((a lor b); )| ( (a , b)) -> ((a lor b); ));; let main = (fun ( ()) -> printf("add 1 2 = %d\n") (add(1) (2)); printf("addt 1,2 = %d\n") (addt((1 , 2))); printf("fib 10 = %d\n") (fib(10)); printf("llor %d\n") (llor((1 , 2))); let a = 12 in let c = 2 in let b = (c + a) in printf("%d\n") (b); );; main(());; let (_:unit) = printf("test222\n"); iter((fun ( x) -> printf("%d\n") (x); ))([1; 2; 3; 4000]); let ls = map((fun ( x) -> (x * 10); ))([1; 2; 3; 4; 5]) in iter((fun ( x) -> printf("%d\n") (x); ))(ls); iter((fun ( x) -> printf("%d\n") (x); ))(ls); iter((fun ( x) -> printf("%d\n") (x); )) (ls); ;; type a = {x:int;y:int};; let aa = {x=1;y=2};; printf("%d\n") ((aa . x));; printf("%d\n") (({x=1;y=2} . x));; let aa = (fun ({x}) -> printf("%d\n") (x); );; type k = KInt of (int)|KAdd of (k * k);; let rec eval = (function | ( KInt(i)) -> (i; )| ( KAdd((a , b))) -> (let a = eval(a) in let b = eval(b) in (a + b); ));; (a , let b = (1 , 2));; printf("1+2=%d\n") (eval(KAdd((KInt(1) , KInt(2)))));; - : unit = ()

    >>> parse_str "1 ";;
    - : Ast.prog = Prog [SExp (EInt 1)]

    >>> parse_str "open A";;
    - : Ast.prog = Prog [SOpen "A"]

    >>> parse_str "a := 1 b := 2 printf(a + b)";;
    - : Ast.prog = Prog [SExp (ELet ("a", TEmpty, EInt 1)); SExp (ELet ("b", TEmpty, EInt 2)); SExp (ECall (EVar "printf", [EBin (EVar "a", "+", EVar "b")]))]

    >>> let ast = parse_str "a(a,b,c d e f)";;
    val ast : Ast.prog = Prog [SExp (ECall (EVar "a", [EBin (EBin (EVar "a", ",", EVar "b"), ",", EVar "c"); EVar "d"; EVar "e"; EVar "f"]))]

    >>> to_string ast;;
    a(((a , b) , c)) (d) (e) (f);; - : unit = ()

    >>> let ast = parse_str "a(b (c))";;
    val ast : Ast.prog = Prog [SExp (ECall (EVar "a", [ECall (EVar "b", [EVar "c"])]))]

    >>> parse_str "b,c := (1,2)";;
    - : Ast.prog = Prog [SExp (EBin (EVar "b", ",", ELet ("c", TEmpty, EBin (EInt 1, ",", EInt 2))))]


*)