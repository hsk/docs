open Printf;;
open List;;
let (b:int) = 2;;
let a = 1;;
let (_:unit) = printf("test222\n");
;;
let _ = printf("test222\n");
;;
let _ = let (f0:(unit)->(int)) = (fun ( ()) -> 0;
) in
let (f0:(unit)->(int)) = (fun ( ()) -> 0;
) in
let (f0:(unit)->(int)) = (fun ( ()) -> 0) in
let f0 = (fun ( ()) -> 0) in
printf("%d\n")(f0(()));
printf("%d\n") (f0(()));
printf("%d\n") (f0(()));
let (f1:((int))->(int)) = (fun ( a) -> a;
) in
let (f1:(int)->(int)) = (fun ( a) -> a;
) in
let (f1:(int)->(int)) = (fun ( a) -> a) in
let f1 = (fun ( a) -> a) in
printf("%d\n")(f1(1));
printf("%d\n") (f1(1));
printf("%d\n") (f1(1));
let (f2:((int))->(((int))->(int))) = (fun ( a) ( b) -> (a + b);
) in
let (f2:(int)->(((int))->(int))) = (fun ( a) -> (fun ( b) -> (a + b);
)) in
let (f2:(int)->((int)->(int))) = (fun ( a) -> (fun ( b) -> (a + b);
)) in
let (f2:(int)->((int)->(int))) = (fun ( a) ( b) -> (a + b)) in
let f2 = (fun ( a) ( b) -> (a + b)) in
printf("%d\n")(f2(1)(2));
printf("%d\n") (f2(1) (2));
printf("%d\n") (f2(1) (2));
let (f3:((int))->(((int))->(((int))->(int)))) = (fun ( a) ( b) ( c) -> ((a + b) + c);
) in
let (f3:(int)->(((int))->(((int))->(int)))) = (fun ( a) -> (fun ( b) ( c) -> ((a + b) + c);
)) in
let (f3:(int)->((int)->(((int))->(int)))) = (fun ( a) -> (fun ( b) -> (fun ( c) -> ((a + b) + c);
))) in
let (f3:(int)->((int)->((int)->(int)))) = (fun ( a) -> (fun ( b) -> (fun ( c) -> ((a + b) + c);
))) in
let (f3:(int)->((int)->((int)->(int)))) = (fun ( a) ( b) ( c) -> ((a + b) + c)) in
let f3 = (fun ( a) ( b) ( c) -> ((a + b) + c)) in
printf("%d\n")(f3(1)(2)(3));
printf("%d\n") (f3(1) (2) (3));
printf("%d\n") (f3(1) (2) (3));
printf("%d %d %d %d\n")(f0(()))(f1(1))(f2(1)(2))(f3(1)(2)(3));
printf("%d %d %d %d\n") (f0(())) (f1(1)) (f2(1) (2)) (f3(1) (2) (3));
printf("%d %d %d %d\n") (f0(())) (f1(1)) (f2(1) (2)) (f3(1) (2) (3));
printf("%d\n")(f3((- 1))((- 2))((- 3)));
printf("%d\n") (f3((- 1)) ((- 2)) ((- 3)));
printf("%d\n") (f3((- 1)) ((- 2)) ((- 3)));
printf("%d+%d=%d\n")(a)(b)((a + b));
printf("%d+%d=%d\n") (a) (b) ((a + b));
printf("%d+%d=%d\n") (a) (b) ((a + b));
;;
let _ = (if (a < 10) then (printf("b1\n")););
(if (a < 10) then (printf("b2\n");
););
(if (a < 10) then (printf("b2\n");
printf("b3\n");
););
(if (a > 10) then (printf("a\n"))else(printf("b3\n")));
printf((if (a < 1) then ("a\n")else("b1\n")));
;;
let _ = let rec (fib:((int))->(int)) = (fun ( n) -> (if (n = 0) then (0)else((if (n = 1) then (1)else((fib((n - 2)) + fib((n - 1)))))));
) in
printf("fib 10 %d\n") (fib(10));
;;
let _ = let (addt:((int * int))->(int)) = (fun ( (a , b)) -> (a + b);
) in
printf("%d\n") (addt((1 , 2)));
let (f2:((int * int))->(((int * int))->(int))) = (fun ( (a , b)) ( (c , d)) -> ((a * b) + (c * d));
) in
printf("%d\n")(f2((1 , 2))((3 , 4)));
printf("%d\n") (f2((1 , 2)) ((3 , 4)));
;;
let _ = let rec (fib:((int))->(int)) = (fun ( n) -> (match n with | ( 0) -> (0;
)| ( 1) -> (1;
)| ( n) -> ((fib((n - 2)) + fib((n - 1)));
));
) in ()
;;
let _ = let rec (fib:((int))->(int)) = (function | ( 0) -> (0;
)| ( 1) -> (1;
)| ( n) -> ((fib((n - 2)) + fib((n - 1)));
)) in
printf("fib 10 = %d\n") (fib(10));
let (llor:((int * int))->(int)) = (function | ( (0 , 0)) -> (let a = 1 in
let b = 2 in
(a lor b);
)| ( (a , b)) -> ((a lor b);
)) in
printf("llor %d\n") (llor((1 , 2)));
;;
let _ = iter((fun ( x) -> printf("%d\n")(x);
))([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
))([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
))([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
)) ([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
))([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
))([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
))([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
))([1; 2; 3; 4]);
iter((fun ( x) -> printf("%d\n") (x);
))(map((fun ( x) -> (x * 10);
))([1; 2; 3; 4]));
;;
type a = {x:int;y:int};;
let _ = let aa = {x=1;y=2} in
printf("%d\n") ((aa . x));
printf("%d\n") (({x=1;y=2} . x));
let aa = (fun ({x}) -> printf("%d\n") (x);
) in ()
;;
type e = EUnit|EInt of (int)|EAdd of (e * e);;
let _ = let rec (eval:((e))->(int)) = (function | ( EUnit) -> (0;
)| ( EInt(i)) -> (i;
)| ( EAdd((a , b))) -> ((eval(a) + eval(b));
)) in
printf("1+2=%d\n") (eval(EAdd((EInt(1) , EInt(2)))));
;;

