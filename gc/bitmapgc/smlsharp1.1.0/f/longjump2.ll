; ModuleID = 'longjump2.c'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

%struct.exn_stack = type { [37 x i32], %struct.exn_stack* }

@exn = common global %struct.exn_stack* null, align 8
@.str = private unnamed_addr constant [7 x i8] c"test1\0A\00", align 1
@.str1 = private unnamed_addr constant [10 x i8] c"catch %d\0A\00", align 1
@.str2 = private unnamed_addr constant [12 x i8] c"before: %d\0A\00", align 1
@.str3 = private unnamed_addr constant [4 x i8] c"aaa\00", align 1
@.str4 = private unnamed_addr constant [15 x i8] c"after : %d %d\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define void @catchf() #0 {
  %st = alloca %struct.exn_stack*, align 8
  %1 = load %struct.exn_stack** @exn, align 8
  %2 = icmp eq %struct.exn_stack* %1, null
  br i1 %2, label %3, label %4

; <label>:3                                       ; preds = %0
  br label %11

; <label>:4                                       ; preds = %0
  %5 = load %struct.exn_stack** @exn, align 8
  store %struct.exn_stack* %5, %struct.exn_stack** %st, align 8
  %6 = load %struct.exn_stack** @exn, align 8
  %7 = getelementptr inbounds %struct.exn_stack* %6, i32 0, i32 1
  %8 = load %struct.exn_stack** %7, align 8
  store %struct.exn_stack* %8, %struct.exn_stack** @exn, align 8
  %9 = load %struct.exn_stack** %st, align 8
  %10 = bitcast %struct.exn_stack* %9 to i8*
  call void @free(i8* %10)
  br label %11

; <label>:11                                      ; preds = %4, %3
  ret void
}

declare void @free(i8*) #1

; Function Attrs: nounwind ssp uwtable
define void @throw(i32 %r) #0 {
  %1 = alloca i32, align 4
  store i32 %r, i32* %1, align 4
  %2 = load %struct.exn_stack** @exn, align 8
  %3 = getelementptr inbounds %struct.exn_stack* %2, i32 0, i32 0
  %4 = getelementptr inbounds [37 x i32]* %3, i32 0, i32 0
  %5 = load i32* %1, align 4
  call void @longjmp(i32* %4, i32 %5) #4
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn

declare i32 @setjmp(i32*) #3
declare void @longjmp(i32*, i32) #2

; Function Attrs: nounwind ssp uwtable
define i32 @test1() #0 {
  %e = alloca i32, align 4
  %st = alloca %struct.exn_stack*, align 8
  %1 = call i8* @malloc(i64 160)
  %2 = bitcast i8* %1 to %struct.exn_stack*
  store %struct.exn_stack* %2, %struct.exn_stack** %st, align 8
  %3 = load %struct.exn_stack** @exn, align 8
  %4 = load %struct.exn_stack** %st, align 8
  %5 = getelementptr inbounds %struct.exn_stack* %4, i32 0, i32 1
  store %struct.exn_stack* %3, %struct.exn_stack** %5, align 8
  %6 = load %struct.exn_stack** %st, align 8
  store %struct.exn_stack* %6, %struct.exn_stack** @exn, align 8
  %7 = load %struct.exn_stack** %st, align 8
  %8 = getelementptr inbounds %struct.exn_stack* %7, i32 0, i32 0
  %9 = getelementptr inbounds [37 x i32]* %8, i32 0, i32 0
  %10 = call i32 @setjmp(i32* %9) #5
  store i32 %10, i32* %e, align 4
  %11 = load i32* %e, align 4
  %12 = icmp eq i32 %11, 0
  br i1 %12, label %13, label %15

; <label>:13                                      ; preds = %0
  %14 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str, i32 0, i32 0))
  call void @throw(i32 112)
  call void @catchf()
  br label %18

; <label>:15                                      ; preds = %0
  call void @catchf()
  %16 = load i32* %e, align 4
  %17 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([10 x i8]* @.str1, i32 0, i32 0), i32 %16)
  br label %18

; <label>:18                                      ; preds = %15, %13
  call void @throw(i32 222)
  ret i32 0
}

declare i8* @malloc(i64) #1

; Function Attrs: returns_twice
declare i32 @setjmp(i32*) #3

declare i32 @printf(i8*, ...) #1

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %e = alloca i32, align 4
  %st = alloca %struct.exn_stack*, align 8
  store i32 0, i32* %1
  store i32 777, i32* %x, align 4
  %2 = call i8* @malloc(i64 160)
  %3 = bitcast i8* %2 to %struct.exn_stack*
  store %struct.exn_stack* %3, %struct.exn_stack** %st, align 8
  %4 = load %struct.exn_stack** @exn, align 8
  %5 = load %struct.exn_stack** %st, align 8
  %6 = getelementptr inbounds %struct.exn_stack* %5, i32 0, i32 1
  store %struct.exn_stack* %4, %struct.exn_stack** %6, align 8
  %7 = load %struct.exn_stack** %st, align 8
  store %struct.exn_stack* %7, %struct.exn_stack** @exn, align 8
  %8 = load %struct.exn_stack** %st, align 8
  %9 = getelementptr inbounds %struct.exn_stack* %8, i32 0, i32 0
  %10 = getelementptr inbounds [37 x i32]* %9, i32 0, i32 0
  %11 = call i32 @setjmp(i32* %10) #5
  store i32 %11, i32* %e, align 4
  %12 = load i32* %e, align 4
  %13 = icmp eq i32 %12, 0
  br i1 %13, label %14, label %19

; <label>:14                                      ; preds = %0
  %15 = load i32* %x, align 4
  %16 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str2, i32 0, i32 0), i32 %15)
  store i32 666, i32* %x, align 4
  %17 = call i32 @test1()
  %18 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str3, i32 0, i32 0))
  call void @catchf()
  br label %23

; <label>:19                                      ; preds = %0
  call void @catchf()
  %20 = load i32* %x, align 4
  %21 = load i32* %e, align 4
  %22 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str4, i32 0, i32 0), i32 %20, i32 %21)
  br label %23

; <label>:23                                      ; preds = %19, %14
  ret i32 0
}

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { returns_twice "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { noreturn }
attributes #5 = { returns_twice }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Apple LLVM version 6.0 (clang-600.0.57) (based on LLVM 3.5svn)"}
