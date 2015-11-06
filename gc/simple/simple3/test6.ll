
%struct.ObjectHeader = type { %struct.ObjectHeader*, i32, i8, i8 }
%struct.FrameMap = type { i16, i8*, i8*, %struct.Frame*, %struct.FrameMap* }
%struct.Frame = type { i8*, i16, i32* }
%union.Object = type { %struct.anon }
%struct.anon = type { %union.Object*, %union.Object* }

@heap_list = common global %struct.ObjectHeader* null, align 8
@.str = private unnamed_addr constant [9 x i8] c"mark %p\0A\00", align 1
@.str1 = private unnamed_addr constant [10 x i8] c"size=%ld\0A\00", align 1
@.str2 = private unnamed_addr constant [6 x i8] c"PAIR\0A\00", align 1
@.str3 = private unnamed_addr constant [17 x i8] c"RECORD size=%ld\0A\00", align 1
@.str4 = private unnamed_addr constant [9 x i8] c"skip %d\0A\00", align 1
@.str5 = private unnamed_addr constant [21 x i8] c"frame size error %d\0A\00", align 1
@gc_frame_map_list = common global %struct.FrameMap* null, align 8
@.str6 = private unnamed_addr constant [27 x i8] c"find frame bitmap_size=%d\0A\00", align 1
@gc_top_ptr = common global i8** null, align 8
@heap_num = common global i32 0, align 4
@heap_max = common global i32 0, align 4
@.str7 = private unnamed_addr constant [37 x i8] c"Collected %d objects, %d remaining.\0A\00", align 1
@.str8 = private unnamed_addr constant [13 x i8] c"gc_alloc %p\0A\00", align 1
@.str9 = private unnamed_addr constant [12 x i8] c"int ptr %p\0A\00", align 1
@test.start_ptr = internal global i8* blockaddress(@test, %12), align 8
@.str10 = private unnamed_addr constant [13 x i8] c"frame[1]=%p\0A\00", align 1
@test.bitmap = internal global [2 x i32] [i32 1, i32 0], align 4
@test.frames = internal global [4 x %struct.Frame] [%struct.Frame { i8* blockaddress(@test, %7), i16 1, i32* getelementptr inbounds ([2 x i32]* @test.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test, %8), i16 1, i32* getelementptr inbounds ([2 x i32]* @test.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test, %9), i16 0, i32* getelementptr inbounds ([2 x i32]* @test.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test, %12), i16 0, i32* null }], align 16
@test.f = internal global %struct.FrameMap { i16 2, i8* bitcast (void ()* @test to i8*), i8* blockaddress(@test, %12), %struct.Frame* getelementptr inbounds ([4 x %struct.Frame]* @test.frames, i32 0, i32 0), %struct.FrameMap* null }, align 8
@test2.start_ptr = internal global i8* blockaddress(@test2, %11), align 8
@test2.bitmap = internal global [1 x i32] [i32 1], align 4
@test2.frames = internal global [3 x %struct.Frame] [%struct.Frame { i8* blockaddress(@test2, %7), i16 1, i32* getelementptr inbounds ([1 x i32]* @test2.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test2, %8), i16 1, i32* getelementptr inbounds ([1 x i32]* @test2.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test2, %11), i16 0, i32* null }], align 16
@test2.f = internal global %struct.FrameMap { i16 2, i8* bitcast (void ()* @test2 to i8*), i8* blockaddress(@test2, %11), %struct.Frame* getelementptr inbounds ([3 x %struct.Frame]* @test2.frames, i32 0, i32 0), %struct.FrameMap* null }, align 8
@test3.start_ptr = internal global i8* blockaddress(@test3, %123), align 8
@.str11 = private unnamed_addr constant [15 x i8] c"test frame=%p\0A\00", align 1
@.str12 = private unnamed_addr constant [15 x i8] c"data1 = %p %d\0A\00", align 1
@.str13 = private unnamed_addr constant [15 x i8] c"data2 = %p %d\0A\00", align 1
@.str14 = private unnamed_addr constant [15 x i8] c"data3 = %p %d\0A\00", align 1
@.str15 = private unnamed_addr constant [15 x i8] c"data4 = %p %d\0A\00", align 1
@.str16 = private unnamed_addr constant [15 x i8] c"data5 = %p %d\0A\00", align 1
@.str17 = private unnamed_addr constant [15 x i8] c"data6 = %p %d\0A\00", align 1
@test3.bitmap = internal global [3 x i32] [i32 1, i32 3, i32 6], align 4
@test3.frames = internal global [4 x %struct.Frame] [%struct.Frame { i8* blockaddress(@test3, %20), i16 1, i32* getelementptr inbounds ([3 x i32]* @test3.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test3, %36), i16 1, i32* bitcast (i8* getelementptr (i8* bitcast ([3 x i32]* @test3.bitmap to i8*), i64 4) to i32*) }, %struct.Frame { i8* blockaddress(@test3, %120), i16 1, i32* bitcast (i8* getelementptr (i8* bitcast ([3 x i32]* @test3.bitmap to i8*), i64 8) to i32*) }, %struct.Frame { i8* blockaddress(@test3, %123), i16 0, i32* null }], align 16
@test3.f = internal global %struct.FrameMap { i16 4, i8* bitcast (void ()* @test3 to i8*), i8* blockaddress(@test3, %123), %struct.Frame* getelementptr inbounds ([4 x %struct.Frame]* @test3.frames, i32 0, i32 0), %struct.FrameMap* null }, align 8
@test_int.start_ptr = internal global i8* blockaddress(@test_int, %14), align 8
@test_int.bitmap = internal global [2 x i32] [i32 1, i32 1], align 4
@test_int.frames = internal global [2 x %struct.Frame] [%struct.Frame { i8* blockaddress(@test_int, %10), i16 1, i32* getelementptr inbounds ([2 x i32]* @test_int.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test_int, %14), i16 1, i32* bitcast (i8* getelementptr (i8* bitcast ([2 x i32]* @test_int.bitmap to i8*), i64 8) to i32*) }], align 16
@test_int.f = internal global %struct.FrameMap { i16 2, i8* bitcast (%union.Object* (i32)* @test_int to i8*), i8* blockaddress(@test_int, %14), %struct.Frame* getelementptr inbounds ([2 x %struct.Frame]* @test_int.frames, i32 0, i32 0), %struct.FrameMap* null }, align 8
@test_record.start_ptr = internal global i8* blockaddress(@test_record, %28), align 8
@test_record.bitmap = internal global [2 x i32] [i32 1, i32 1], align 4
@test_record.frames = internal global [2 x %struct.Frame] [%struct.Frame { i8* blockaddress(@test_record, %25), i16 1, i32* getelementptr inbounds ([2 x i32]* @test_record.bitmap, i32 0, i32 0) }, %struct.Frame { i8* blockaddress(@test_record, %28), i16 1, i32* bitcast (i8* getelementptr (i8* bitcast ([2 x i32]* @test_record.bitmap to i8*), i64 8) to i32*) }], align 16
@test_record.f = internal global %struct.FrameMap { i16 2, i8* bitcast (void ()* @test_record to i8*), i8* blockaddress(@test_record, %28), %struct.Frame* getelementptr inbounds ([2 x %struct.Frame]* @test_record.frames, i32 0, i32 0), %struct.FrameMap* null }, align 8
@.str18 = private unnamed_addr constant [5 x i8] c"---\0A\00", align 1
@.str19 = private unnamed_addr constant [28 x i8] c"sizeof type %ld header %ld\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define i32 @heap_find(%struct.ObjectHeader* %o) #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.ObjectHeader*, align 8
  %object = alloca %struct.ObjectHeader*, align 8
  store %struct.ObjectHeader* %o, %struct.ObjectHeader** %2, align 8
  %3 = load %struct.ObjectHeader** @heap_list, align 8
  store %struct.ObjectHeader* %3, %struct.ObjectHeader** %object, align 8
  br label %4

; <label>:4                                       ; preds = %12, %0
  %5 = load %struct.ObjectHeader** %object, align 8
  %6 = icmp ne %struct.ObjectHeader* %5, null
  br i1 %6, label %7, label %16

; <label>:7                                       ; preds = %4
  %8 = load %struct.ObjectHeader** %object, align 8
  %9 = load %struct.ObjectHeader** %2, align 8
  %10 = icmp eq %struct.ObjectHeader* %8, %9
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  store i32 1, i32* %1
  br label %17

; <label>:12                                      ; preds = %7
  %13 = load %struct.ObjectHeader** %object, align 8
  %14 = getelementptr inbounds %struct.ObjectHeader* %13, i32 0, i32 0
  %15 = load %struct.ObjectHeader** %14, align 8
  store %struct.ObjectHeader* %15, %struct.ObjectHeader** %object, align 8
  br label %4

; <label>:16                                      ; preds = %4
  store i32 0, i32* %1
  br label %17

; <label>:17                                      ; preds = %16, %11
  %18 = load i32* %1
  ret i32 %18
}

; Function Attrs: nounwind ssp uwtable
define void @gc_mark_object(%union.Object* %object) #0 {
  %1 = alloca %union.Object*, align 8
  %head = alloca %struct.ObjectHeader*, align 8
  %size = alloca i64, align 8
  %bitmap = alloca i64*, align 8
  %i = alloca i32, align 4
  %i1 = alloca i32, align 4
  store %union.Object* %object, %union.Object** %1, align 8
  %2 = load %union.Object** %1, align 8
  %3 = bitcast %union.Object* %2 to %struct.ObjectHeader*
  %4 = getelementptr inbounds %struct.ObjectHeader* %3, i64 -1
  store %struct.ObjectHeader* %4, %struct.ObjectHeader** %head, align 8
  %5 = load %struct.ObjectHeader** %head, align 8
  %6 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @.str, i32 0, i32 0), %struct.ObjectHeader* %5)
  %7 = load %struct.ObjectHeader** %head, align 8
  %8 = call i32 @heap_find(%struct.ObjectHeader* %7)
  %9 = icmp ne i32 %8, 0
  br i1 %9, label %11, label %10

; <label>:10                                      ; preds = %0
  br label %108

; <label>:11                                      ; preds = %0
  %12 = load %struct.ObjectHeader** %head, align 8
  %13 = getelementptr inbounds %struct.ObjectHeader* %12, i32 0, i32 3
  %14 = load i8* %13, align 1
  %15 = icmp ne i8 %14, 0
  br i1 %15, label %16, label %17

; <label>:16                                      ; preds = %11
  br label %108

; <label>:17                                      ; preds = %11
  %18 = load %struct.ObjectHeader** %head, align 8
  %19 = getelementptr inbounds %struct.ObjectHeader* %18, i32 0, i32 3
  store i8 1, i8* %19, align 1
  %20 = load %struct.ObjectHeader** %head, align 8
  %21 = getelementptr inbounds %struct.ObjectHeader* %20, i32 0, i32 2
  %22 = load i8* %21, align 1
  %23 = zext i8 %22 to i32
  switch i32 %23, label %108 [
    i32 0, label %24
    i32 2, label %48
    i32 1, label %58
    i32 3, label %59
  ]

; <label>:24                                      ; preds = %17
  %25 = load %struct.ObjectHeader** %head, align 8
  %26 = getelementptr inbounds %struct.ObjectHeader* %25, i32 0, i32 1
  %27 = load i32* %26, align 4
  %28 = sext i32 %27 to i64
  %29 = udiv i64 %28, 8
  store i64 %29, i64* %size, align 8
  %30 = load i64* %size, align 8
  %31 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([10 x i8]* @.str1, i32 0, i32 0), i64 %30)
  store i32 0, i32* %i, align 4
  br label %32

; <label>:32                                      ; preds = %44, %24
  %33 = load i32* %i, align 4
  %34 = sext i32 %33 to i64
  %35 = load i64* %size, align 8
  %36 = icmp slt i64 %34, %35
  br i1 %36, label %37, label %47

; <label>:37                                      ; preds = %32
  %38 = load i32* %i, align 4
  %39 = sext i32 %38 to i64
  %40 = load %union.Object** %1, align 8
  %41 = bitcast %union.Object* %40 to [0 x %union.Object*]*
  %42 = getelementptr inbounds [0 x %union.Object*]* %41, i32 0, i64 %39
  %43 = load %union.Object** %42, align 8
  call void @gc_mark_object(%union.Object* %43)
  br label %44

; <label>:44                                      ; preds = %37
  %45 = load i32* %i, align 4
  %46 = add nsw i32 %45, 1
  store i32 %46, i32* %i, align 4
  br label %32

; <label>:47                                      ; preds = %32
  br label %108

; <label>:48                                      ; preds = %17
  %49 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.str2, i32 0, i32 0))
  %50 = load %union.Object** %1, align 8
  %51 = bitcast %union.Object* %50 to %struct.anon*
  %52 = getelementptr inbounds %struct.anon* %51, i32 0, i32 0
  %53 = load %union.Object** %52, align 8
  call void @gc_mark_object(%union.Object* %53)
  %54 = load %union.Object** %1, align 8
  %55 = bitcast %union.Object* %54 to %struct.anon*
  %56 = getelementptr inbounds %struct.anon* %55, i32 0, i32 1
  %57 = load %union.Object** %56, align 8
  call void @gc_mark_object(%union.Object* %57)
  br label %108

; <label>:58                                      ; preds = %17
  br label %108

; <label>:59                                      ; preds = %17
  %60 = load %struct.ObjectHeader** %head, align 8
  %61 = getelementptr inbounds %struct.ObjectHeader* %60, i32 0, i32 1
  %62 = load i32* %61, align 4
  %63 = sext i32 %62 to i64
  %64 = udiv i64 %63, 8
  store i64 %64, i64* %size, align 8
  %65 = load i64* %size, align 8
  %66 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([17 x i8]* @.str3, i32 0, i32 0), i64 %65)
  %67 = load i64* %size, align 8
  %68 = load %union.Object** %1, align 8
  %69 = bitcast %union.Object* %68 to [0 x i64]*
  %70 = getelementptr inbounds [0 x i64]* %69, i32 0, i64 %67
  store i64* %70, i64** %bitmap, align 8
  %71 = load i64* %size, align 8
  %72 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([10 x i8]* @.str1, i32 0, i32 0), i64 %71)
  store i32 0, i32* %i1, align 4
  br label %73

; <label>:73                                      ; preds = %104, %59
  %74 = load i32* %i1, align 4
  %75 = sext i32 %74 to i64
  %76 = load i64* %size, align 8
  %77 = icmp slt i64 %75, %76
  br i1 %77, label %78, label %107

; <label>:78                                      ; preds = %73
  %79 = load i32* %i1, align 4
  %80 = sext i32 %79 to i64
  %81 = udiv i64 %80, 8
  %82 = load i64** %bitmap, align 8
  %83 = getelementptr inbounds i64* %82, i64 %81
  %84 = load i64* %83, align 8
  %85 = load i32* %i1, align 4
  %86 = sext i32 %85 to i64
  %87 = urem i64 %86, 8
  %88 = trunc i64 %87 to i32
  %89 = shl i32 1, %88
  %90 = sext i32 %89 to i64
  %91 = and i64 %84, %90
  %92 = icmp ne i64 %91, 0
  br i1 %92, label %93, label %100

; <label>:93                                      ; preds = %78
  %94 = load i32* %i1, align 4
  %95 = sext i32 %94 to i64
  %96 = load %union.Object** %1, align 8
  %97 = bitcast %union.Object* %96 to [0 x %union.Object*]*
  %98 = getelementptr inbounds [0 x %union.Object*]* %97, i32 0, i64 %95
  %99 = load %union.Object** %98, align 8
  call void @gc_mark_object(%union.Object* %99)
  br label %103

; <label>:100                                     ; preds = %78
  %101 = load i32* %i1, align 4
  %102 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @.str4, i32 0, i32 0), i32 %101)
  br label %103

; <label>:103                                     ; preds = %100, %93
  br label %104

; <label>:104                                     ; preds = %103
  %105 = load i32* %i1, align 4
  %106 = add nsw i32 %105, 1
  store i32 %106, i32* %i1, align 4
  br label %73

; <label>:107                                     ; preds = %73
  br label %108

; <label>:108                                     ; preds = %10, %16, %17, %107, %58, %48, %47
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: nounwind ssp uwtable
define void @gc_add_frame_map(%struct.FrameMap* %frame_map) #0 {
  %1 = alloca %struct.FrameMap*, align 8
  store %struct.FrameMap* %frame_map, %struct.FrameMap** %1, align 8
  %2 = load %struct.FrameMap** %1, align 8
  %3 = getelementptr inbounds %struct.FrameMap* %2, i32 0, i32 0
  %4 = load i16* %3, align 2
  %5 = zext i16 %4 to i32
  %6 = srem i32 %5, 2
  %7 = icmp ne i32 %6, 0
  br i1 %7, label %8, label %14

; <label>:8                                       ; preds = %0
  %9 = load %struct.FrameMap** %1, align 8
  %10 = getelementptr inbounds %struct.FrameMap* %9, i32 0, i32 0
  %11 = load i16* %10, align 2
  %12 = zext i16 %11 to i32
  %13 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([21 x i8]* @.str5, i32 0, i32 0), i32 %12)
  call void @exit(i32 0) #3
  unreachable

; <label>:14                                      ; preds = %0
  %15 = load %struct.FrameMap** @gc_frame_map_list, align 8
  %16 = load %struct.FrameMap** %1, align 8
  %17 = getelementptr inbounds %struct.FrameMap* %16, i32 0, i32 4
  store %struct.FrameMap* %15, %struct.FrameMap** %17, align 8
  %18 = load %struct.FrameMap** %1, align 8
  store %struct.FrameMap* %18, %struct.FrameMap** @gc_frame_map_list, align 8
  ret void
}

; Function Attrs: noreturn
declare void @exit(i32) #2

; Function Attrs: nounwind ssp uwtable
define i8** @get_stack_top() #0 {
  %data = alloca i8*, align 8
  %ptr = alloca i8**, align 8
  store i8** %data, i8*** %ptr, align 8
  %1 = load i8*** %ptr, align 8
  %2 = getelementptr inbounds i8** %1, i64 1
  store i8** %2, i8*** %ptr, align 8
  %3 = load i8*** %ptr, align 8
  %4 = getelementptr inbounds i8** %3, i64 0
  %5 = load i8** %4, align 8
  %6 = bitcast i8* %5 to i8**
  ret i8** %6
}

; Function Attrs: nounwind ssp uwtable
define %struct.FrameMap* @gc_mark_find_frame_map(i8* %addr) #0 {
  %1 = alloca %struct.FrameMap*, align 8
  %2 = alloca i8*, align 8
  %frame_map = alloca %struct.FrameMap*, align 8
  store i8* %addr, i8** %2, align 8
  %3 = load %struct.FrameMap** @gc_frame_map_list, align 8
  store %struct.FrameMap* %3, %struct.FrameMap** %frame_map, align 8
  br label %4

; <label>:4                                       ; preds = %21, %0
  %5 = load %struct.FrameMap** %frame_map, align 8
  %6 = icmp ne %struct.FrameMap* %5, null
  br i1 %6, label %7, label %25

; <label>:7                                       ; preds = %4
  %8 = load %struct.FrameMap** %frame_map, align 8
  %9 = getelementptr inbounds %struct.FrameMap* %8, i32 0, i32 1
  %10 = load i8** %9, align 8
  %11 = load i8** %2, align 8
  %12 = icmp ule i8* %10, %11
  br i1 %12, label %13, label %21

; <label>:13                                      ; preds = %7
  %14 = load i8** %2, align 8
  %15 = load %struct.FrameMap** %frame_map, align 8
  %16 = getelementptr inbounds %struct.FrameMap* %15, i32 0, i32 2
  %17 = load i8** %16, align 8
  %18 = icmp ule i8* %14, %17
  br i1 %18, label %19, label %21

; <label>:19                                      ; preds = %13
  %20 = load %struct.FrameMap** %frame_map, align 8
  store %struct.FrameMap* %20, %struct.FrameMap** %1
  br label %26

; <label>:21                                      ; preds = %13, %7
  %22 = load %struct.FrameMap** %frame_map, align 8
  %23 = getelementptr inbounds %struct.FrameMap* %22, i32 0, i32 4
  %24 = load %struct.FrameMap** %23, align 8
  store %struct.FrameMap* %24, %struct.FrameMap** %frame_map, align 8
  br label %4

; <label>:25                                      ; preds = %4
  store %struct.FrameMap* null, %struct.FrameMap** %1
  br label %26

; <label>:26                                      ; preds = %25, %19
  %27 = load %struct.FrameMap** %1
  ret %struct.FrameMap* %27
}

; Function Attrs: nounwind ssp uwtable
define %struct.Frame* @gc_mark_find_frame(%struct.FrameMap* %f, i8* %addr) #0 {
  %1 = alloca %struct.Frame*, align 8
  %2 = alloca %struct.FrameMap*, align 8
  %3 = alloca i8*, align 8
  %frames = alloca %struct.Frame*, align 8
  store %struct.FrameMap* %f, %struct.FrameMap** %2, align 8
  store i8* %addr, i8** %3, align 8
  %4 = load %struct.FrameMap** %2, align 8
  %5 = getelementptr inbounds %struct.FrameMap* %4, i32 0, i32 3
  %6 = load %struct.Frame** %5, align 8
  store %struct.Frame* %6, %struct.Frame** %frames, align 8
  br label %7

; <label>:7                                       ; preds = %19, %0
  %8 = load %struct.Frame** %frames, align 8
  %9 = icmp ne %struct.Frame* %8, null
  br i1 %9, label %10, label %22

; <label>:10                                      ; preds = %7
  %11 = load %struct.Frame** %frames, align 8
  %12 = getelementptr inbounds %struct.Frame* %11, i64 1
  %13 = getelementptr inbounds %struct.Frame* %12, i32 0, i32 0
  %14 = load i8** %13, align 8
  %15 = load i8** %3, align 8
  %16 = icmp ugt i8* %14, %15
  br i1 %16, label %17, label %19

; <label>:17                                      ; preds = %10
  %18 = load %struct.Frame** %frames, align 8
  store %struct.Frame* %18, %struct.Frame** %1
  br label %23

; <label>:19                                      ; preds = %10
  %20 = load %struct.Frame** %frames, align 8
  %21 = getelementptr inbounds %struct.Frame* %20, i32 1
  store %struct.Frame* %21, %struct.Frame** %frames, align 8
  br label %7

; <label>:22                                      ; preds = %7
  store %struct.Frame* null, %struct.Frame** %1
  br label %23

; <label>:23                                      ; preds = %22, %17
  %24 = load %struct.Frame** %1
  ret %struct.Frame* %24
}

; Function Attrs: nounwind ssp uwtable
define void @gc_mark_frame(%struct.Frame* %frame, %union.Object** %objects) #0 {
  %1 = alloca %struct.Frame*, align 8
  %2 = alloca %union.Object**, align 8
  %i = alloca i32, align 4
  %bitmap = alloca i32, align 4
  %n = alloca i32, align 4
  store %struct.Frame* %frame, %struct.Frame** %1, align 8
  store %union.Object** %objects, %union.Object*** %2, align 8
  store i32 0, i32* %i, align 4
  br label %3

; <label>:3                                       ; preds = %39, %0
  %4 = load i32* %i, align 4
  %5 = load %struct.Frame** %1, align 8
  %6 = getelementptr inbounds %struct.Frame* %5, i32 0, i32 1
  %7 = load i16* %6, align 2
  %8 = zext i16 %7 to i32
  %9 = icmp slt i32 %4, %8
  br i1 %9, label %10, label %42

; <label>:10                                      ; preds = %3
  %11 = load i32* %i, align 4
  %12 = sext i32 %11 to i64
  %13 = load %struct.Frame** %1, align 8
  %14 = getelementptr inbounds %struct.Frame* %13, i32 0, i32 2
  %15 = load i32** %14, align 8
  %16 = getelementptr inbounds i32* %15, i64 %12
  %17 = load i32* %16, align 4
  store i32 %17, i32* %bitmap, align 4
  %18 = load i32* %i, align 4
  %19 = mul nsw i32 %18, 32
  store i32 %19, i32* %n, align 4
  br label %20

; <label>:20                                      ; preds = %33, %10
  %21 = load i32* %bitmap, align 4
  %22 = icmp ne i32 %21, 0
  br i1 %22, label %23, label %38

; <label>:23                                      ; preds = %20
  %24 = load i32* %bitmap, align 4
  %25 = and i32 %24, 1
  %26 = icmp ne i32 %25, 0
  br i1 %26, label %27, label %33

; <label>:27                                      ; preds = %23
  %28 = load i32* %n, align 4
  %29 = sext i32 %28 to i64
  %30 = load %union.Object*** %2, align 8
  %31 = getelementptr inbounds %union.Object** %30, i64 %29
  %32 = load %union.Object** %31, align 8
  call void @gc_mark_object(%union.Object* %32)
  br label %33

; <label>:33                                      ; preds = %27, %23
  %34 = load i32* %bitmap, align 4
  %35 = lshr i32 %34, 1
  store i32 %35, i32* %bitmap, align 4
  %36 = load i32* %n, align 4
  %37 = add nsw i32 %36, 1
  store i32 %37, i32* %n, align 4
  br label %20

; <label>:38                                      ; preds = %20
  br label %39

; <label>:39                                      ; preds = %38
  %40 = load i32* %i, align 4
  %41 = add nsw i32 %40, 1
  store i32 %41, i32* %i, align 4
  br label %3

; <label>:42                                      ; preds = %3
  ret void
}

; Function Attrs: nounwind ssp uwtable
define void @gc_mark(i8** %ptr, i8* %addr) #0 {
  %1 = alloca i8**, align 8
  %2 = alloca i8*, align 8
  %frame_map = alloca %struct.FrameMap*, align 8
  %frame = alloca %struct.Frame*, align 8
  %objects = alloca %union.Object**, align 8
  store i8** %ptr, i8*** %1, align 8
  store i8* %addr, i8** %2, align 8
  br label %3

; <label>:3                                       ; preds = %40, %0
  %4 = load i8*** %1, align 8
  %5 = getelementptr inbounds i8** %4, i64 1
  %6 = load i8** %5, align 8
  store i8* %6, i8** %2, align 8
  %7 = load i8*** %1, align 8
  %8 = getelementptr inbounds i8** %7, i64 0
  %9 = load i8** %8, align 8
  %10 = bitcast i8* %9 to i8**
  store i8** %10, i8*** %1, align 8
  %11 = load i8** %2, align 8
  %12 = call %struct.FrameMap* @gc_mark_find_frame_map(i8* %11)
  store %struct.FrameMap* %12, %struct.FrameMap** %frame_map, align 8
  %13 = load %struct.FrameMap** %frame_map, align 8
  %14 = icmp ne %struct.FrameMap* %13, null
  br i1 %14, label %16, label %15

; <label>:15                                      ; preds = %3
  br label %40

; <label>:16                                      ; preds = %3
  %17 = load %struct.FrameMap** %frame_map, align 8
  %18 = load i8** %2, align 8
  %19 = call %struct.Frame* @gc_mark_find_frame(%struct.FrameMap* %17, i8* %18)
  store %struct.Frame* %19, %struct.Frame** %frame, align 8
  %20 = load %struct.Frame** %frame, align 8
  %21 = icmp ne %struct.Frame* %20, null
  br i1 %21, label %23, label %22

; <label>:22                                      ; preds = %16
  br label %40

; <label>:23                                      ; preds = %16
  %24 = load %struct.Frame** %frame, align 8
  %25 = getelementptr inbounds %struct.Frame* %24, i32 0, i32 1
  %26 = load i16* %25, align 2
  %27 = zext i16 %26 to i32
  %28 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([27 x i8]* @.str6, i32 0, i32 0), i32 %27)
  %29 = load %struct.FrameMap** %frame_map, align 8
  %30 = getelementptr inbounds %struct.FrameMap* %29, i32 0, i32 0
  %31 = load i16* %30, align 2
  %32 = zext i16 %31 to i32
  %33 = sub nsw i32 -2, %32
  %34 = sext i32 %33 to i64
  %35 = load i8*** %1, align 8
  %36 = getelementptr inbounds i8** %35, i64 %34
  %37 = bitcast i8** %36 to %union.Object**
  store %union.Object** %37, %union.Object*** %objects, align 8
  %38 = load %struct.Frame** %frame, align 8
  %39 = load %union.Object*** %objects, align 8
  call void @gc_mark_frame(%struct.Frame* %38, %union.Object** %39)
  br label %40

; <label>:40                                      ; preds = %23, %22, %15
  %41 = load i8*** %1, align 8
  %42 = load i8*** @gc_top_ptr, align 8
  %43 = icmp ult i8** %41, %42
  br i1 %43, label %3, label %44

; <label>:44                                      ; preds = %40
  ret void
}

; Function Attrs: nounwind ssp uwtable
define void @gc_sweep() #0 {
  %object = alloca %struct.ObjectHeader**, align 8
  %unreached = alloca %struct.ObjectHeader*, align 8
  store %struct.ObjectHeader** @heap_list, %struct.ObjectHeader*** %object, align 8
  br label %1

; <label>:1                                       ; preds = %29, %0
  %2 = load %struct.ObjectHeader*** %object, align 8
  %3 = load %struct.ObjectHeader** %2, align 8
  %4 = icmp ne %struct.ObjectHeader* %3, null
  br i1 %4, label %5, label %30

; <label>:5                                       ; preds = %1
  %6 = load %struct.ObjectHeader*** %object, align 8
  %7 = load %struct.ObjectHeader** %6, align 8
  %8 = getelementptr inbounds %struct.ObjectHeader* %7, i32 0, i32 3
  %9 = load i8* %8, align 1
  %10 = icmp ne i8 %9, 0
  br i1 %10, label %22, label %11

; <label>:11                                      ; preds = %5
  %12 = load %struct.ObjectHeader*** %object, align 8
  %13 = load %struct.ObjectHeader** %12, align 8
  store %struct.ObjectHeader* %13, %struct.ObjectHeader** %unreached, align 8
  %14 = load %struct.ObjectHeader** %unreached, align 8
  %15 = getelementptr inbounds %struct.ObjectHeader* %14, i32 0, i32 0
  %16 = load %struct.ObjectHeader** %15, align 8
  %17 = load %struct.ObjectHeader*** %object, align 8
  store %struct.ObjectHeader* %16, %struct.ObjectHeader** %17, align 8
  %18 = load %struct.ObjectHeader** %unreached, align 8
  %19 = bitcast %struct.ObjectHeader* %18 to i8*
  call void @free(i8* %19)
  %20 = load i32* @heap_num, align 4
  %21 = add nsw i32 %20, -1
  store i32 %21, i32* @heap_num, align 4
  br label %29

; <label>:22                                      ; preds = %5
  %23 = load %struct.ObjectHeader*** %object, align 8
  %24 = load %struct.ObjectHeader** %23, align 8
  %25 = getelementptr inbounds %struct.ObjectHeader* %24, i32 0, i32 3
  store i8 0, i8* %25, align 1
  %26 = load %struct.ObjectHeader*** %object, align 8
  %27 = load %struct.ObjectHeader** %26, align 8
  %28 = getelementptr inbounds %struct.ObjectHeader* %27, i32 0, i32 0
  store %struct.ObjectHeader** %28, %struct.ObjectHeader*** %object, align 8
  br label %29

; <label>:29                                      ; preds = %22, %11
  br label %1

; <label>:30                                      ; preds = %1
  ret void
}

declare void @free(i8*) #1

; Function Attrs: nounwind ssp uwtable
define void @gc_collect() #0 {
  %data = alloca i8*, align 8
  %prev_num = alloca i32, align 4
  %1 = load i32* @heap_num, align 4
  store i32 %1, i32* %prev_num, align 4
  %2 = call i8** @get_stack_top()
  call void @gc_mark(i8** %2, i8* null)
  call void @gc_sweep()
  %3 = load i32* %prev_num, align 4
  %4 = mul nsw i32 %3, 2
  store i32 %4, i32* @heap_max, align 4
  %5 = load i32* %prev_num, align 4
  %6 = load i32* @heap_num, align 4
  %7 = sub nsw i32 %5, %6
  %8 = load i32* @heap_num, align 4
  %9 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([37 x i8]* @.str7, i32 0, i32 0), i32 %7, i32 %8)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define i8* @gc_alloc(i32 %type, i32 %size) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %head = alloca %struct.ObjectHeader*, align 8
  store i32 %type, i32* %1, align 4
  store i32 %size, i32* %2, align 4
  %3 = load i32* @heap_num, align 4
  %4 = load i32* @heap_max, align 4
  %5 = icmp eq i32 %3, %4
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @gc_collect()
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i32* %2, align 4
  %9 = sext i32 %8 to i64
  %10 = add i64 16, %9
  %11 = call i8* @malloc(i64 %10)
  %12 = bitcast i8* %11 to %struct.ObjectHeader*
  store %struct.ObjectHeader* %12, %struct.ObjectHeader** %head, align 8
  %13 = load %struct.ObjectHeader** %head, align 8
  %14 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str8, i32 0, i32 0), %struct.ObjectHeader* %13)
  %15 = load i32* %1, align 4
  %16 = trunc i32 %15 to i8
  %17 = load %struct.ObjectHeader** %head, align 8
  %18 = getelementptr inbounds %struct.ObjectHeader* %17, i32 0, i32 2
  store i8 %16, i8* %18, align 1
  %19 = load %struct.ObjectHeader** @heap_list, align 8
  %20 = load %struct.ObjectHeader** %head, align 8
  %21 = getelementptr inbounds %struct.ObjectHeader* %20, i32 0, i32 0
  store %struct.ObjectHeader* %19, %struct.ObjectHeader** %21, align 8
  %22 = load %struct.ObjectHeader** %head, align 8
  store %struct.ObjectHeader* %22, %struct.ObjectHeader** @heap_list, align 8
  %23 = load %struct.ObjectHeader** %head, align 8
  %24 = getelementptr inbounds %struct.ObjectHeader* %23, i32 0, i32 3
  store i8 0, i8* %24, align 1
  %25 = load i32* %2, align 4
  %26 = load %struct.ObjectHeader** %head, align 8
  %27 = getelementptr inbounds %struct.ObjectHeader* %26, i32 0, i32 1
  store i32 %25, i32* %27, align 4
  %28 = load i32* @heap_num, align 4
  %29 = add nsw i32 %28, 1
  store i32 %29, i32* @heap_num, align 4
  %30 = load %struct.ObjectHeader** %head, align 8
  %31 = getelementptr inbounds %struct.ObjectHeader* %30, i64 1
  %32 = bitcast %struct.ObjectHeader* %31 to i8*
  ret i8* %32
}

declare i8* @malloc(i64) #1

; Function Attrs: nounwind ssp uwtable
define i8* @gc_alloc_int(i32 %n) #0 {
  %1 = alloca i32, align 4
  %data = alloca i32*, align 8
  store i32 %n, i32* %1, align 4
  %2 = call i8* @gc_alloc(i32 1, i32 4)
  %3 = bitcast i8* %2 to i32*
  store i32* %3, i32** %data, align 8
  %4 = load i32** %data, align 8
  %5 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str9, i32 0, i32 0), i32* %4)
  %6 = load i32* %1, align 4
  %7 = load i32** %data, align 8
  store i32 %6, i32* %7, align 4
  %8 = load i32** %data, align 8
  %9 = bitcast i32* %8 to i8*
  ret i8* %9
}

; Function Attrs: nounwind ssp uwtable
define void @gc_init() #0 {
  %1 = call i8** @get_stack_top()
  store i8** %1, i8*** @gc_top_ptr, align 8
  store %struct.ObjectHeader* null, %struct.ObjectHeader** @heap_list, align 8
  store i32 0, i32* @heap_num, align 4
  store i32 8, i32* @heap_max, align 4
  ret void
}

; Function Attrs: nounwind ssp uwtable
define void @gc_free() #0 {
  call void @gc_collect()
  ret void
}

; Function Attrs: nounwind ssp uwtable
define void @test() #0 {
  %frame = alloca [2 x i8*], align 16
  %1 = load i8** @test.start_ptr, align 8
  br label %10

; <label>:2                                       ; preds = %12, %10
  %3 = getelementptr inbounds [2 x i8*]* %frame, i32 0, i32 0
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str10, i32 0, i32 0), i8** %3)
  %5 = call i8* @gc_alloc(i32 0, i32 16)
  %6 = getelementptr inbounds [2 x i8*]* %frame, i32 0, i64 0
  store i8* %5, i8** %6, align 8
  br label %7

; <label>:7                                       ; preds = %10, %2
  call void @gc_collect()
  br label %8

; <label>:8                                       ; preds = %10, %7
  call void @gc_collect()
  br label %9

; <label>:9                                       ; preds = %10, %8
  call void @gc_collect()
  ret void

; <label>:10                                      ; preds = %0
  %11 = phi i8* [ %1, %0 ]
  indirectbr i8* %11, [label %12, label %7, label %8, label %9, label %12, label %12, label %2]

; <label>:12                                      ; preds = %10, %10, %10
  call void @gc_add_frame_map(%struct.FrameMap* @test.f)
  store i8* blockaddress(@test, %2), i8** @test.start_ptr, align 8
  br label %2
}

; Function Attrs: nounwind ssp uwtable
define void @test2() #0 {
  %frame = alloca [2 x i8*], align 16
  %1 = load i8** @test2.start_ptr, align 8
  br label %9

; <label>:2                                       ; preds = %11, %9
  %3 = call i8* @gc_alloc(i32 0, i32 16)
  %4 = getelementptr inbounds [2 x i8*]* %frame, i32 0, i64 0
  store i8* %3, i8** %4, align 8
  %5 = call i8* @gc_alloc(i32 0, i32 16)
  %6 = getelementptr inbounds [2 x i8*]* %frame, i32 0, i64 1
  store i8* %5, i8** %6, align 8
  br label %7

; <label>:7                                       ; preds = %9, %2
  call void @gc_collect()
  br label %8

; <label>:8                                       ; preds = %9, %7
  ret void

; <label>:9                                       ; preds = %0
  %10 = phi i8* [ %1, %0 ]
  indirectbr i8* %10, [label %11, label %7, label %8, label %11, label %11, label %2]

; <label>:11                                      ; preds = %9, %9, %9
  call void @gc_add_frame_map(%struct.FrameMap* @test2.f)
  store i8* blockaddress(@test2, %2), i8** @test2.start_ptr, align 8
  br label %2
}

; Function Attrs: nounwind ssp uwtable
define void @test3() #0 {
  %frame = alloca [4 x %union.Object*], align 16
  %1 = load i8** @test3.start_ptr, align 8
  br label %121

; <label>:2                                       ; preds = %123, %121
  %3 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i32 0
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str11, i32 0, i32 0), %union.Object** %3)
  %5 = call i8* @gc_alloc(i32 2, i32 16)
  %6 = bitcast i8* %5 to %union.Object*
  %7 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 0
  store %union.Object* %6, %union.Object** %7, align 8
  %8 = call i8* @gc_alloc_int(i32 10)
  %9 = bitcast i8* %8 to %union.Object*
  %10 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 0
  %11 = load %union.Object** %10, align 8
  %12 = bitcast %union.Object* %11 to %struct.anon*
  %13 = getelementptr inbounds %struct.anon* %12, i32 0, i32 0
  store %union.Object* %9, %union.Object** %13, align 8
  %14 = call i8* @gc_alloc_int(i32 20)
  %15 = bitcast i8* %14 to %union.Object*
  %16 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 0
  %17 = load %union.Object** %16, align 8
  %18 = bitcast %union.Object* %17 to %struct.anon*
  %19 = getelementptr inbounds %struct.anon* %18, i32 0, i32 1
  store %union.Object* %15, %union.Object** %19, align 8
  br label %20

; <label>:20                                      ; preds = %121, %2
  %21 = call i8* @gc_alloc(i32 0, i32 16)
  %22 = bitcast i8* %21 to %union.Object*
  %23 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 1
  store %union.Object* %22, %union.Object** %23, align 8
  %24 = call i8* @gc_alloc_int(i32 30)
  %25 = bitcast i8* %24 to %union.Object*
  %26 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 1
  %27 = load %union.Object** %26, align 8
  %28 = bitcast %union.Object* %27 to [0 x %union.Object*]*
  %29 = getelementptr inbounds [0 x %union.Object*]* %28, i32 0, i64 0
  store %union.Object* %25, %union.Object** %29, align 8
  %30 = call i8* @gc_alloc_int(i32 40)
  %31 = bitcast i8* %30 to %union.Object*
  %32 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 1
  %33 = load %union.Object** %32, align 8
  %34 = bitcast %union.Object* %33 to [0 x %union.Object*]*
  %35 = getelementptr inbounds [0 x %union.Object*]* %34, i32 0, i64 1
  store %union.Object* %31, %union.Object** %35, align 8
  br label %36

; <label>:36                                      ; preds = %121, %20
  %37 = call i8* @gc_alloc(i32 1, i32 8)
  %38 = bitcast i8* %37 to %union.Object*
  %39 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 2
  store %union.Object* %38, %union.Object** %39, align 8
  %40 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 2
  %41 = load %union.Object** %40, align 8
  %42 = bitcast %union.Object* %41 to [0 x i32]*
  %43 = getelementptr inbounds [0 x i32]* %42, i32 0, i64 0
  store i32 50, i32* %43, align 4
  %44 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 2
  %45 = load %union.Object** %44, align 8
  %46 = bitcast %union.Object* %45 to [0 x i32]*
  %47 = getelementptr inbounds [0 x i32]* %46, i32 0, i64 1
  store i32 60, i32* %47, align 4
  %48 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 0
  %49 = load %union.Object** %48, align 8
  %50 = bitcast %union.Object* %49 to %struct.anon*
  %51 = getelementptr inbounds %struct.anon* %50, i32 0, i32 0
  %52 = load %union.Object** %51, align 8
  %53 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 0
  %54 = load %union.Object** %53, align 8
  %55 = bitcast %union.Object* %54 to %struct.anon*
  %56 = getelementptr inbounds %struct.anon* %55, i32 0, i32 0
  %57 = load %union.Object** %56, align 8
  %58 = bitcast %union.Object* %57 to i32*
  %59 = load i32* %58, align 4
  %60 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str12, i32 0, i32 0), %union.Object* %52, i32 %59)
  %61 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 0
  %62 = load %union.Object** %61, align 8
  %63 = bitcast %union.Object* %62 to %struct.anon*
  %64 = getelementptr inbounds %struct.anon* %63, i32 0, i32 1
  %65 = load %union.Object** %64, align 8
  %66 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 0
  %67 = load %union.Object** %66, align 8
  %68 = bitcast %union.Object* %67 to %struct.anon*
  %69 = getelementptr inbounds %struct.anon* %68, i32 0, i32 1
  %70 = load %union.Object** %69, align 8
  %71 = bitcast %union.Object* %70 to i32*
  %72 = load i32* %71, align 4
  %73 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str13, i32 0, i32 0), %union.Object* %65, i32 %72)
  %74 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 1
  %75 = load %union.Object** %74, align 8
  %76 = bitcast %union.Object* %75 to [0 x %union.Object*]*
  %77 = getelementptr inbounds [0 x %union.Object*]* %76, i32 0, i64 0
  %78 = load %union.Object** %77, align 8
  %79 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 1
  %80 = load %union.Object** %79, align 8
  %81 = bitcast %union.Object* %80 to [0 x %union.Object*]*
  %82 = getelementptr inbounds [0 x %union.Object*]* %81, i32 0, i64 0
  %83 = load %union.Object** %82, align 8
  %84 = bitcast %union.Object* %83 to i32*
  %85 = load i32* %84, align 4
  %86 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str14, i32 0, i32 0), %union.Object* %78, i32 %85)
  %87 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 1
  %88 = load %union.Object** %87, align 8
  %89 = bitcast %union.Object* %88 to [0 x %union.Object*]*
  %90 = getelementptr inbounds [0 x %union.Object*]* %89, i32 0, i64 1
  %91 = load %union.Object** %90, align 8
  %92 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 1
  %93 = load %union.Object** %92, align 8
  %94 = bitcast %union.Object* %93 to [0 x %union.Object*]*
  %95 = getelementptr inbounds [0 x %union.Object*]* %94, i32 0, i64 1
  %96 = load %union.Object** %95, align 8
  %97 = bitcast %union.Object* %96 to i32*
  %98 = load i32* %97, align 4
  %99 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str15, i32 0, i32 0), %union.Object* %91, i32 %98)
  %100 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 2
  %101 = load %union.Object** %100, align 8
  %102 = bitcast %union.Object* %101 to [0 x i32]*
  %103 = getelementptr inbounds [0 x i32]* %102, i32 0, i64 0
  %104 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 2
  %105 = load %union.Object** %104, align 8
  %106 = bitcast %union.Object* %105 to [0 x i32]*
  %107 = getelementptr inbounds [0 x i32]* %106, i32 0, i64 0
  %108 = load i32* %107, align 4
  %109 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str16, i32 0, i32 0), i32* %103, i32 %108)
  %110 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 2
  %111 = load %union.Object** %110, align 8
  %112 = bitcast %union.Object* %111 to [0 x i32]*
  %113 = getelementptr inbounds [0 x i32]* %112, i32 0, i64 1
  %114 = getelementptr inbounds [4 x %union.Object*]* %frame, i32 0, i64 2
  %115 = load %union.Object** %114, align 8
  %116 = bitcast %union.Object* %115 to [0 x i32]*
  %117 = getelementptr inbounds [0 x i32]* %116, i32 0, i64 1
  %118 = load i32* %117, align 4
  %119 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str17, i32 0, i32 0), i32* %113, i32 %118)
  call void @gc_collect()
  br label %120

; <label>:120                                     ; preds = %121, %36
  ret void

; <label>:121                                     ; preds = %0
  %122 = phi i8* [ %1, %0 ]
  indirectbr i8* %122, [label %123, label %20, label %36, label %120, label %123, label %123, label %2]

; <label>:123                                     ; preds = %121, %121, %121
  call void @gc_add_frame_map(%struct.FrameMap* @test3.f)
  store i8* blockaddress(@test3, %2), i8** @test3.start_ptr, align 8
  br label %2
}

; Function Attrs: nounwind ssp uwtable
define %union.Object* @test_int(i32 %n) #0 {
  %1 = alloca i32, align 4
  %frame = alloca [2 x %union.Object*], align 16
  %a = alloca %union.Object*, align 8
  store i32 %n, i32* %1, align 4
  %2 = load i8** @test_int.start_ptr, align 8
  br label %12

; <label>:3                                       ; preds = %12
  %4 = load i32* %1, align 4
  %5 = call i8* @gc_alloc_int(i32 %4)
  %6 = bitcast i8* %5 to %union.Object*
  %7 = getelementptr inbounds [2 x %union.Object*]* %frame, i32 0, i64 0
  store %union.Object* %6, %union.Object** %7, align 8
  %8 = getelementptr inbounds [2 x %union.Object*]* %frame, i32 0, i64 0
  %9 = load %union.Object** %8, align 8
  store %union.Object* %9, %union.Object** %a, align 8
  br label %10

; <label>:10                                      ; preds = %12, %3
  %11 = load %union.Object** %a, align 8
  ret %union.Object* %11

; <label>:12                                      ; preds = %14, %0
  %13 = phi i8* [ %2, %0 ], [ %15, %14 ]
  indirectbr i8* %13, [label %14, label %10, label %14, label %14, label %3]

; <label>:14                                      ; preds = %12, %12, %12
  call void @gc_add_frame_map(%struct.FrameMap* @test_int.f)
  store i8* blockaddress(@test_int, %3), i8** @test_int.start_ptr, align 8
  %15 = load i8** @test_int.start_ptr, align 8
  br label %12
}

; Function Attrs: nounwind ssp uwtable
define void @test_record() #0 {
  %frame = alloca [2 x %union.Object*], align 16
  %1 = load i8** @test_record.start_ptr, align 8
  br label %26

; <label>:2                                       ; preds = %26
  %3 = call i8* @gc_alloc(i32 3, i32 25)
  %4 = bitcast i8* %3 to %union.Object*
  %5 = getelementptr inbounds [2 x %union.Object*]* %frame, i32 0, i64 0
  store %union.Object* %4, %union.Object** %5, align 8
  %6 = getelementptr inbounds [2 x %union.Object*]* %frame, i32 0, i64 0
  %7 = load %union.Object** %6, align 8
  %8 = bitcast %union.Object* %7 to [0 x i64]*
  %9 = getelementptr inbounds [0 x i64]* %8, i32 0, i64 0
  store i64 10, i64* %9, align 8
  %10 = call i8* @gc_alloc_int(i32 20)
  %11 = bitcast i8* %10 to %union.Object*
  %12 = getelementptr inbounds [2 x %union.Object*]* %frame, i32 0, i64 0
  %13 = load %union.Object** %12, align 8
  %14 = bitcast %union.Object* %13 to [0 x %union.Object*]*
  %15 = getelementptr inbounds [0 x %union.Object*]* %14, i32 0, i64 1
  store %union.Object* %11, %union.Object** %15, align 8
  %16 = call %union.Object* @test_int(i32 30)
  %17 = getelementptr inbounds [2 x %union.Object*]* %frame, i32 0, i64 0
  %18 = load %union.Object** %17, align 8
  %19 = bitcast %union.Object* %18 to [0 x %union.Object*]*
  %20 = getelementptr inbounds [0 x %union.Object*]* %19, i32 0, i64 2
  store %union.Object* %16, %union.Object** %20, align 8
  %21 = getelementptr inbounds [2 x %union.Object*]* %frame, i32 0, i64 0
  %22 = load %union.Object** %21, align 8
  %23 = bitcast %union.Object* %22 to [0 x i64]*
  %24 = getelementptr inbounds [0 x i64]* %23, i32 0, i64 3
  store i64 6, i64* %24, align 8
  call void @gc_collect()
  br label %25

; <label>:25                                      ; preds = %26, %2
  ret void

; <label>:26                                      ; preds = %28, %0
  %27 = phi i8* [ %1, %0 ], [ %29, %28 ]
  indirectbr i8* %27, [label %28, label %25, label %28, label %28, label %2]

; <label>:28                                      ; preds = %26, %26, %26
  call void @gc_add_frame_map(%struct.FrameMap* @test_record.f)
  store i8* blockaddress(@test_record, %2), i8** @test_record.start_ptr, align 8
  %29 = load i8** @test_record.start_ptr, align 8
  br label %26
}

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  store i32 0, i32* %1
  call void @gc_init()
  call void @test()
  call void @gc_free()
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str18, i32 0, i32 0))
  call void @gc_init()
  call void @test2()
  call void @gc_free()
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str18, i32 0, i32 0))
  call void @gc_init()
  call void @test3()
  call void @gc_free()
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str18, i32 0, i32 0))
  call void @gc_init()
  call void @test_record()
  call void @gc_free()
  %5 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([28 x i8]* @.str19, i32 0, i32 0), i64 4, i64 16)
  ret i32 0
}

