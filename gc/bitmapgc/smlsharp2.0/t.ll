; RUN: opt -S -consthoist < %s | FileCheck %s

target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

declare void @tackmap(i64, i32)
define i128 @test1(i128 %a) {
entry:
  %0 = add i128 %a, 134646182756734033220
  tail call void @tackmap(i64 1, i32 240)
  ret i128 %0
}


