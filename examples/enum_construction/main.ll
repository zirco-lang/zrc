; ModuleID = 'main.zr'
source_filename = "main.zr"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define { i64, i32 } @divide(i32 %0, i32 %1) local_unnamed_addr #0 !dbg !3 {
entry:
  call void @llvm.dbg.value(metadata i32 %0, metadata !15, metadata !DIExpression()), !dbg !17
  call void @llvm.dbg.value(metadata i32 %1, metadata !16, metadata !DIExpression()), !dbg !17
  %cmp = icmp eq i32 %1, 0, !dbg !19
  br i1 %cmp, label %common.ret, label %then_else, !dbg !19

common.ret:                                       ; preds = %entry, %then_else
  %common.ret.op = phi { i64, i32 } [ %struct_val710, %then_else ], [ { i64 1, i32 -1 }, %entry ]
  ret { i64, i32 } %common.ret.op, !dbg !21

then_else:                                        ; preds = %entry
  %div = sdiv i32 %0, %1, !dbg !22
  %struct_val710 = insertvalue { i64, i32 } { i64 0, i32 poison }, i32 %div, 1, !dbg !22
  br label %common.ret
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @main() local_unnamed_addr #0 !dbg !24 {
entry:
  call void @llvm.dbg.value(metadata { i64, i32 } poison, metadata !28, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata { i64, i32 } poison, metadata !31, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata { i64, i32 } poison, metadata !32, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata { i64, i32 } poison, metadata !33, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata { i64, i64 } poison, metadata !34, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata { i64, i64 } poison, metadata !41, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i64 poison, metadata !42, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i64 poison, metadata !43, metadata !DIExpression()), !dbg !44
  ret i32 0, !dbg !45
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0}
!llvm.dbg.cu = !{!1}

!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = distinct !DICompileUnit(language: DW_LANG_C, file: !2, producer: "zrc version 0.1.0 (f7330b8567bd8196253250b7780794de676f71dd, tainted!) built for x86_64-unknown-linux-gnu on 2025-10-08T18:04:46Z (debug mode)\0Arustc 1.90.0 (1159e78c4 2025-09-14) (stable-x86_64-unknown-linux-gnu on linux-x86_64)\0Acargo 1.90.0 (840b83a10 2025-07-30)\0Atainted files:\0A  * compiler/zrc_typeck/src/typeck/expr/misc.rs", isOptimized: false, flags: "../../target/debug/zrc main.zr", runtimeVersion: 0, emissionKind: NoDebug, splitDebugInlining: false)
!2 = !DIFile(filename: "main.zr", directory: "/home/runner/work/zrc/zrc/examples/enum_construction")
!3 = distinct !DISubprogram(name: "divide", linkageName: "divide", scope: null, file: !2, line: 15, type: !4, scopeLine: 15, spFlags: DISPFlagDefinition, unit: !1, retainedNodes: !14)
!4 = !DISubroutineType(types: !5)
!5 = !{!6, !13, !13}
!6 = !DICompositeType(tag: DW_TAG_structure_type, name: "struct { __discriminant__: usize, __value__: union { Ok: i32, Error: i32 } }", scope: !2, file: !2, elements: !7)
!7 = !{!8, !10}
!8 = !DIDerivedType(tag: DW_TAG_member, name: "__discriminant__", scope: !2, file: !2, baseType: !9)
!9 = !DIBasicType(name: "usize")
!10 = !DIDerivedType(tag: DW_TAG_member, name: "__value__", scope: !2, file: !2, baseType: !11)
!11 = !DICompositeType(tag: DW_TAG_union_type, name: "union { Ok: i32, Error: i32 }", scope: !2, file: !2, elements: !12)
!12 = !{!13, !13}
!13 = !DIBasicType(name: "i32")
!14 = !{!15, !16}
!15 = !DILocalVariable(name: "a", scope: !3, file: !2, line: 15, type: !13)
!16 = !DILocalVariable(name: "b", arg: 1, scope: !3, file: !2, line: 15, type: !13)
!17 = !DILocation(line: 0, scope: !18)
!18 = distinct !DILexicalBlock(scope: !3, file: !2, line: 15, column: 37)
!19 = !DILocation(line: 16, column: 14, scope: !20)
!20 = distinct !DILexicalBlock(scope: !18, file: !2, line: 15, column: 37)
!21 = !DILocation(line: 0, scope: !20)
!22 = !DILocation(line: 19, column: 37, scope: !23)
!23 = distinct !DILexicalBlock(scope: !20, file: !2, line: 18, column: 12)
!24 = distinct !DISubprogram(name: "main", linkageName: "main", scope: null, file: !2, line: 23, type: !25, scopeLine: 23, spFlags: DISPFlagDefinition, unit: !1, retainedNodes: !27)
!25 = !DISubroutineType(types: !26)
!26 = !{!13}
!27 = !{!28, !31, !32, !33, !34, !41, !42, !43}
!28 = !DILocalVariable(name: "success", scope: !29, file: !2, line: 25, type: !6)
!29 = distinct !DILexicalBlock(scope: !30, file: !2, line: 23, column: 18)
!30 = distinct !DILexicalBlock(scope: !24, file: !2, line: 23, column: 18)
!31 = !DILocalVariable(name: "failure", scope: !29, file: !2, line: 26, type: !6)
!32 = !DILocalVariable(name: "div_result", scope: !29, file: !2, line: 29, type: !6)
!33 = !DILocalVariable(name: "div_error", scope: !29, file: !2, line: 30, type: !6)
!34 = !DILocalVariable(name: "small_val", scope: !29, file: !2, line: 33, type: !35)
!35 = !DICompositeType(tag: DW_TAG_structure_type, name: "struct { __discriminant__: usize, __value__: union { Small: i32, Large: i64 } }", scope: !2, file: !2, elements: !36)
!36 = !{!8, !37}
!37 = !DIDerivedType(tag: DW_TAG_member, name: "__value__", scope: !2, file: !2, baseType: !38)
!38 = !DICompositeType(tag: DW_TAG_union_type, name: "union { Small: i32, Large: i64 }", scope: !2, file: !2, elements: !39)
!39 = !{!13, !40}
!40 = !DIBasicType(name: "i64")
!41 = !DILocalVariable(name: "large_val", scope: !29, file: !2, line: 34, type: !35)
!42 = !DILocalVariable(name: "success_discriminant", scope: !29, file: !2, line: 37, type: !9)
!43 = !DILocalVariable(name: "failure_discriminant", scope: !29, file: !2, line: 38, type: !9)
!44 = !DILocation(line: 0, scope: !29)
!45 = !DILocation(line: 41, column: 12, scope: !29)
