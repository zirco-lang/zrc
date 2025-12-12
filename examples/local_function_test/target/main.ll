; ModuleID = 'main.zr'
source_filename = "main.zr"

@str = private unnamed_addr constant [24 x i8] c"Helper called with: %d\0A\00", align 1
@str.1 = private unnamed_addr constant [12 x i8] c"Result: %d\0A\00", align 1
@str.2 = private unnamed_addr constant [19 x i8] c"Public result: %d\0A\00", align 1

declare i32 @printf(ptr, ...)

declare i32 @scanf(ptr, ...)

declare i32 @sscanf(ptr, ptr, ...)

declare i32 @vprintf(ptr, ...)

declare i32 @vsprintf(ptr, ptr, ...)

declare i32 @vsnprintf(ptr, i64, ptr, ...)

declare i32 @getchar()

declare ptr @gets(ptr)

declare i32 @putchar(i32)

declare i32 @puts(ptr)

declare i32 @sprintf(ptr, ptr, ...)

declare i32 @snprintf(ptr, i64, ptr, ...)

declare ptr @fopen(ptr, ptr)

declare i32 @fclose(ptr)

declare i32 @fprintf(ptr, ptr, ...)

declare i64 @fread(ptr, i64, i64, ptr)

declare i32 @remove(ptr)

define internal i32 @helper(i32 %0) {
entry:
  %let_result = alloca i32, align 4
  %arg_x = alloca i32, align 4
  store i32 %0, ptr %arg_x, align 4
  %load = load i32, ptr %arg_x, align 4
  %mul = mul i32 %load, 2
  store i32 %mul, ptr %let_result, align 4
  %load1 = load i32, ptr %arg_x, align 4
  %call = call i32 (ptr, ...) @printf(ptr @str, i32 %load1)
  %load2 = load i32, ptr %let_result, align 4
  ret i32 %load2
}

define i32 @public_helper(i32 %0) {
entry:
  %arg_x = alloca i32, align 4
  store i32 %0, ptr %arg_x, align 4
  %load = load i32, ptr %arg_x, align 4
  %mul = mul i32 %load, 3
  ret i32 %mul
}

define i32 @main() {
entry:
  %let_result2 = alloca i32, align 4
  %let_result = alloca i32, align 4
  %call = call i32 @helper(i32 21)
  store i32 %call, ptr %let_result, align 4
  %load = load i32, ptr %let_result, align 4
  %call1 = call i32 (ptr, ...) @printf(ptr @str.1, i32 %load)
  %call2 = call i32 @public_helper(i32 10)
  store i32 %call2, ptr %let_result2, align 4
  %load3 = load i32, ptr %let_result2, align 4
  %call4 = call i32 (ptr, ...) @printf(ptr @str.2, i32 %load3)
  ret i32 0
}
