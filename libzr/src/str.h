#pragma once
#include <stddef.h>
#include <stdbool.h>

typedef struct
{
    size_t len;
    char *ptr;
} Str;

Str str_from_cstr(const char *s);
const char *cstr_from_str(Str s);
Str str_clone(Str s);
bool str_equal(Str a, Str b);
Str str_slice(Str s, size_t start, size_t end);
