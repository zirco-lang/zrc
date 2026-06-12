#ifndef ZRC_H
#define ZRC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * Free a string returned by the zrc C API.
 *
 * # Safety
 * The caller must guarantee that `s` is a valid pointer to a null-terminated C
 * string that was returned by a function in the zrc C API, and that it has not
 * already been freed.
 */
void zrc_free_string(char *str);

#endif  /* ZRC_H */
