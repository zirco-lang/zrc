#include <stdint.h>
#include "../str.h"
#include "../internal.h"

#define BUILD_SHL_FUNC(name, type)                            \
    type shl_##name(type value, uint32_t shift)               \
    {                                                         \
        if (shift >= sizeof(type) * 8)                        \
        {                                                     \
            _libzr_crash_out(str_from_cstr(                   \
                "Shift amount out of bounds in shl_" #name)); \
        }                                                     \
        return value << shift;                                \
    }

#define BUILD_SHR_FUNC(name, type)                            \
    type shr_##name(type value, uint32_t shift)               \
    {                                                         \
        if (shift >= sizeof(type) * 8)                        \
        {                                                     \
            _libzr_crash_out(str_from_cstr(                   \
                "Shift amount out of bounds in shr_" #name)); \
        }                                                     \
        return value >> shift;                                \
    }

BUILD_SHL_FUNC(i8, int8_t)
BUILD_SHL_FUNC(i16, int16_t)
BUILD_SHL_FUNC(i32, int32_t)
BUILD_SHL_FUNC(i64, int64_t)
BUILD_SHL_FUNC(u8, uint8_t)
BUILD_SHL_FUNC(u16, uint16_t)
BUILD_SHL_FUNC(u32, uint32_t)
BUILD_SHL_FUNC(u64, uint64_t)
BUILD_SHR_FUNC(i8, int8_t)
BUILD_SHR_FUNC(i16, int16_t)
BUILD_SHR_FUNC(i32, int32_t)
BUILD_SHR_FUNC(i64, int64_t)
BUILD_SHR_FUNC(u8, uint8_t)
BUILD_SHR_FUNC(u16, uint16_t)
BUILD_SHR_FUNC(u32, uint32_t)
BUILD_SHR_FUNC(u64, uint64_t)
