#include <stdint.h>

#define BUILD_SHL_FUNC(name, type)              \
    type shl_##name(type value, uint32_t shift) \
    {                                           \
        return value << shift;                  \
    }

#define BUILD_SHR_FUNC(name, type)              \
    type shr_##name(type value, uint32_t shift) \
    {                                           \
        return value >> shift;                  \
    }

BUILD_SHL_FUNC(i8, uint8_t)
BUILD_SHL_FUNC(i16, uint16_t)
BUILD_SHL_FUNC(i32, uint32_t)
BUILD_SHL_FUNC(i64, uint64_t)
BUILD_SHL_FUNC(u8, uint8_t)
BUILD_SHL_FUNC(u16, uint16_t)
BUILD_SHL_FUNC(u32, uint32_t)
BUILD_SHL_FUNC(u64, uint64_t)
BUILD_SHR_FUNC(i8, uint8_t)
BUILD_SHR_FUNC(i16, uint16_t)
BUILD_SHR_FUNC(i32, uint32_t)
BUILD_SHR_FUNC(i64, uint64_t)
BUILD_SHR_FUNC(u8, uint8_t)
BUILD_SHR_FUNC(u16, uint16_t)
BUILD_SHR_FUNC(u32, uint32_t)
BUILD_SHR_FUNC(u64, uint64_t)
