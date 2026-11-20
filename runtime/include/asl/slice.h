////////////////////////////////////////////////////////////////
// Runtime library for ASL's C backend: bitvector slices
//
// Copyright (C) 2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_SLICE_H
#define ASL_SLICE_H

#ifdef ASL_C23

/*
 * Fast extraction and assignment of power-of-2 sized slices of wide
 * bitvectors.
 *
 * ASL_get_slice_<n>_<w>(x, lo) returns bits [lo +: w] of the n-bit
 * bitvector x for n in {256, 512, 1024, 2048} and w in
 * {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024} with w < n.
 *
 * ASL_set_slice_<w>(x, lo, v) sets bits [lo +: w] of the bitvector
 * pointed to by x to v for w in {1, 2, 4, 8, 16, 32, 64, 128, 256, 512,
 * 1024}.
 *
 * Instead of shifting (and masking) the entire bitvector, the slice is
 * read from or written to memory directly. This relies on the bitvector
 * (an unsigned _BitInt) being stored in little-endian order, so that
 * bit 'lo' is in byte 'lo / 8', and on 'lo' being a multiple of 8
 * (for w >= 8) or a multiple of w (for w < 8, so that the slice lies
 * within a single byte).
 */

#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "asl/bits.h"

#if __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#error "slice.h requires little-endian target"
#endif

// GCC (seen with 15.2, 16.1 and 16.2 at -O2) miscompiles reading part of
// a copy of an unsigned _BitInt through memcpy when the read is inlined
// into a loop and the vectorizer is enabled: reads at non-zero offsets
// return stale data. Since x is a copy (a by-value parameter), these
// functions must not be inlined into the caller when compiling with GCC.
#if defined(__GNUC__) && !defined(__clang__)
#define ASL_GET_SLICE_ATTR __attribute__((noinline, unused)) static
#else
#define ASL_GET_SLICE_ATTR static inline
#endif

#define ASL_GET_SLICE(n, w)                                       \
        ASL_GET_SLICE_ATTR ASL_bits##w##_t                        \
        ASL_get_slice_##n##_##w(ASL_bits##n##_t x, int64_t lo)    \
        {                                                         \
                ASL_bits##w##_t r;                                \
                memcpy(&r, (const char *)&x + lo / 8, sizeof(r)); \
                return r;                                         \
        }

// Slices narrower than a byte are read from the byte containing them
#define ASL_GET_SUBBYTE_SLICE(n, w)                               \
        ASL_GET_SLICE_ATTR ASL_bits##w##_t                        \
        ASL_get_slice_##n##_##w(ASL_bits##n##_t x, int64_t lo)    \
        {                                                         \
                uint8_t b;                                        \
                memcpy(&b, (const char *)&x + lo / 8, sizeof(b)); \
                return (ASL_bits##w##_t)(b >> (lo % 8));          \
        }

#define ASL_GET_SLICE_N(n)          \
        ASL_GET_SUBBYTE_SLICE(n, 1) \
        ASL_GET_SUBBYTE_SLICE(n, 2) \
        ASL_GET_SUBBYTE_SLICE(n, 4) \
        ASL_GET_SLICE(n, 8)         \
        ASL_GET_SLICE(n, 16)        \
        ASL_GET_SLICE(n, 32)        \
        ASL_GET_SLICE(n, 64)        \
        ASL_GET_SLICE(n, 128)

#if BITINT_MAXWIDTH >= 256
ASL_GET_SLICE_N(256)
#endif
#if BITINT_MAXWIDTH >= 512
ASL_GET_SLICE_N(512)
ASL_GET_SLICE(512, 256)
#endif
#if BITINT_MAXWIDTH >= 1024
ASL_GET_SLICE_N(1024)
ASL_GET_SLICE(1024, 256)
ASL_GET_SLICE(1024, 512)
#endif
#if BITINT_MAXWIDTH >= 2048
ASL_GET_SLICE_N(2048)
ASL_GET_SLICE(2048, 256)
ASL_GET_SLICE(2048, 512)
ASL_GET_SLICE(2048, 1024)
#endif

#undef ASL_GET_SLICE_N
#undef ASL_GET_SUBBYTE_SLICE
#undef ASL_GET_SLICE
#undef ASL_GET_SLICE_ATTR

#define ASL_SET_SLICE(w)                                          \
        static inline void                                        \
        ASL_set_slice_##w(void *x, int64_t lo, ASL_bits##w##_t v) \
        {                                                         \
                memcpy((char *)x + lo / 8, &v, sizeof(v));        \
        }

// Slices narrower than a byte are merged into the byte containing them
#define ASL_SET_SUBBYTE_SLICE(w)                                      \
        static inline void                                            \
        ASL_set_slice_##w(void *x, int64_t lo, ASL_bits##w##_t v)     \
        {                                                             \
                unsigned char *p = (unsigned char *)x + lo / 8;       \
                unsigned shift = lo % 8;                              \
                uint8_t mask = (uint8_t)(((1u << w) - 1) << shift);   \
                *p = (uint8_t)((*p & ~mask) | ((uint8_t)v << shift)); \
        }

ASL_SET_SUBBYTE_SLICE(1)
ASL_SET_SUBBYTE_SLICE(2)
ASL_SET_SUBBYTE_SLICE(4)
ASL_SET_SLICE(8)
ASL_SET_SLICE(16)
ASL_SET_SLICE(32)
ASL_SET_SLICE(64)
ASL_SET_SLICE(128)
#if BITINT_MAXWIDTH >= 256
ASL_SET_SLICE(256)
#endif
#if BITINT_MAXWIDTH >= 512
ASL_SET_SLICE(512)
#endif
#if BITINT_MAXWIDTH >= 1024
ASL_SET_SLICE(1024)
#endif

#undef ASL_SET_SUBBYTE_SLICE
#undef ASL_SET_SLICE

#endif  // ASL_C23

#endif  // ASL_SLICE_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
