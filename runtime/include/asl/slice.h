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
 * Fast extraction of power-of-2 sized slices from wide bitvectors.
 *
 * ASL_get_slice_<n>_<w>(x, lo) returns bits [lo +: w] of the n-bit
 * bitvector x for n in {256, 512, 1024, 2048} and w in
 * {8, 16, 32, 64, 128, 256, 512, 1024} with w < n.
 *
 * Instead of shifting the entire bitvector, the slice is read directly
 * from memory. This relies on the bitvector (an unsigned _BitInt) being
 * stored in little-endian order, so that bit 'lo' is in byte 'lo / 8',
 * and on 'lo' being a multiple of 8.
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

#define ASL_GET_SLICE_N(n)    \
        ASL_GET_SLICE(n, 8)   \
        ASL_GET_SLICE(n, 16)  \
        ASL_GET_SLICE(n, 32)  \
        ASL_GET_SLICE(n, 64)  \
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
#undef ASL_GET_SLICE
#undef ASL_GET_SLICE_ATTR

#endif  // ASL_C23

#endif  // ASL_SLICE_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
