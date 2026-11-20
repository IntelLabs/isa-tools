////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend (C23)
//
// Copyright (C) 2022-2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS_H
#define ASL_BITS_H

#include <limits.h>
#include <stdint.h>

// The following types are used in the foreign function interface
typedef uint8_t ASL_bits8_t;
typedef uint16_t ASL_bits16_t;
typedef uint32_t ASL_bits32_t;
typedef uint64_t ASL_bits64_t;
typedef unsigned __int128 ASL_bits128_t;
#if BITINT_MAXWIDTH >= 256
typedef unsigned _BitInt(256) ASL_bits256_t;
#endif
#if BITINT_MAXWIDTH >= 512
typedef unsigned _BitInt(512) ASL_bits512_t;
#endif
#if BITINT_MAXWIDTH >= 1024
typedef unsigned _BitInt(1024) ASL_bits1024_t;
#endif
#if BITINT_MAXWIDTH >= 2048
typedef unsigned _BitInt(2048) ASL_bits2048_t;
#endif

// Sub-byte bitvectors used by the runtime
typedef unsigned _BitInt(1) ASL_bits1_t;
typedef unsigned _BitInt(2) ASL_bits2_t;
typedef unsigned _BitInt(4) ASL_bits4_t;

#define ASL_cast_bits_512_64(x) ((ASL_bits64_t)(x))
#define ASL_cast_bits_256_64(x) ((ASL_bits64_t)(x))
#define ASL_cast_bits_128_64(x) ((ASL_bits64_t)(x))

#endif // ASL_BITS_H
