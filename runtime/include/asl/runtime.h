////////////////////////////////////////////////////////////////
// Runtime library for ASL's C backend
//
// Copyright (C) 2022-2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_RUNTIME_H
#define ASL_RUNTIME_H

#if defined(ASL_C23)

#include <stdio.h>
#include <stdint.h>

// The following types are used in the foreign function interface
typedef __int128 ASL_int_t;

#include "asl/bits.h"

#else // defined(ASL_FALLBACK)

// The following types are used in the foreign function interface
// Note that they are all 64 bits wide in this runtime.
#include <stdint.h>
typedef uint64_t ASL_bits8_t;
typedef uint64_t ASL_bits16_t;
typedef uint64_t ASL_bits32_t;

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"
#include "asl/bits1024.h"
#include "asl/bits2048.h"
#include "asl/bits4096.h"
#include "asl/bits8192.h"
#include "asl/integer.h"
#include "asl/print.h"

#endif // ASL_FALLBACK

#include "asl/error.h"
#include "asl/info.h"
#include "asl/ram.h"
#include "asl/slice.h"
#include "asl/track_valid.h"

#endif  // ASL_RUNTIME_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
