// FFI testing support functions to be imported into ASL test programs
// Copyright (C) 2025-2026 Intel Corporation
#include <stdint.h>
#include "isa_ffi.h"

// Wide bitvectors are represented as arrays of uint64_t
// If returning a wide bitvector, an extra array argument is
// added as the last argument

void FFI_bits_bool(int64_t x, uint64_t r0[2], bool *r1)
{
        for(int i = 0; i < 2; ++i) {
                r0[i] = (uint64_t)x;
        }
        *r1 = true;
}
