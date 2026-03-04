////////////////////////////////////////////////////////////////
// Runtime info support for ASL's C backend
//
// Copyright (C) 2026-2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_INFO_H
#define ASL_INFO_H

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void ASL_info(int level, const char *fmt, ...);

#if defined(ASL_INT128) || defined(ASL_C23)

#define ASL_INT_BUFSZ 64

/* Format a signed 128-bit integer as a string.
   Formats small values, fitting in int64_t, in decimal, large numbers in hex.
   The caller provides the buffer (use compound literal for thread-safety). */
static inline const char *
ASL_format_int_as_str(char *buf, __int128 x)
{
        // Check if fits into int64_t (long long)
        if ((__int128)(long long)x == x) {
                snprintf(buf, ASL_INT_BUFSZ, "%lld", (long long)x);
        } else {
                char *p = buf;
                unsigned __int128 ux = (unsigned __int128)x;
                if (x < 0) {
                        *p++ = '-';
                        ux = -ux;
                }
                snprintf(p, (size_t)(buf + ASL_INT_BUFSZ - p), "0x%llx%016llx",
                         (unsigned long long)(ux >> 64),
                         (unsigned long long)ux);
        }

        return buf;
}

#endif

#ifdef __cplusplus
}
#endif

#endif  // ASL_INFO_H
