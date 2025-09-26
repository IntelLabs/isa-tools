// FFI testing support functions to be imported into ISA test programs
// Copyright (C) 2025-2025 Intel Corporation

#include <assert.h>
#include <stdint.h>

#include "isa_ffi.h"

static int x = 0;

int v1_Read(void)
{
        return x + 1;
}

void v1_Write(int value)
{
        x = value + 1;
}

#define SIZE 1
static int y[SIZE] = { 0 };

int v2_Read(int i)
{
        assert(i >= 0 && i < SIZE);
        return y[i] + 1;
}

void v2_Write(int i, int value)
{
        assert(i >= 0 && i < SIZE);
        y[i] = value + 1;
}
