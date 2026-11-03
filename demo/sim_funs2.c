#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#ifndef ASL_C23
#define ASL_C23
#endif
#include "asl/runtime.h"

#include "sim_types2.h"
#include "sim_exceptions.h"
#include "sim_vars.h"

void Step_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr) {
    uint8_t opcode;
    if (threadlocal_state_ptr->halted) {
        return;
    }
    opcode = RAM_size_in_bytes_1_0(client_arg, threadlocal_state_ptr, threadlocal_state_ptr->PC);
    if (((opcode & 0xfc) == 0x10)) {
        __int128 r;
        r = ((__int128)((unsigned __int128)((unsigned _BitInt(2))(opcode >> 0))));
        threadlocal_state_ptr->R[((int64_t)r)] = ((uint64_t)(threadlocal_state_ptr->R[((int64_t)r)] + ((uint64_t)0x1uwb)));
    } else if (((opcode & 0xf0) == 0x20)) {
        __int128 r;
        __int128 d;
        unsigned _BitInt(2048) src;
        unsigned _BitInt(2048) dest;
        r = ((__int128)((unsigned __int128)((unsigned _BitInt(2))(opcode >> 0))));
        d = ((__int128)((unsigned __int128)((unsigned _BitInt(2))(opcode >> 2))));
        src = threadlocal_state_ptr->V[((int64_t)r)];
        Instr_VINC_0(client_arg, threadlocal_state_ptr, &src, &dest);
        threadlocal_state_ptr->V[((int64_t)d)] = dest;
    } else {
        threadlocal_state_ptr->halted = true;
    }
    threadlocal_state_ptr->PC = ((uint64_t)(threadlocal_state_ptr->PC + ((uint64_t)0x1uwb)));
}

uint8_t RAM_size_in_bytes_1_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, uint64_t address) {
    return ASL_ram_read(64, 1, global_state_ptr->__Memory, address);
}

void write_32(unsigned _BitInt(2048) *x, __int128 i, uint32_t y)
{
    ((uint32_t*)x)[i] = y;
}

uint32_t read_32(const unsigned _BitInt(2048) *x, __int128 i)
{
    return ((uint32_t*)x)[i];
}

void Instr_VINC_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, const unsigned _BitInt(2048) *restrict src, unsigned _BitInt(2048) *restrict dest)
{
    __int128 elements;
    
    elements = ((__int128)0x40wb);
    for (__int128 i = ((__int128)0x0wb); i <= ((__int128)0x3fwb); ++i) {
#if 0
        *dest = (((*dest) & (~ ((unsigned _BitInt(2048))(((unsigned _BitInt(2048))0xffffffffuwb) << ((__int128)(((__int128)0x20wb) * i)))))) | ((unsigned _BitInt(2048))(((unsigned _BitInt(2048))(uint32_t)((uint32_t)(((uint32_t)(*src >> ((int64_t)((__int128)(((__int128)0x20wb) * i))))) + ((uint32_t)0x1uwb)))) << ((__int128)(((__int128)0x20wb) * i)))));
#else
        write_32(dest, i, read_32(src, i) + 1);
#endif
    }
}
