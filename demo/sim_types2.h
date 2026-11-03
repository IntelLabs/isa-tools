#ifndef SIM_TYPES_H
#define SIM_TYPES_H

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#ifndef ASL_C23
#define ASL_C23
#endif
#include "asl/runtime.h"

#ifdef __cplusplus
extern "C" {
#endif

struct threadlocal_state;

void Print_State_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr);

void ASL_WriteMemory8_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, uint64_t address, uint8_t x);

void RAM_write_size_in_bytes_1_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, uint64_t address, uint8_t value);

void ASL_WriteReg64_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, __int128 index, uint64_t val);

uint64_t ASL_ReadReg64_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, __int128 index);

void Unimplemented_Feature_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, const char *name);

bool ASL_IsHalted_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr);

void ASL_Step_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr);

void Step_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr);

uint8_t RAM_size_in_bytes_1_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, uint64_t address);

void Instr_VINC_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, const unsigned _BitInt(2048) *src, unsigned _BitInt(2048) *dest);

__int128 Std$Integer$Exp_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr, __int128 left, __int128 right);

void ASL_Reset_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr);

void Reset_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr);

void RAM_Init_0(void* client_arg, struct threadlocal_state *threadlocal_state_ptr);

#ifdef __cplusplus
}
#endif

#endif  // SIM_TYPES_H
