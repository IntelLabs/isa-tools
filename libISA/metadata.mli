(****************************************************************
 * Metadata generator
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

(** Write metadata to a json file *)
val generate_callgraph : string -> Isa_ast.declaration list -> unit

(****************************************************************
 * End
 ****************************************************************)
