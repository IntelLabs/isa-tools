(****************************************************************
 * ISA global variable wrapping transform
 *
 * It supplements global variables with functions for reading and
 * writing the variables, and replaces use of the variables by
 * calls to the functions.
 *
 * For variable x the names of created functions are x_Read and
 * x_Write.
 *
 * Because the variables typically represent processor or memory
 * state, a simulator can redefine the functions to provide access
 * to its own simulation state instead.
 *
 * Partial assignments, e.g., to a slice of the variable, read the
 * variable into a temporary, update the temporary and write it back.
 *
 * Copyright (C) 2024-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
open Isa_utils
open Identset

let mk_read_fident = Xform_var_access.mk_read_fident
let mk_write_fident = Xform_var_access.mk_write_fident

(* Set of identifiers of variables to be wrapped *)
let decl_vars (ds : AST.declaration list) : IdentSet.t =
  ds
  |> List.filter_map (function
       | AST.Decl_Var (v, ty, loc) -> Some v
       | _ -> None)
  |> IdentSet.of_list

let tmpVariables = new Isa_utils.nameSupply "__wrap"

class replaceClass (ds : AST.declaration list) =
  object
    inherit Xform_var_access.replacerClass tmpVariables ds (decl_vars ds)
  end

let xform_decl (replacer : replaceClass) (d : AST.declaration) :
    AST.declaration list =
  let d' = Isa_visitor.visit_decl (replacer :> Isa_visitor.isaVisitor) d in
  match d' with
  | Decl_Var (v, Type_Array (ixty, ty), loc) ->
      let i = Ident.mk_ident "i" in
      let rd_f = mk_read_fident v in
      let rd_fty : AST.function_type = {
        parameters=[];
        args=[(i, ixtype_basetype ixty, None)];
        setter_arg=None;
        rty=ty;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow;
        is_builtin=false;
      } in
      let rd_type = AST.Decl_FunType (rd_f, rd_fty, loc) in
      let rd_body = [ AST.Stmt_Return (Expr_Array (Expr_Var v, Expr_Var i), loc) ] in
      let rd_defn = AST.Decl_FunDefn (rd_f, rd_fty, rd_body, loc) in

      let wr_f = mk_write_fident v in
      let vl = Ident.mk_ident "v" in
      let wr_fty : AST.function_type = {
        parameters=[];
        args=[(i, ixtype_basetype ixty, None); (vl, ty, None)];
        rty=type_unit;
        setter_arg=None;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow;
        is_builtin=false;
      } in
      let wr_type = AST.Decl_FunType (wr_f, wr_fty, loc) in
      let wr_body = [ AST.Stmt_Assign (LExpr_Array (LExpr_Var v, Expr_Var i), Expr_Var vl, loc) ] in
      let wr_defn = AST.Decl_FunDefn (wr_f, wr_fty, wr_body, loc) in

      [ d' ; rd_type ; wr_type ; rd_defn ; wr_defn ]
  | Decl_Var (v, ty, loc) ->
      let rd_f = mk_read_fident v in
      let rd_fty : AST.function_type = {
        parameters=[];
        args=[];
        rty=ty;
        setter_arg=None;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow;
        is_builtin=false;
      } in
      let rd_type = AST.Decl_FunType (rd_f, rd_fty, loc) in
      let rd_body = [ AST.Stmt_Return (Expr_Var v, loc) ] in
      let rd_defn = AST.Decl_FunDefn (rd_f, rd_fty, rd_body, loc) in

      let wr_f = mk_write_fident v in
      let vl = Ident.mk_ident "v" in
      let wr_fty : AST.function_type = {
        parameters=[];
        args=[(vl, ty, None)];
        rty=type_unit;
        setter_arg=None;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow;
        is_builtin=false;
      } in
      let wr_type = AST.Decl_FunType (wr_f, wr_fty, loc) in
      let wr_body = [ AST.Stmt_Assign (LExpr_Var v, Expr_Var vl, loc) ] in
      let wr_defn = AST.Decl_FunDefn (wr_f, wr_fty, wr_body, loc) in

      [ d' ; rd_type ; wr_type ; rd_defn ; wr_defn ]
  | _ -> [ d' ]

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replaceClass ds in
  ds
  |> List.map (xform_decl replacer)
  |> List.concat
  (* Re-hoist function prototypes because the transformation adds new ones *)
  |> hoist_prototypes

(****************************************************************
 * Command: :xform_wrap
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_wrap" [] [] [] "Wrap global variables into functions" cmd

(****************************************************************
 * End
 ****************************************************************)
