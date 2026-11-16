(****************************************************************
 * ISA global variable access transform
 *
 * Replaces all uses of a set of global variables with calls to
 * functions to read/write the variables. Used by xform_foreign
 * and xform_wrap.
 *
 * For variable x the names of the functions are x_Read.0 and
 * x_Write.0, e.g.,
 *   y := x;          =>  y := x_Read();
 *   x := e;          =>  x_Write(e);
 *   a[i] := e;       =>  a_Write(i, e);
 *
 * Partial assignments, e.g., to a slice of the variable, read the
 * variable into a temporary, update the temporary and write it
 * back:
 *   x[i +: 8] := e;
 * =>
 *   var tmp := x_Read();
 *   tmp[i +: 8] := e;
 *   x_Write(tmp);
 *
 * Copyright (C) 2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
open Identset

let mk_read_fident (i : Ident.t) : Ident.t =
  Ident.add_suffix (Ident.mk_fident_with_tag i ~tag:0) ~suffix:"Read"

let mk_write_fident (i : Ident.t) : Ident.t =
  Ident.add_suffix (Ident.mk_fident_with_tag i ~tag:0) ~suffix:"Write"

(* Replace accesses to the variables in 'vs' declared in 'ds'.
 * Temporaries for partial assignments are named using 'tmps'.
 *)
class replacerClass (tmps : Isa_utils.nameSupply) (ds : AST.declaration list) (vs : IdentSet.t) =
  object (self)
    inherit Isa_visitor.nopIsaVisitor

    (* Types of the variables (element type for arrays) *)
    val var_types : AST.ty Bindings.t =
      ds
      |> List.filter_map (function
           | AST.Decl_Var (v, Type_Array (_, ty), _) when IdentSet.mem v vs -> Some (v, ty)
           | AST.Decl_Var (v, ty, _) when IdentSet.mem v vs -> Some (v, ty)
           | _ -> None)
      |> List.to_seq
      |> Bindings.of_seq

    method is_replaced (v : Ident.t) : bool = Bindings.mem v var_types

    method! vexpr e =
      match e with
      | Expr_Array (Expr_Var v, i) when self#is_replaced v ->
          let f = mk_read_fident v in
          let e' = AST.Expr_TApply (f, [], [ i ], NoThrow) in
          ChangeDoChildrenPost (e', Fun.id)
      | Expr_Var v when self#is_replaced v ->
          let f = mk_read_fident v in
          ChangeTo (AST.Expr_TApply (f, [], [], NoThrow))
      | _ -> DoChildren

    (* Variables (and array indices) that are partially assigned in
     * the current statement, e.g., x[i +: 8] := e, and the temporary
     * variables that replace them.
     *)
    val mutable partial_writes : (Ident.t * AST.expr option * Ident.t) list = []

    method! vlexpr l =
      match l with
      | LExpr_Array (LExpr_Var v, i) when self#is_replaced v ->
          let i' = Isa_visitor.visit_expr (self :> Isa_visitor.isaVisitor) i in
          let tmp = tmps#fresh in
          partial_writes <- (v, Some i', tmp) :: partial_writes;
          ChangeTo (AST.LExpr_Var tmp)
      | LExpr_Var v when self#is_replaced v ->
          let tmp = tmps#fresh in
          partial_writes <- (v, None, tmp) :: partial_writes;
          ChangeTo (AST.LExpr_Var tmp)
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_Assign (LExpr_Array (LExpr_Var v, i), e, loc) when self#is_replaced v ->
          let f = mk_write_fident v in
          let s' = AST.Stmt_TCall (f, [], [ i; e ], NoThrow, loc) in
          ChangeDoChildrenPost ([ s' ], Fun.id)
      | Stmt_Assign (LExpr_Var v, e, loc) when self#is_replaced v ->
          let f = mk_write_fident v in
          let s' = AST.Stmt_TCall (f, [], [ e ], NoThrow, loc) in
          ChangeDoChildrenPost ([ s' ], Fun.id)
      | Stmt_Assign (_, _, loc) ->
          let post_action (ss : AST.stmt list) : AST.stmt list =
            let wrap (ss : AST.stmt list) (v, oi, tmp) =
              let idx = Option.to_list oi in
              let ty = Bindings.find v var_types in
              let rd = AST.Expr_TApply (mk_read_fident v, [], idx, NoThrow) in
              let r = AST.Stmt_VarDecl (false, AST.DeclItem_Var (tmp, Some ty), rd, loc) in
              let w = AST.Stmt_TCall (mk_write_fident v, [], idx @ [ Expr_Var tmp ], NoThrow, loc) in
              [ r ] @ ss @ [ w ]
            in
            let ss' = List.fold_left wrap ss partial_writes in
            partial_writes <- [];
            ss'
          in
          ChangeDoChildrenPost ([ s ], post_action)
      | _ -> DoChildren
  end

(****************************************************************
 * End
 ****************************************************************)
