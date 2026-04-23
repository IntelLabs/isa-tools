(****************************************************************
 * Locations in source code
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

type pos = Lexing.position

(** Location tracking *)
type t =
    | Unknown
    | Int of string * t option
    | Generated of t
    | Range of pos * pos

let rec to_string (l : t) : string =
    ( match l with
    | Unknown -> "no location information available"
    | Generated l -> Printf.sprintf "Generated: %s"  (to_string l)
    | Range(p1, p2) ->
        let f = if String.length p1.Lexing.pos_fname == 0
          then ""
          else Printf.sprintf "file %s " p1.Lexing.pos_fname
        in
        let p = if p1.Lexing.pos_lnum = p2.Lexing.pos_lnum
                then
                  Printf.sprintf "line %d char %d - %d"
                    p1.Lexing.pos_lnum
                    (p1.Lexing.pos_cnum - p1.Lexing.pos_bol)
                    (p2.Lexing.pos_cnum - p2.Lexing.pos_bol)
                else
                  Printf.sprintf "line %d char %d - line %d char %d"
                    p1.Lexing.pos_lnum
                    (p1.Lexing.pos_cnum - p1.Lexing.pos_bol)
                    p2.Lexing.pos_lnum
                    (p2.Lexing.pos_cnum - p2.Lexing.pos_bol)
        in
        f ^ p
    | Int(s,lo) -> Printf.sprintf "%s %s" s (match lo with Some l -> to_string l | None -> "none")
    )

let pp (fmt : Format.formatter) (x : t) : unit =
  Format.pp_print_string fmt (to_string x)

(****************************************************************
 * End
 ****************************************************************)
