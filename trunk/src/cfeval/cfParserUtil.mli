(*
    Confluence System Design Language Compiler
    Copyright (C) 2003-2005 Tom Hawkins (tomahawkins@yahoo.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
*)

(** Parsing Transformations and Management *)

(** Setting the base environment. *)
val set_base_env : string -> unit;;

(** Main parsing. *)
val parse_program           : string -> 'a -> ('a -> Lexing.lexbuf -> CfAst.expr) -> CfAst.expr;;

(** Error Reporting *)
val error                   : string -> unit;;

(** Parser and Lexer Functions *)

val current_file_loc       : unit -> Loc.loc;;
val set_env                : string -> unit;;
val add_import             : string -> string;;
val next_lex               : Lexing.lexbuf -> (Loc.loc * string);;
val get_current_token      : unit -> string;;

(** Parser Transformations *)

val free_arg_list                  : Loc.loc -> string list -> CfAst.expr list;;
val application_expr_of_namespace  : Loc.loc -> string list -> CfAst.stmt list -> CfAst.expr;;
val application_of_namespace       : Loc.loc -> string list -> CfAst.stmt list -> CfAst.stmt;;   (* location, names, stmts *)
val select_position                : Loc.loc -> CfAst.expr -> int -> CfAst.expr;;
val connect_name                   : Loc.loc -> (Loc.loc * string) -> CfAst.expr -> CfAst.expr;;
val stmt_of_comp_named             : Loc.loc -> (Loc.loc * string) -> string list -> CfAst.stmt list -> CfAst.stmt;;
val application_of_prefix          : (Loc.loc * string) -> CfAst.expr -> CfAst.expr;;                              (* name, expr *)
val application_of_infix           : (Loc.loc * string) -> CfAst.expr -> CfAst.expr -> CfAst.expr;;                (* name, expr, expr *)
val application_of_trifix          : (Loc.loc * string) -> CfAst.expr -> CfAst.expr -> CfAst.expr -> CfAst.expr;;  (* name, expr, expr, expr *)
val conditional_of_ifelse          : Loc.loc -> CfAst.expr -> CfAst.stmt list -> CfAst.stmt list -> CfAst.stmt;;
val format_string                  : string -> string;;
val application_of_string          : Loc.loc -> string -> CfAst.expr;;

