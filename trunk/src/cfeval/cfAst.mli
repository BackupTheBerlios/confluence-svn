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


exception CfAstError;;

type expr     = Apply       of Loc.loc * expr * expr list  (* location, comp, arg list. *)
              | Connect     of Loc.loc * expr * expr             (* location, sink expr, source expr. *)
              | Cond        of Loc.loc * expr * expr * expr                    (* predicate, true, false *)
              | Name        of Loc.loc * string
              | Comp        of Loc.loc * string list * stmt list         (* ports, statements *)
              | Prim        of Loc.loc * string * string list            (* primitive name, ports *)
              | DotName     of Loc.loc * expr * string
              | DotPosition of Loc.loc * expr * Intbig.intbig
              | Integer     of Loc.loc * Intbig.intbig
              | Float       of Loc.loc * float
              | Boolean     of Loc.loc * bool
              | Vector      of Loc.loc * string
              | Record      of Loc.loc * (string * expr) list
              | Free        of Loc.loc

and  stmt     = ApplyStmt   of expr
              | ConnectStmt of expr
;;

val expr_loc       : expr    -> Loc.loc;;
val stmt_loc       : stmt    -> Loc.loc;;
val add_sub_env    : expr -> expr -> string -> expr;;
val write_apply    : expr -> string -> out_channel -> unit;;

