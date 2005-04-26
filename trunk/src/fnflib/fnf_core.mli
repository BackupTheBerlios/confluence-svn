(*
    FNF: Free Netlist Format
    Copyright (C) 2004-2005 Tom Hawkins (tomahawkins@yahoo.com)

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

(** Free Netlist Format (FNF) Core Functions *)

(** {2 Netlist Exceptions} *)

exception Error of string;;

(** {2 Netlist Types} *)

type scope;;
type cell;;
type port;;

type cell_info = Input  of string * int
               | Output of string * int
               | Name   of string * int
               | Dangle
               | Const  of string
               | Buf    of int
               | Not    of int
               | And    of int
               | Xor    of int
               | Or     of int
               | Concat of int * int
               | Select of int * int
               | Eq     of int
               | Lt     of int
               | Add    of int
               | Sub    of int
               | Mul    of int
               | Mux    of int
               | Ff     of int
               | Ffc    of int
               | Bbox   of string * int * int * int list
;;

type item = Scope of scope | Cell of cell;;

(** {2 Scope Manipulation} *)

val create_root_scope : string -> scope;;
val create_sub_scope  : scope -> string -> string -> scope;;  (* parent, module, instance *)
val parent_of_scope   : scope -> scope;;
val root_of_scope     : scope -> scope;;

val items_of_scope     : scope -> item list;;
val all_cells         : scope -> cell list;;

val module_name_of_scope   : scope -> string;;
val instance_name_of_scope : scope -> string;;

val scope_of_cell : cell -> scope;;
val path_of_scope : scope -> string list;;
val path_of_cell  : cell  -> string list;;

(** {2 Cell Manipulation} *)

val info_of_cell      : cell -> cell_info;;
val name_of_cell      : cell -> string;;

val width_of_cell     : cell -> int;;
val width_of_port     : port -> int;;
val arity_of_cell     : cell -> int;;
val is_port_dangling  : port -> bool;;
val is_cell_producer  : cell -> bool;;  (* Not output or name. *)

val id_of_cell        : cell -> int;;
val cell_of_port      : port -> cell;;
val ports_of_cell     : cell -> port list;;
val port_of_cell      : cell -> int -> port;;

val connect           : cell -> port -> unit;;  (* Check that source cell is not an output or a name.  Raise exception if not connected to dangle. *)
val reconnect         : cell -> port -> unit;;  (* Check that source cell is not an output or a name. *)
val consumers_of_cell : cell -> port list;;
val producer_of_port  : port -> cell;;

(*
val delete_cell       : cell -> unit;;
val move_cell         : cell -> scope;;   (* Move cell to other scope. *)
*)

(** {2 Cell Creation} *)

val dangle        : scope -> cell;;
val create_input  : scope -> string -> int -> cell;;
val create_output : scope -> string -> int -> cell * port;;
val create_name   : scope -> string -> int -> cell * port;;
val create_const  : scope -> string -> cell;;
val create_buf    : scope -> int -> cell * port;;
val create_not    : scope -> int -> cell * port;;
val create_and    : scope -> int -> cell * port * port;;
val create_xor    : scope -> int -> cell * port * port;;
val create_or     : scope -> int -> cell * port * port;;
val create_concat : scope -> int -> int -> cell * port * port;;
val create_select : scope -> int -> int -> cell * port;;
val create_eq     : scope -> int -> cell * port * port;;
val create_lt     : scope -> int -> cell * port * port;;
val create_add    : scope -> int -> cell * port * port;;
val create_sub    : scope -> int -> cell * port * port;;
val create_mul    : scope -> int -> cell * port * port;;
val create_mux    : scope -> int -> cell * port * port * port;;
val create_ff     : scope -> int -> cell * port * port;;
val create_ffc    : scope -> int -> cell * port * port * port;;
val create_bbox   : scope -> string -> int -> int -> int list -> cell * port;;

