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


(** Confluence to FNF Interface Functions *)


(** Types *)

type producer;;
type system;;


(** Synchronization *)

val width_of_producer  : producer -> int;;

val new_root_system   : unit -> system;;
val new_sub_system    : system -> Loc.loc list -> Loc.loc -> system;;

val set_clock_domain : system -> string   -> (string -> unit) -> unit;;
val add_sub_enable   : system -> producer -> (string -> unit) -> unit;;
val add_sub_reset    : system -> producer -> (string -> unit) -> unit;;



(** Combinatorial Primitives *)

val new_input       : system -> string -> int -> producer;;
val new_output      : system -> string -> producer -> unit;;
val new_const       : system -> string -> producer;;
val new_buf         : system -> producer -> producer;;
val new_not         : system -> producer -> producer;;
val new_and         : system -> producer -> producer -> producer;;
val new_or          : system -> producer -> producer -> producer;;
val new_xor         : system -> producer -> producer -> producer;;
val new_concat      : system -> producer -> producer -> producer;;
val new_select      : system -> producer -> int -> producer;;
val new_equ         : system -> producer -> producer -> producer;;
val new_lt          : system -> producer -> producer -> producer;;
val new_add         : system -> producer -> producer -> producer;;
val new_sub         : system -> producer -> producer -> producer;;
val new_mul         : system -> producer -> producer -> producer;;
val new_mux         : system -> producer -> producer -> producer -> producer;;


(** Stateful Primitives: First call newStateful, then call specific. *)

val new_state       : system -> int -> producer;;
val new_reg         : system -> producer -> producer -> unit;;


(** Code Generator *)

val output_fnf     : out_channel -> unit;;

