(*
    Confluence System Design Language Compiler
    Copyright (C) 2003-2004 Tom Hawkins (tomahawkins@yahoo.com)

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


(* Intbig: arbitrary-precision integers. *)

type intbig;;


(* Constants. *)

val zero : intbig;;
val one  : intbig;;
val two  : intbig;;
val ones : intbig;;  (* All ones.  Same as -1. *)


(* Arithmetics. *)

val abs  : intbig -> intbig;;
val neg  : intbig -> intbig;;
val add  : intbig -> intbig -> intbig;;
val sub  : intbig -> intbig -> intbig;;
val mul  : intbig -> intbig -> intbig;;
val div  : intbig -> intbig -> intbig;;
val modu : intbig -> intbig -> intbig;;
val pow  : intbig -> intbig -> intbig;;


(* Comparisons. *)

val sign    : intbig -> int;;
val compare : intbig -> intbig -> int;;
val eq      : intbig -> intbig -> bool;;
val ne      : intbig -> intbig -> bool;;
val le      : intbig -> intbig -> bool;;
val ge      : intbig -> intbig -> bool;;
val lt      : intbig -> intbig -> bool;;
val gt      : intbig -> intbig -> bool;;


(* Shifting. *)

val shift_left  : intbig -> intbig -> intbig;;
val shift_right : intbig -> intbig -> intbig;;


(* Bitwise logics. *)

val bw_not : intbig -> intbig;;
val bw_and : intbig -> intbig -> intbig;;
val bw_or  : intbig -> intbig -> intbig;;
val bw_xor : intbig -> intbig -> intbig;;


(* String conversion. *)

val string_of_intbig        : intbig -> string;;
val intbig_of_string        : string -> intbig;;
val binary_string_of_intbig : intbig -> intbig -> string;;  (* Value -> Width -> BinaryString *)
val intbig_of_binary_string : string -> intbig;;


(* Numeric type conversion. *)

val int_of_intbig    : intbig -> int;;
val intbig_of_int    : int -> intbig;;

val char_of_intbig   : intbig -> char;;
val intbig_of_char   : char -> intbig;;

val float_of_intbig  : intbig -> float;;
val intbig_of_float  : float -> intbig;;


(* Unit tests. *)

val test : string -> unit;;



