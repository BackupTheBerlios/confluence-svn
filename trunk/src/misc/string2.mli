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


(* Common string functions. *)

val replace      : string -> char -> char -> string;;
val take_left    : string -> int -> string;;
val take_right   : string -> int -> string;;
val drop_left    : string -> int -> string;;
val drop_right   : string -> int -> string;;
val right_char   : string -> char;;
val left_char    : string -> char;;
val right_string : string -> string;;
val left_string  : string -> string;;
val cons_left    : char -> string -> string;;
val cons_right   : string -> char -> string;;
val replace_meta_chars : string -> string;;
val replace_xml_chars  : string -> string;;
val replace_printf_chars  : string -> string;;
val insert_meta_chars  : string -> (char * string) list -> string;;
val map          : (char -> char) -> string -> string;;
val map2         : (char -> char -> char) -> string -> string -> string;;
(*
val fold_left    : ('a -> char -> 'a) -> 'a -> string -> 'a;;
val fold_right   : (char -> 'a -> 'a) -> string -> 'a -> 'a;;
*)
val chars_of_string : string -> char list;;
val string_of_chars : char list -> string;;
    
val join                    : string list -> string -> string;;
val split                   : string -> string -> string list;;
val string_of_char          : char -> string;;
val replace_chars_in_string : (char -> string) -> string -> string;;
    
(* Unit tests. *)

val test : string -> unit;;




