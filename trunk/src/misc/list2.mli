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


(* Common list functions. *)

val mapi        : (int -> 'a -> 'b) -> 'a list -> 'b list;;
val iteri       : (int -> 'a -> unit) -> 'a list -> unit;;
val index       : 'a -> 'a list -> int -> int;;
val indexq      : 'a -> 'a list -> int -> int;;
val indexf      : ('a -> bool) -> 'a list -> int -> int;;
val range       : int -> int -> int list;;
val take        : 'a list -> int -> 'a list;;
val drop        : 'a list -> int -> 'a list;;
val make        : int -> 'a -> 'a list;;


