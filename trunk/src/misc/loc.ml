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


type loc = Unify    of string * int * int        (* file, line, col *)
         | Argument of string * int * int * int  (* file, line, col, argument *)
         | Unknown  of string;;

let create file line col =
  Unify (file, line, col);;

let unknown msg =
  Unknown msg;;

let argument loc arg =
  match loc with
    Unify (file, line, col) -> Argument (file, line, col, arg)
  | Argument (file, line, col, _) -> Argument (file, line, col, arg)
  | Unknown msg -> Unknown msg;;

let toString loc =
  match loc with
    Unify (file, line, col) -> file  ^ "  " ^ string_of_int line ^ "," ^ string_of_int col
  | Argument (file, line, col, arg) -> file  ^ "  " ^ string_of_int line ^ "," ^ string_of_int col ^ "  arg " ^ string_of_int arg
  | Unknown msg -> "Location Unknown: " ^ msg;;

let isKnown loc =
  match loc with
    Unknown _ -> false
  | _ -> true;;

