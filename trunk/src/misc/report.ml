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

let error_limit = ref (-1);;

let set_error_limit limit =
  error_limit := limit
;;

let errors_encountered = ref 0;;

let errorReported () =
  !errors_encountered > 0
;;

let fatal msg =
  prerr_string ("\n" ^ msg ^ "\n");
  prerr_newline ();
  exit(1)
;;

let error msg =
  incr errors_encountered;
  if !error_limit < 0 || !errors_encountered <= !error_limit then begin
    prerr_string ("\n" ^ msg ^ "\n");
    prerr_newline ()
  end
;;

let warning msg =
  print_string ("\n" ^ msg ^ "\n");
  print_newline ()
;;

let debug msg =
  print_string ("** Debug: " ^ msg);
  print_newline ()
;;


let hashSize h =
  string_of_int (Hashtbl.fold (fun a b c -> c + 1) h 0);;

