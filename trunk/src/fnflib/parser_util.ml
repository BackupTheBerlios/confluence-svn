(*
    InFormal Digital Logic Verification Environment
    Copyright (C) 2004 Tom Hawkins (tomahawkins@yahoo.com)

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

(** Generic parser utility functions. *)

(** Error exception. *)
exception Error of string;;

(** Stateful parser data keeps track of file locations. *)

let error_line      = ref 1;;
let error_col       = ref 0;;

let current_file    = ref "channel";;
let current_line    = ref 1;;
let current_col     = ref 0;;
let current_token   = ref "";;

let reset_state () =
  current_file    := "channel";
  current_line    := 1;
  current_col     := 0;
  current_token   := "";

;;


(** Parsing error reporting. *)
let error msg =
  let s = "\n** Syntax Error in " ^ !current_file ^ " at line " ^ string_of_int !error_line ^ "," ^ string_of_int !error_col ^ "\n\n  " ^ msg ^ "\n\n" in
  print_string s;
  print_newline ();
  exit 2
;;


(** Updates parser location. *)
let update_positions c =
  if c == '\n' then
    begin
      current_line := !current_line + 1;
      current_col  := 1
    end
  else
    current_col  := !current_col  + 1;;

(** Handles next token. *)
let next_lex lexbuf =
  error_line := !current_line;
  error_col  := !current_col;
  let token = Lexing.lexeme lexbuf in
  current_token := token;
  let position = (!current_file, !current_line, !current_col) in
  String.iter update_positions token;
  token, position;;

(** Gets the current token. *)
let get_current_token () =
  !current_token;;

(** Parses a channel and returns the AST. *)
let parse_channel channel channel_name token reduction =
  reset_state ();
  current_file := channel_name;
  try
    let lexbuf = Lexing.from_channel channel in
    reduction token lexbuf
  with Error msg -> error msg
;;

