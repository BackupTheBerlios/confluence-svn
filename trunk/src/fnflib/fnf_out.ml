(*
    FNF: Free Netlist Format
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

open Fnf_core;;

let channel = ref stdout;;

let is_compact = ref false;;

let level = ref 0;;

let write line =
  if not !is_compact then output_string !channel (String.make (!level * 2) ' ');
  output_string !channel line;
  output_char !channel '\n'
;;

let format_string string =
  let len = String.length string in
  let rec r i sofar =
    if i >= len then
      sofar
    else
      r (i + 1) (sofar ^ (match string.[i] with '\\' -> "\\\\" | '"' -> "\\\"" | c -> String.make 1 c))
  in
  "\"" ^ r 0 "" ^ "\""
;;

let rec output_scope_item scope_item =
  match scope_item with
  | Scope scope ->
      write ("(scope " ^ format_string (module_name_of_scope scope) ^ " " ^ format_string (instance_name_of_scope scope) ^ " (");
      incr level;
      List.iter output_scope_item (items_of_scope scope);
      decr level;
      write ("))")
  | Cell cell ->
      let id_of_cell cell = string_of_int (id_of_cell cell) in
      let id = id_of_cell cell ^ " " in
      let rec inputs i =
        if i >= arity_of_cell cell then
          ""
        else
          " " ^ id_of_cell (producer_of_port (port_of_cell cell i)) ^ inputs (i + 1)
      in
      let inputs = inputs 0 in
      (match info_of_cell cell with
      | Input  (name, w) -> write ("(input  " ^ id ^ format_string name ^ " " ^ string_of_int w ^ ")")
      | Output (name, w) -> write ("(output " ^ id ^ format_string name ^ " " ^ string_of_int w ^ inputs ^ ")")
      | Name   (name, w) -> write ("(name   " ^ id ^ format_string name ^ " " ^ string_of_int w ^ inputs ^ ")")
      | Dangle           -> write ("(dangle " ^ id_of_cell cell ^ ")")
      | Const  value     -> write ("(const  " ^ id ^ format_string value ^ ")")
      | Buf    w         -> write ("(buf    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Not    w         -> write ("(not    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | And    w         -> write ("(and    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Xor    w         -> write ("(xor    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Or     w         -> write ("(or     " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Concat (wl, wr)  -> write ("(concat " ^ id ^ string_of_int wl ^ " " ^ string_of_int wr ^ inputs ^ ")")
      | Select (w, bit)  -> write ("(select " ^ id ^ string_of_int w ^ " " ^ string_of_int bit ^ " " ^ inputs ^ ")")
      | Eq     w         -> write ("(eq     " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Lt     w         -> write ("(lt     " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Add    w         -> write ("(add    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Sub    w         -> write ("(sub    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Mul    w         -> write ("(mul    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Mux    w         -> write ("(mux    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Ff     w         -> write ("(ff     " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Ffc    w         -> write ("(ffc    " ^ id ^ string_of_int w ^ inputs ^ ")")
      | Bbox (n, wo, wi, p) -> write ("(bbox   " ^ id ^ format_string n ^ " " ^ string_of_int wo ^ " " ^ string_of_int wi ^ " (" ^ String2.join (List.map string_of_int p) " " ^ ")" ^ inputs ^ ")")
      )
;;

let output_fnf out_channel scope =
  channel := out_channel;
  is_compact := false;
  level := 0;
  output_scope_item (Scope (root_of_scope scope))
;;

let output_fnf_compact out_channel scope =
  channel := out_channel;
  is_compact := true;
  level := 0;
  output_scope_item (Scope (root_of_scope scope))
;;

