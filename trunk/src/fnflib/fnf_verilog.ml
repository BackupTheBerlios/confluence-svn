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

let write line =
  output_string !channel line;
  output_char !channel '\n'
;;

let id_of_cell cell =
  "n" ^ string_of_int (id_of_cell cell)
;;

let debug msg =
  print_string msg;
  print_newline ()
;;

let assign cell expr =
  write ("  assign " ^ id_of_cell cell ^ " = " ^ expr ^ ";")
;;

let rec output_scope_item scope_item =
  match scope_item with
  | Scope scope -> List.iter output_scope_item (items_of_scope scope);
  | Cell  cell  ->
      let id = id_of_cell cell in
      let input num =
        let cell = producer_of_port (port_of_cell cell num) in
        (match info_of_cell cell with Dangle -> raise (Invalid_argument "encountered unconnected cell") | _ -> ());
        id_of_cell cell
      in
      (match info_of_cell cell with
      | Input  (name, w) -> assign cell name
      | Output (name, w) -> write ("  assign " ^ name ^ " = " ^ input 0 ^ ";")
      | Name   (name, w) -> ()
      | Dangle           -> ()
      | Const  value     -> assign cell (string_of_int (String.length value) ^ "'b" ^ value)
      | Buf    w         -> assign cell (input 0)
      | Not    w         -> assign cell ("~ " ^ input 0)
      | And    w         -> assign cell (input 0 ^ " & " ^ input 1)
      | Xor    w         -> assign cell (input 0 ^ " ^ " ^ input 1)
      | Or     w         -> assign cell (input 0 ^ " | " ^ input 1)
      | Concat (wl, wr)  -> assign cell ("{" ^ input 0 ^ ", " ^ input 1 ^ "}")
      | Select (w, bit)  -> if w = 1 then assign cell (input 0) else assign cell (input 0 ^ "[" ^ string_of_int bit ^ "]")
      | Eq     w         -> assign cell (input 0 ^ " == " ^ input 1)
      | Lt     w         -> assign cell (input 0 ^ " < " ^ input 1)
      | Add    w         -> assign cell (input 0 ^ " + " ^ input 1)
      | Sub    w         -> assign cell (input 0 ^ " - " ^ input 1)
      | Mul    w         -> assign cell (input 0 ^ " * " ^ input 1)
      | Mux    w         -> assign cell (input 0 ^ " ? " ^ input 2 ^ " : " ^ input 1)
      | Ff     w         -> write ("  always @ (posedge " ^ input 0 ^ ") " ^ id ^ " <= " ^ input 1 ^ ";")
      | Ffc    w         -> write ("  always @ (posedge " ^ input 0 ^ " or posedge " ^ input 1 ^ ") if (" ^ input 0 ^ ") " ^ id ^ " <= {" ^ string_of_int w ^ "{1'b0}}; else " ^ id ^ " <= " ^ input 2 ^ ";")
      | Bbox (n, wo, wi, p) -> write ("  " ^ n ^ " #(" ^ String2.join (List.map string_of_int (wo :: wi :: p)) ", " ^ ") " ^ id ^ "_i (" ^ id ^ ", " ^ input 0 ^ ");")
      )
;;

let rec find_io sofar scope_item =
  match scope_item with
  | Scope scope -> List.fold_left find_io sofar (items_of_scope scope)
  | Cell  cell  ->
    (match info_of_cell cell with 
    | Input  (name, _)
    | Output (name, _) -> name :: sofar
    | _ -> sofar
    )
;;

let rec output_io_declarations scope_item =
  match scope_item with
  | Scope scope -> List.iter output_io_declarations (items_of_scope scope);
  | Cell  cell  ->
      (match info_of_cell cell with
      | Input  (name, w) -> write ("  input  " ^ (if w = 1 then "" else "[" ^ string_of_int (w - 1) ^ ":0] ") ^ name ^ ";")
      | Output (name, w) -> write ("  output " ^ (if w = 1 then "" else "[" ^ string_of_int (w - 1) ^ ":0] ") ^ name ^ ";")
      | _ -> ()
      )
;;

let rec output_local_declarations scope_item =
  match scope_item with
  | Scope scope -> List.iter output_local_declarations (items_of_scope scope);
  | Cell  cell  ->
      (match info_of_cell cell with
      | Output _
      | Name   _
      | Dangle -> ()
      | Ff  w
      | Ffc w  -> write ("  reg    [" ^ string_of_int (w - 1) ^ ":0] " ^ id_of_cell cell ^ ";") 
      | _      -> write ("  wire   [" ^ string_of_int (width_of_cell cell - 1) ^ ":0] " ^ id_of_cell cell ^ ";")
      )
;;

let output_verilog out_channel scope =
  channel := out_channel;
  let scope_item = Scope (root_of_scope scope) in
  write ("module " ^ module_name_of_scope scope ^ "(" ^ String2.join (find_io [] scope_item) ", " ^ ");");
  output_io_declarations scope_item;
  output_local_declarations scope_item;
  output_scope_item scope_item;
  write ("endmodule")
;;

