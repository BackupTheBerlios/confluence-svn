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

let debug msg =
  print_string msg;
  print_newline ()
;;

let channel = ref stdout;;

let level = ref 0;;

let write line =
  for i = 1 to !level do
    output_string !channel "  "
  done;
  output_string !channel line;
  output_char !channel '\n'
;;

let id_of_cell cell =
  "n" ^ string_of_int (id_of_cell cell)
;;

let iter count func =
  for i = 0 to count - 1 do
    func ("_" ^ string_of_int i)
  done
;;

let if_expr pred on_true on_false =
  "case " ^ pred ^ " : " ^ on_true ^ "; 1 : " ^ on_false ^ "; esac"
;;



(** NuSMV Data Representation

For 2-value model:

  <cell>_<bit>
  
For 4-value model:

  <cell>_x_<bit>
  <cell>_y_<bit>

  x y : val
  ---------
  0 0 : Z
  0 1 : X
  1 0 : 0
  1 1 : 1

  x = is_known
  y = if is_known then is_high else is_driven

*)

let rec output_scope_item scope_item =
  match scope_item with
  | Scope scope ->
      write ("--(scope " ^ module_name_of_scope scope ^ " " ^ instance_name_of_scope scope);
      incr level;
      List.iter output_scope_item (items_of_scope scope);
      decr level;
      write ("--)")

  | Cell  cell  ->
      let id = id_of_cell cell in
      let input num =
        let cell = producer_of_port (port_of_cell cell num) in
        (match info_of_cell cell with Dangle -> raise (Invalid_argument "encountered unconnected cell") | _ -> ());
        id_of_cell cell
      in
      (match info_of_cell cell with

      | Input  (name, w) -> 
          write ("-- input " ^ name);
          iter w (fun bit ->
            write ("IVAR " ^ id ^ bit ^ "_input : boolean;");
            write ("VAR  " ^ id ^ bit ^ " : boolean;");
            write ("ASSIGN init(" ^ id ^ bit ^ ") := 0;");
            write ("ASSIGN next(" ^ id ^ bit ^ ") := " ^ id ^ bit ^ "_input;");
          )

      | Output (name, w) ->
          write ("-- output " ^ name ^ " " ^ input 0)

      | Name   (name, w) ->
          write ("-- name " ^ name ^ " " ^ input 0)

      | Dangle           -> ()

      | Const  value ->
          write ("-- const \"" ^ value ^ "\"");
          let len = String.length value in
          for i = 0 to len - 1 do
            let c = value.[len - 1 - i] in
            write ("DEFINE " ^ id ^ "_" ^ string_of_int i ^ " := " ^ (if c = '1' then "1" else if c = '0' then "0" else raise (Invalid_argument "invalid constant string")) ^ ";");
          done

      | Buf    w ->
          write ("-- buf");
          iter w (fun bit -> write ("DEFINE " ^ id ^ bit ^ " := " ^ input 0 ^ bit ^ ";"))

      | Not    w ->
          write ("-- not");
          iter w (fun bit -> write ("DEFINE " ^ id ^ bit ^ " := ! " ^ input 0 ^ bit ^ ";"))

      | And    w ->
          write ("-- and");
          iter w (fun bit -> write ("DEFINE " ^ id ^ bit ^ " := " ^ input 0 ^ bit ^ " & " ^ input 1 ^ bit ^ ";"))

      | Xor    w ->
          write ("-- xor");
          iter w (fun bit -> write ("DEFINE " ^ id ^ bit ^ " := " ^ input 0 ^ bit ^ " xor " ^ input 1 ^ bit ^ ";"))

      | Or     w ->
          write ("-- or");
          iter w (fun bit -> write ("DEFINE " ^ id ^ bit ^ " := " ^ input 0 ^ bit ^ " | " ^ input 1 ^ bit ^ ";"))

      | Concat (wl, wr) ->
          write ("-- concat");
          iter wr (fun bit -> write ("DEFINE " ^ id ^ bit ^ " := " ^ input 1 ^ bit ^ ";"));
          for i = wr to wr + wl - 1 do
            write ("DEFINE " ^ id ^ "_" ^ string_of_int i ^ " := " ^ input 0 ^ "_" ^ string_of_int (i - wr) ^ ";")
          done

      | Select (w, bit) ->
          write ("-- select");
          write ("DEFINE " ^ id ^ "_0 := " ^ input 0 ^ "_" ^ string_of_int bit ^ ";");

      | Eq     w ->
          let rec eq bit =
            if bit < 0 then [] else
              ("(" ^ input 0 ^ "_" ^ string_of_int bit ^ " <-> " ^ input 1 ^ "_" ^ string_of_int bit ^ ")") :: eq (bit - 1)
          in
          write ("-- eq " ^ id ^ " = " ^ input 0 ^ " == " ^ input 1);
          write ("DEFINE " ^ id ^ "_0 := " ^ String2.join (eq (w - 1)) " & " ^ ";")

      | Lt     w ->
          let rec lt bit =
            let expr = id ^ "_" ^ string_of_int bit ^ "_c" in
            if bit = 0 then
              write ("DEFINE " ^ expr ^ " := ! " ^ input 0 ^ "_0 & " ^ input 1 ^ "_0;")
            else begin
              let c = lt (bit - 1) in
              let bit = "_" ^ string_of_int bit in
              write ("DEFINE " ^ expr ^ " := (" ^ input 1 ^ bit ^ " & " ^ c ^ ") | (" ^ input 1 ^ bit ^ " & ! " ^ input 0 ^ bit ^ ") | (" ^ c ^ " & ! " ^ input 0 ^ bit ^ ");")
            end;
            expr
          in
          write ("-- lt " ^ id ^ " = " ^ input 0 ^ " < " ^ input 1);
          write ("DEFINE " ^ id ^ "_0 := " ^ lt (w - 1) ^ ";")

      | Add    w ->
          let rec add bit =
            let xout = id ^ "_" ^ string_of_int bit in
            let cout = id ^ "_" ^ string_of_int bit ^ "_c" in
            let temp = id ^ "_" ^ string_of_int bit ^ "_t" in
            let cin  = if bit = 0 then "0" else add (bit - 1) in
            let bit = "_" ^ string_of_int bit in
            write ("DEFINE " ^ temp ^   " := " ^ input 0 ^ bit ^ " xor " ^ input 1 ^ bit ^ ";");
            write ("DEFINE " ^ cout ^   " := (" ^ temp ^ " & " ^ cin ^ ") | (" ^ input 0 ^ bit ^ " & " ^ input 1 ^ bit ^ ");");
            write ("DEFINE " ^ xout ^ "   := " ^ temp ^ " xor " ^ cin ^ ";");
            cout
          in
          write ("-- add " ^ id ^ " = " ^ input 0 ^ " + " ^ input 1);
          let _ = add (w - 1) in
          ()

      | Sub    w ->
          let rec sub bit =
            let xout = id ^ "_" ^ string_of_int bit in
            let cout = id ^ "_" ^ string_of_int bit ^ "_c" in
            let temp = id ^ "_" ^ string_of_int bit ^ "_t" in
            let cin  = if bit = 0 then "0" else sub (bit - 1) in
            let bit = "_" ^ string_of_int bit in
            write ("DEFINE " ^ temp ^   " := ! " ^ input 0 ^ bit ^ ";");
            write ("DEFINE " ^ cout ^   " := (" ^ input 1 ^ bit ^ " & " ^ cin ^ ") | (" ^ temp ^ " & " ^ input 1 ^ bit ^ ") | (" ^ temp ^ " & " ^ cin ^ ");");
            write ("DEFINE " ^ xout ^ "   := " ^ input 0 ^ bit ^ " xor " ^ input 1 ^ bit ^ " xor " ^ cin ^ ";");
            cout
          in
          write ("-- sub " ^ id ^ " = " ^ input 0 ^ " - " ^ input 1);
          let _ = sub (w - 1) in
          ()

      | Mul    w -> raise (Invalid_argument "multiply not supported")

      | Mux    w ->
          write ("-- mux");
          (*
          write ("SPEC E [1 U " ^ input 0 ^ "_0] & E [1 U ! " ^ input 0 ^ "_0];");
          *)
          iter w (fun bit -> write ("DEFINE " ^ id ^ bit ^ " := " ^ if_expr (input 0 ^ "_0") (input 2 ^ bit) (input 1 ^ bit) ^ ";"))

      | Ff     w ->
          let clock = input 0 ^ "_0" in
          let clock_state = id ^ "_clock_state" in
          write ("-- ff");
          write ("VAR " ^ clock_state ^ " : boolean;");
          write ("ASSIGN init(" ^ clock_state ^ ") := 1;");
          write ("ASSIGN next(" ^ clock_state ^ ") := " ^ clock ^ ";");
          iter w (fun bit ->
            write ("VAR " ^ id ^ bit ^ " : boolean;");
            write ("ASSIGN init(" ^ id ^ bit ^ ") := 0;");
            write ("ASSIGN next(" ^ id ^ bit ^ ") := " ^ if_expr (clock ^ " & ! " ^ clock_state) (input 1 ^ bit) (id ^ bit) ^ ";");
          )

      | Ffc    w ->
          let clear = input 0 ^ "_0" in
          let clear_state = id ^ "_clear_state" in
          let clock = input 1 ^ "_0" in
          let clock_state = id ^ "_clock_state" in
          write ("-- ffc");
          write ("VAR " ^ clear_state ^ " : boolean;");
          write ("ASSIGN init(" ^ clear_state ^ ") := 1;");
          write ("ASSIGN next(" ^ clear_state ^ ") := " ^ clear ^ ";");
          write ("VAR " ^ clock_state ^ " : boolean;");
          write ("ASSIGN init(" ^ clock_state ^ ") := 1;");
          write ("ASSIGN next(" ^ clock_state ^ ") := " ^ clock ^ ";");
          iter w (fun bit ->
            write ("VAR " ^ id ^ bit ^ " : boolean;");
            write ("ASSIGN init(" ^ id ^ bit ^ ") := 0;");
            write ("ASSIGN next(" ^ id ^ bit ^ ") := " ^ if_expr (clear ^ " & ! " ^ clear_state) "0" (if_expr (clock ^ " & ! " ^ clock_state) (input 2 ^ bit) (id ^ bit)) ^ ";");
          )
      )
;;

let output_nusmv out_channel scope =
  channel := out_channel;
  level := 0;
  let scope_item = Scope (root_of_scope scope) in
  write ("MODULE main");
  output_scope_item scope_item;
;;

