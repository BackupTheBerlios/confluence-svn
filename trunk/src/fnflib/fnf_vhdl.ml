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
  write ("  " ^ id_of_cell cell ^ " <= " ^ expr ^ ";")
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
      | Input  (name, w) ->
          if w = 1 then
            write ("  " ^ id ^ " <= \"1\" when " ^ name ^ " = '1' else \"0\";")
          else
            assign cell ("unsigned(" ^ name ^ ")")

      | Output (name, w) ->
          if w = 1 then
            write ("  " ^ name ^ " <= '1' when " ^ input 0 ^ " = \"0\" else '0';")
          else
            write ("  " ^ name ^ " <= std_logic_vector(" ^ input 0 ^ ");")

      | Name   (name, w) -> ()
      | Dangle           -> ()
      | Const  value     -> assign cell ("\"" ^ value ^ "\"")
      | Buf    w         -> assign cell (input 0)
      | Not    w         -> assign cell ("not " ^ input 0)
      | And    w         -> assign cell (input 0 ^ " and " ^ input 1)
      | Xor    w         -> assign cell (input 0 ^ " xor " ^ input 1)
      | Or     w         -> assign cell (input 0 ^ " or " ^ input 1)
      | Concat (wl, wr)  -> assign cell (input 0 ^ " & " ^ input 1)
      | Select (w, bits) -> assign cell (String2.join (List.map (fun bit -> input 0 ^ "(" ^ string_of_int bit ^ " downto " ^ string_of_int bit ^ ")") bits) " & ")
      | Eq     w         -> assign cell ("\"1\" when " ^ input 0 ^ " = " ^ input 1 ^ " else \"0\"")
      | Lt     w         -> assign cell ("\"1\" when " ^ input 0 ^ " < " ^ input 1 ^ " else \"0\"")
      | Add    w         -> assign cell (input 0 ^ " + " ^ input 1)
      | Sub    w         -> assign cell (input 0 ^ " - " ^ input 1)
      | Mul    w         -> assign cell ("resize(" ^ input 0 ^ " * " ^ input 1 ^ ", " ^ string_of_int w ^ ")")
      | Mux    w         -> assign cell (input 2 ^ " when " ^ input 0 ^ " = \"1\" else " ^ input 1)
      | Ff     w         ->
                            write ("  " ^ id ^ "_clk <= '1' when " ^ input 0 ^ " = \"1\" else '0';");
                            write ("  process (" ^ id ^ "_clk)");
                            write ("  begin");
                            write ("    if rising_edge(" ^ id ^ "_clk) then");
                            write ("      " ^ id ^ " <= " ^ input 1 ^ ";");
                            write ("    end if;");
                            write ("  end process;")
      | Ffc    w         ->
                            write ("  " ^ id ^ "_clr <= '1' when " ^ input 0 ^ " = \"1\" else '0';");
                            write ("  " ^ id ^ "_clk <= '1' when " ^ input 1 ^ " = \"1\" else '0';");
                            write ("  process (" ^ id ^ "_clr, " ^ id ^ "_clk)");
                            write ("  begin");
                            write ("    if rising_edge(" ^ id ^ "_clr) then");
                            write ("      " ^ id ^ " <= \"" ^ String.make w '0' ^ "\";");
                            write ("    elseif rising_edge(" ^ id ^ "_clk) then");
                            write ("      " ^ id ^ " <= " ^ input 2 ^ ";");
                            write ("    end if;");
                            write ("  end process;")
      )
;;

let rec find_io sofar scope_item =
  match scope_item with
  | Scope scope -> List.fold_left find_io sofar (items_of_scope scope)
  | Cell  cell  ->
    (match info_of_cell cell with 
    | Input  _
    | Output _ -> cell :: sofar
    | _ -> sofar
    )
;;

let output_io_declaration cell term =
  match info_of_cell cell with
  | Input (name, w)  ->
      if w = 1 then
        write ("  signal " ^ name ^ " : in std_logic" ^ term)
      else
        write ("  signal " ^ name ^ " : in std_logic_vector(" ^ string_of_int (w - 1) ^ " downto 0)" ^ term)
  | Output (name, w) ->
      if w = 1 then
        write ("  signal " ^ name ^ " : out std_logic" ^ term)
      else
        write ("  signal " ^ name ^ " : out std_logic_vector(" ^ string_of_int (w - 1) ^ " downto 0)" ^ term)
  | _ -> raise (Invalid_argument "encountered a non-port cell")
;;

let rec output_io_declarations cells =
  match cells with
  | [] -> ()
  | [cell] -> output_io_declaration cell ""
  | cell :: cells -> output_io_declaration cell ";"; output_io_declarations cells
;;

let rec output_local_declarations scope_item =
  match scope_item with
  | Scope scope -> List.iter output_local_declarations (items_of_scope scope);
  | Cell  cell  ->
      (match info_of_cell cell with
      | Output _
      | Name   _
      | Dangle -> ()
      | Ff _ ->
          write ("  signal " ^ id_of_cell cell ^ "_clk : std_logic;");
          write ("  signal " ^ id_of_cell cell ^ " : unsigned(" ^ string_of_int (width_of_cell cell - 1) ^ " downto 0);") 
      | Ffc _ ->
          write ("  signal " ^ id_of_cell cell ^ "_clr : std_logic;");
          write ("  signal " ^ id_of_cell cell ^ "_clk : std_logic;");
          write ("  signal " ^ id_of_cell cell ^ " : unsigned(" ^ string_of_int (width_of_cell cell - 1) ^ " downto 0);") 
      | _ ->
          write ("  signal " ^ id_of_cell cell ^ " : unsigned(" ^ string_of_int (width_of_cell cell - 1) ^ " downto 0);") 
      )
;;

let output_vhdl out_channel scope =
  channel := out_channel;
  let scope_item = Scope (root_of_scope scope) in
  let ports = find_io [] scope_item in
  write ("library ieee;");
  write ("use ieee.std_logic_1164.all;");
  write ("use ieee.numeric_std.all;");
  write ("entity " ^ module_name_of_scope scope ^ " is");
  write ("port(");
  output_io_declarations ports;
  write (");");
  write ("end entity " ^ module_name_of_scope scope ^ ";");
  write ("architecture rtl of " ^ module_name_of_scope scope ^ " is");
  output_local_declarations scope_item;
  write ("begin");
  output_scope_item scope_item;
  write ("end architecture rtl;")
;;

