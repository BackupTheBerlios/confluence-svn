(*
    FNF: Free Netlist Format
    Copyright (C) 2004-2005 Tom Hawkins (tomahawkins@yahoo.com)

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

(* JHDL generator adapted from VHDL generator by Nathan Cain *)

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

(*******************************************************)

(**** Note: May not correctly support tristate values in const! ****)


let output_cellinterface cell term =
  match info_of_cell cell with
  | Input (name, w)  ->
  	write ("    in(\"" ^ name ^ "\", " ^ string_of_int(w) ^ ")" ^ term)
  | Output (name, w) ->
  	write ("    out(\"" ^ name ^ "\", " ^ string_of_int(w) ^ ")" ^ term)
  | _ -> raise (Invalid_argument "encountered a non-port cell")
;;

let rec output_cellinterfaces cells =
  match cells with
  | [] -> ()
  | [cell] -> output_cellinterface cell ""
  | cell :: cells -> output_cellinterface cell ","; output_cellinterfaces cells
;;


let output_parameter cell term =
  match info_of_cell cell with
  | Input (name, w)
  | Output (name, w) ->
  	write ("      Wire " ^ name ^ term)
  | _ -> raise (Invalid_argument "encountered a non-port cell")
;;


let rec output_parameters cells =
  match cells with
  | [] -> ()
  | [cell] -> output_parameter cell ""
  | cell :: cells -> output_parameter cell ","; output_parameters cells
;;

let output_connect cell =
  match info_of_cell cell with
  | Input (name, w)
  | Output (name, w) ->
        write ("    connect(\"" ^ name ^ "\", " ^ name ^ ");")
  | _ -> raise (Invalid_argument "encountered a non-port cell")
;;
  

let rec output_connects cells =
  match cells with
  | [] -> ()
  | [cell] -> output_connect cell
  | cell :: cells -> output_connect cell; output_connects cells
;;

let rec output_localwires scope_item =
  match scope_item with
  | Scope scope -> List.iter output_localwires (items_of_scope scope);
  | Cell  cell  ->
      (match info_of_cell cell with
      | Output _
      | Name   _
      | Dangle -> ()
      | _ ->
          write ("    Wire " ^ id_of_cell cell ^ " = wire(" ^ string_of_int (width_of_cell cell) ^ ", \"" ^ id_of_cell cell ^ "\");") 
      )
;;

let assign cell expr =
  write ("  " ^ id_of_cell cell ^ " <= " ^ expr ^ ";")
;;


let rec output_logic scope_item =
  match scope_item with
  | Scope scope -> List.iter output_logic (items_of_scope scope);
  | Cell  cell  ->
      let id = id_of_cell cell in
      let input num =
        let cell = producer_of_port (port_of_cell cell num) in
        (match info_of_cell cell with Dangle -> raise (Invalid_argument "encountered unconnected cell") | _ -> ());
        id_of_cell cell
      in
      (match info_of_cell cell with
      | Input  (name, w) ->
	  write ("    //Input: " ^ id);
	  write ("      buf_o(" ^ name ^ ", " ^ id ^ ", \"Input_" ^ id ^ "\");")
	  

      | Output (name, w) ->
	  write ("    //Output: " ^ id);
	  write ("      buf_o(" ^ input 0 ^ ", " ^ name ^ ", \"Output_" ^ id ^ "\");")

      | Name   (name, w) ->
	  write ("    //Name: " ^ id)
	  
      | Dangle           ->
	  write ("    //Dangle: " ^ id) 
	  
      | Const  value     ->
	  write ("    //Const: " ^ id);
	  write ("      constant_o(" ^ id ^ ", new BV(\"0b" ^ value ^ "\"), \"Const_" ^ id ^ "\");")
	  
      | Buf    w         ->
	  write ("    //Buf: " ^ id);
	  write ("      buf_o(" ^ input 0 ^ ", " ^ id ^ ", \"Buf_" ^ id ^ "\");")
	  
      | Not    w         ->
	  write ("    //Not: " ^ id);
	  write ("      not_o(" ^ input 0 ^ ", " ^ id ^ ", \"Not_" ^ id ^ "\");")
	  
      | And    w         -> 
	  write ("    //And: " ^ id);
	  write ("      and_o(" ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"And_" ^ id ^ "\");")
	  
      | Xor    w         ->
	  write ("    //Xor: " ^ id);
	  write ("      xor_o(" ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"Xor_" ^ id ^ "\");")

	  
      | Or     w         ->
	  write ("    //Or: " ^ id);
	  write ("      or_o(" ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"Or_" ^ id ^ "\");")

	  
      | Concat (wl, wr)  ->
	  write ("    //Concat: " ^ id);
	  write ("      concat_o(" ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"Concat_" ^ id ^ "\");")
	  
      | Select (w, bit)  ->
	  write ("    //Select: " ^ id);
	  write ("      WireList " ^ id ^ "_temp = new WireList();");
	  write ("      " ^ id ^ "_temp.append( " ^ input 0 ^ ".gw(" ^ string_of_int bit ^ "));");
	  write ("      concat_o(" ^ id ^ "_temp, " ^ id ^ ", \"Select_" ^ id ^ "\");")
		
	  
      | Eq     w         ->
	  write ("    //Eq: " ^ id);
	  write ("      new eq(this, " ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"Eq_" ^ id ^ "\");")
	  
      | Lt     w         -> 
	  write ("    //Lt: " ^ id);
	  write ("      new lt(this, " ^ input 0 ^ ", " ^ input 1 ^ ", false, " ^ id ^ ", \"Lt_" ^ id ^ "\");")

	  
      | Add    w         ->
	  write ("    //Add: " ^ id);
	  write ("      add_o(" ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"Add_" ^ id ^ "\");")
	  
	  
      | Sub    w         ->
	  write ("    //Sub: " ^ id);
	  write ("      sub_o(" ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"Sub_" ^ id ^ "\");")
	  
      | Mul    w         ->
	  write ("    //Mul: " ^ id);
	  write ("      new arrayMult(this, " ^ input 0 ^ ", " ^ input 1 ^ ", null, " ^ id ^ ", false, 0, \"Mul_" ^ id ^ "\");")
	  
      | Mux    w         ->
	  write ("    //Mux: " ^ id);
	  write ("      mux_o(" ^ input 1 ^ ", " ^ input 2 ^ ", " ^ input 0 ^ ", " ^ id ^ ", \"Mux_" ^ id ^ "\");")
	  
      | Ff     w         ->
	  write ("    //Ff: " ^ id);
	  write ("      regc_o(" ^ input 0 ^ ", " ^ input 1 ^ ", " ^ id ^ ", \"Ff_" ^ id ^ "\");")
	  
      | Ffc    w         ->
	  write ("    //Ffc: " ^ id);
	  write ("      regr_o(" ^ input 1 ^ ", " ^ input 2 ^ ", " ^ input 0 ^ ", " ^ id ^ ", \"Ffc_" ^ id ^ "\");")

      )
;;


let output_jhdl file out_channel scope =
  channel := out_channel;
  let scope_item = Scope (root_of_scope scope) in
  let ports = find_io [] scope_item in
  
  write ("import byucc.jhdl.base.*;");
  write ("import byucc.jhdl.Logic.*;");
  write ("import byucc.jhdl.Logic.Modules.*;");
  write (" ");
  write ("public class " ^ file ^ " extends Logic");
  write ("{");
  write ("  ");
  write ("  public static CellInterface[] cell_interface = {");
  output_cellinterfaces ports;
  write ("  };");
  write ("");

  (* Note: generation of the constructor's parameter list requires at least
  		1 port... can/will there be an fnf with no ports? Only if there's
		no system instantiated in the cf.... but what good is that?*)
  write ("  public " ^ file);
  write ("    (");
  write ("      Node parent,");
  output_parameters ports;
  write ("    )");
  write ("  {");
  write ("    ");
  write ("    super(parent);");
  write ("    ");
  output_connects ports;
  write ("    ");
  output_localwires scope_item;
  write ("    ");
  write ("    ");
  output_logic scope_item;
  write ("    ");
  write ("  }");
  write ("}");

;;

  
