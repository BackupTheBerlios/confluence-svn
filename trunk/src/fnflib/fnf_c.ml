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

open Fnf_core;;

let debug msg =
  print_string msg;
  print_newline ()
;;

let words_of_width bits =
  (bits - 1) / 32 + 1
;;

let cell_index_table = Hashtbl.create 1024;;
let next_index = ref 0;;

let add_cell_index cell index =
  Hashtbl.add cell_index_table (id_of_cell cell) index
;;

let index_of_cell cell =
  Hashtbl.find cell_index_table (id_of_cell cell)
;;

let reset () =
  Hashtbl.clear cell_index_table;
  next_index := 0
;;


let allocate_cell cell =
  let input num =
    let cell = producer_of_port (port_of_cell cell num) in
    (match info_of_cell cell with Dangle -> raise (Invalid_argument "encountered unconnected cell") | _ -> ());
    cell
  in
  match info_of_cell cell with
  | Input  (_, w)
  | Not    w
  | And    w
  | Xor    w
  | Or     w
  | Add    w
  | Sub    w
  | Mul    w
  | Mux    w ->
      add_cell_index cell !next_index;
      next_index := !next_index + (words_of_width w)

  | Output (_, w)
  | Name   (_, w)
  | Buf    w ->
      add_cell_index cell (index_of_cell (input 0))
    
  | Const  value ->
      let w = String.length value in
      add_cell_index cell !next_index;
      next_index := !next_index + (words_of_width w)

  | Concat (wl, wr) ->
      let w = wl + wr in
      add_cell_index cell !next_index;
      next_index := !next_index + (words_of_width w)

  | Select (_, bit) ->
      add_cell_index cell !next_index;
      next_index := !next_index + 1

  | Eq _
  | Lt _ ->
      add_cell_index cell !next_index;
      next_index := !next_index + 1

  | Ff w ->
      add_cell_index cell !next_index;
      next_index := !next_index + (words_of_width w * 2 + 1)

  | Ffc w ->
      add_cell_index cell !next_index;
      next_index := !next_index + (words_of_width w * 2 + 2)

  | Dangle ->
      add_cell_index cell 0  (* So it doesn't raise an error it the calc phase. *)

  | Bbox _ -> raise (Invalid_argument "Black-boxes not supported in C code generation.")
;;
  
let channel_c = ref stdout;;
let channel_h = ref stdout;;

let write_string string =
  output_string !channel_c string
;;

let write line =
  output_string !channel_c line;
  output_char   !channel_c '\n'
;;

let write_h line =
  output_string !channel_h line;
  output_char   !channel_h '\n'
;;




(** Builds the simulation data structure. *)
let output_sim_struct scope cells =
  let indent = ref 0 in
  let write line =
    for i = 1 to !indent do
      output_string !channel_h "  ";
      output_string !channel_c "  ";
    done;
    output_string !channel_h line;
    output_string !channel_c line;
    output_char   !channel_h '\n';
    output_char   !channel_c '\n'
  in
  let rec declare_names scope_item =
    match scope_item with
    | Scope scope ->
        write "struct {";
        incr indent;
        List.iter declare_names (items_of_scope scope);
        decr indent;
        write ("} " ^ instance_name_of_scope scope ^ ";")
    | Cell cell ->
        (match info_of_cell cell with
        | Input  (name, w) ->
            write ("unsigned long * " ^ name ^ ";  // input  " ^ name ^ " : " ^ string_of_int w ^ " bits, " ^ string_of_int (words_of_width w) ^ " words")
        | Output (name, w) ->
            write ("unsigned long * " ^ name ^ ";  // output " ^ name ^ " : " ^ string_of_int w ^ " bits, " ^ string_of_int (words_of_width w) ^ " words")
        | Name   (name, w) -> write ("unsigned long * " ^ name ^ ";")
        | _ -> ()
        )
  in
  write "";
  write "// Simulator Data Type";
  write "struct simulator_s {";
  incr indent;
  write "struct {";
  incr indent;
  declare_names (Scope (root_of_scope scope));
  decr indent;
  write "} signals;";
  write ("unsigned long memory[" ^ string_of_int !next_index ^ "];");
  decr indent;
  write "};";
  write "";
  write "typedef struct simulator_s *simulator_t;";
  write ""
;;


(** Creates the "new" and "delete" functions. *)
let output_new_delete () =
  write_h "// Simulator Constructor";
  write_h "simulator_t new_simulator();\n";
  write_h "// Simulator Destructor";
  write_h "void delete_simulator(simulator_t);\n";
  write_string ("simulator_t new_simulator()
{
  simulator_t simulator;
  simulator = (simulator_t) malloc(sizeof(*simulator));
  return simulator;
}

void delete_simulator(simulator_t simulator)
{
  free ((void*) simulator);
}

")
;;


(** Creates the primitive functions. *)
let output_primitive_functions () =
  write_string "
void fnf_mask(int words, unsigned long mask, unsigned long* x)
{
  x[words - 1] = x[words - 1] & mask;
}

void fnf_not(int words, unsigned long mask, unsigned long* a, unsigned long* x)
{
  int i;
  for (i = 0; i < words; i++)
    x[i] = ~ a[i];
  fnf_mask(words, mask, x);
}

void fnf_and(int words, unsigned long mask, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  for (i = 0; i < words; i++)
    x[i] = a[i] & b[i];
  fnf_mask(words, mask, x);
}

void fnf_xor(int words, unsigned long mask, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  for (i = 0; i < words; i++)
    x[i] = a[i] ^ b[i];
  fnf_mask(words, mask, x);
}

void fnf_or(int words, unsigned long mask, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  for (i = 0; i < words; i++)
    x[i] = a[i] | b[i];
  fnf_mask(words, mask, x);
}

void fnf_concat_over(int words_a, int words_b, int shift_left, int shift_right, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  int j;
  for (i = 0; i < words_b; i++)
    x[i] = b[i];
  for (i = 0, j = words_b - 1; i < words_a; i++, j++) {
    x[j] = x[j] | (0xFFFFFFFF & (a[i] << shift_left));
    x[j + 1] = a[i] >> shift_right;
  }
}

void fnf_concat_under(int words_a, int words_b, int shift_left, int shift_right, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  int j;
  for (i = 0; i < words_b; i++)
    x[i] = b[i];
  for (i = 0, j = words_b - 1; i < words_a - 1; i++, j++) {
    x[j] = x[j] | (0xFFFFFFFF & (a[i] << shift_left));
    x[j + 1] = a[i] >> shift_right;
  }
  x[j] = x[j] | (0xFFFFFFFF & (a[i] << shift_left));
}

void fnf_concat_simple(int words_a, int words_b, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  int j;
  for (i = 0; i < words_b; i++)
    x[i] = b[i];
  for (i = 0, j = words_b; i < words_a; i++, j++)
    x[j] = a[i];
}

void fnf_select(int word, int bit, unsigned long* a, unsigned long* x)
{
  x[0] = 1 & (a[word] >> bit);
}

void fnf_eq(int words, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  x[0] = 1;
  for (i = 0; i < words; i++)
    x[0] = x[0] && a[i] == b[i];
}

void fnf_lt(int words, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  x[0] = 0;
  for (i = words - 1; i >= 0; i--) {
    if (a[i] < b[i]) {
      x[0] = 1;
      return;
    }
    else if (a[i] > b[i]) {
      return;
    }
  }
}

void fnf_add(int words, unsigned long mask, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  unsigned long long tmp = 0;
  for (i = 0; i < words; i++) {
    tmp = (unsigned long long) a[i] + (unsigned long long) b[i] + tmp;
    x[i] = (unsigned long) (tmp & 0xFFFFFFFF);
    tmp = (tmp >> 32) & 1;
  }
  fnf_mask(words, mask, x);
}

void fnf_sub(int words, unsigned long mask, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i;
  unsigned long long tmp = 0;
  for (i = 0; i < words; i++) {
    tmp = (unsigned long long) a[i] - (unsigned long long) b[i] - tmp;
    x[i] = (unsigned long) (tmp & 0xFFFFFFFF);
    tmp = (tmp >> 32) & 1;
  }
  fnf_mask(words, mask, x);
}

void fnf_mul(int words, unsigned long mask, unsigned long* a, unsigned long* b, unsigned long* x)
{
  int i, ia, ib, ic;
  unsigned long long tmp;
  for (i = 0; i < words; i++) x[i] = 0;
  for (i = 0; i < words; i++) {
    for (ia = i, ib = 0; ia >= 0; ia--, ib++) {
      tmp = (unsigned long long) (a[ia]) * (unsigned long long) (b[ib]);
      for (ic = i; ic < words; ic++) {
        tmp = tmp + (unsigned long long) x[ic];
        x[ic] = (unsigned long) (tmp & 0xFFFFFFFF);
        tmp = (tmp >> 8);
      }
    }
  }
  fnf_mask(words, mask, x);
}

void fnf_mux(int words, unsigned long* select, unsigned long* on_0, unsigned long* on_1, unsigned long* x)
{
  int i;
  for (i = 0; i < words; i++)
    x[i] = select[0] ? on_1[i] : on_0[i];
}

void fnf_ff(int words, unsigned long* clk, unsigned long* q)
{
  int i;
  unsigned long* state_clk = & q[words];
  unsigned long* state_d   = & q[words + 1];
  if (clk[0] && ! state_clk[0]) 
    for (i = 0; i < words; i++)
      q[i] = state_d[i];
  state_clk[0] = clk[0];
}

void fnf_ff_update(int words, unsigned long* data, unsigned long* q)
{
  int i;
  unsigned long* state_d   = & q[words + 1];
  for (i = 0; i < words; i++)
    state_d[i] = data[i];
}

void fnf_ffc(int words, unsigned long* clr, unsigned long* clk, unsigned long* q)
{
  int i;
  unsigned long* state_clr = & q[words];
  unsigned long* state_clk = & q[words + 1];
  unsigned long* state_d   = & q[words + 2];
  if (clr[0] && ! state_clr[0])
    for (i = 0; i < words; i++)
      q[i] = 0;
  else if (clk[0] && ! state_clk[0]) 
    for (i = 0; i < words; i++)
      q[i] = state_d[i];
  state_clr[0] = clr[0];
  state_clk[0] = clk[0];
}

void fnf_ffc_update(int words, unsigned long* data, unsigned long* q)
{
  int i;
  unsigned long* state_d = & q[words + 2];
  for (i = 0; i < words; i++)
    state_d[i] = data[i];
}

"
;;



(** Misc Helpers *)


let hex_of_bin bin =
  let m = String.length bin mod 4 in
  let bin = if m = 0 then bin else String.make (4 - m) '0' ^ bin in
  let rec f sofar bin =
    if bin = "" then
      sofar
    else
      f (sofar ^ match String.sub bin 0 4 with
        | "0000" -> "0"
        | "0001" -> "1"
        | "0010" -> "2"
        | "0011" -> "3"
        | "0100" -> "4"
        | "0101" -> "5"
        | "0110" -> "6"
        | "0111" -> "7"
        | "1000" -> "8"
        | "1001" -> "9"
        | "1010" -> "A"
        | "1011" -> "B"
        | "1100" -> "C"
        | "1101" -> "D"
        | "1110" -> "E"
        | "1111" -> "F"
        | _ -> raise (Invalid_argument "Invalid binary string.")
        ) (String.sub bin 4 (String.length bin - 4))
  in
  "0x" ^ f "" bin
;;

let mask_of_width width =
  let remainder = width mod 32 in
  if remainder = 0 then
    "0xFFFFFFFF"
  else
    hex_of_bin (String.make remainder '1')
;;


(** Cell Initialization *)

let init_cell cell =
  let id = id_of_cell cell in
  let input num =
    let cell = producer_of_port (port_of_cell cell num) in
    (match info_of_cell cell with Dangle -> raise (Invalid_argument "encountered unconnected cell") | _ -> ());
    id_of_cell cell
  in
  match info_of_cell cell with

  | Input  (name, w)
  | Output (name, w)
  | Name   (name, w) ->
      write ("  simulator->signals." ^ String2.join (path_of_cell cell) "." ^ "." ^ name ^ " = &(memory[" ^ string_of_int (index_of_cell cell) ^ "]);")

  | Const  value ->
      let rec f value index =
        let len = String.length value in
        if len <= 32 then
          write ("  memory[" ^ string_of_int index ^ "] = " ^ hex_of_bin value ^ ";")
        else begin
          write ("  memory[" ^ string_of_int index ^ "] = " ^ hex_of_bin (String.sub value (len - 32) 32) ^ ";");
          f (String.sub value 0 (len - 32)) (index + 1)
        end
      in
      f value (index_of_cell cell)

  | Ff w  ->
      write ("  memory[" ^ string_of_int (index_of_cell cell + words_of_width w) ^ "] = 1;")

  | Ffc w  ->
      write ("  memory[" ^ string_of_int (index_of_cell cell + words_of_width w    ) ^ "] = 1;");
      write ("  memory[" ^ string_of_int (index_of_cell cell + words_of_width w + 1) ^ "] = 1;");

  | _ -> ()
;;


let words_of_width width =
  string_of_int (words_of_width width)
;;


(** Define the init function. *)
let output_init scope cells =
  write_h "// Simulator Initialization";
  write_h "void init_simulator(simulator_t);";
  write_h "";
  write_string ("void init_simulator(simulator_t simulator)
{ 
  int i;
  unsigned long *memory;
  memory = simulator->memory;
  for (i = 0; i < " ^ string_of_int !next_index ^ "; i++) simulator->memory[i] = 0;
");
  List.iter init_cell cells;
  write "}\n";
;;



(** Cell Calculation *)

let calc_cell cell =
  let index = index_of_cell cell in
  let input num =
    let cell = producer_of_port (port_of_cell cell num) in
    (match info_of_cell cell with Dangle -> raise (Invalid_argument "encountered unconnected cell") | _ -> ());
    index_of_cell cell
  in
  let mem_index = "& memory[" ^ string_of_int index ^ "]" in
  let mem_input num = "& memory[" ^ string_of_int (input num) ^ "]" in
  let write_fnf_func name args =
    write ("  " ^ name ^ "(" ^ String2.join args ", " ^ ");")
  in
  match info_of_cell cell with

  | Input  (_, w) ->
      write_fnf_func "fnf_mask" [words_of_width w; mask_of_width w; mem_index]

  | Output _
  | Name   _
  | Dangle
  | Const  _
  | Buf    _ -> ()

  | Not    w ->
      write_fnf_func "fnf_not" [words_of_width w; mask_of_width w; mem_input 0; mem_index]

  | And    w ->
      write_fnf_func "fnf_and" [words_of_width w; mask_of_width w; mem_input 0; mem_input 1; mem_index]

  | Xor    w ->
      write_fnf_func "fnf_xor" [words_of_width w; mask_of_width w; mem_input 0; mem_input 1; mem_index]

  | Or     w ->
      write_fnf_func "fnf_or" [words_of_width w; mask_of_width w; mem_input 0; mem_input 1; mem_index]

  | Concat (wl, wr) ->
      let shift_left  = wr mod 32 in
      let shift_right = 32 - shift_left in
      if shift_left = 0 then
        write_fnf_func "fnf_concat_simple" [words_of_width wl; words_of_width wr; mem_input 0; mem_input 1; mem_index]
      else if shift_right > wl mod 32 then
        write_fnf_func "fnf_concat_under" [words_of_width wl; words_of_width wr; string_of_int shift_left; string_of_int shift_right; mem_input 0; mem_input 1; mem_index]
      else
        write_fnf_func "fnf_concat_over" [words_of_width wl; words_of_width wr; string_of_int shift_left; string_of_int shift_right; mem_input 0; mem_input 1; mem_index]

  | Select (w, bit) ->
      write_fnf_func "fnf_select" [string_of_int (bit / 32); string_of_int (bit mod 32); mem_input 0; mem_index]

  | Eq     w ->
      write_fnf_func "fnf_eq" [words_of_width w; mem_input 0; mem_input 1; mem_index]

  | Lt     w ->
      write_fnf_func "fnf_lt" [words_of_width w; mem_input 0; mem_input 1; mem_index]

  | Add    w ->
      write_fnf_func "fnf_add" [words_of_width w; mask_of_width w; mem_input 0; mem_input 1; mem_index]

  | Sub    w ->
      write_fnf_func "fnf_sub" [words_of_width w; mask_of_width w; mem_input 0; mem_input 1; mem_index]

  | Mul    w ->
      write_fnf_func "fnf_mul" [words_of_width w; mask_of_width w; mem_input 0; mem_input 1; mem_index]

  | Mux    w ->
      write_fnf_func "fnf_mux" [words_of_width w; mem_input 0; mem_input 1; mem_input 2; mem_index]

  | Ff     w ->
      write_fnf_func "fnf_ff" [words_of_width w; mem_input 0; mem_index]

  | Ffc    w ->
      write_fnf_func "fnf_ffc" [words_of_width w; mem_input 0; mem_input 1; mem_index]

  | Bbox _ -> raise (Invalid_argument "Black-boxes not supported in C code generation.")
;;

let reg_update_cell cell =
  let input num =
    let cell = producer_of_port (port_of_cell cell num) in
    (match info_of_cell cell with Dangle -> raise (Invalid_argument "encountered unconnected cell") | _ -> ());
    index_of_cell cell
  in
  let mem_index = "& memory[" ^ string_of_int (index_of_cell cell) ^ "]" in
  let mem_input num = "& memory[" ^ string_of_int (input num) ^ "]" in
  let write_fnf_func name args =
    write ("  " ^ name ^ "(" ^ String2.join args ", " ^ ");")
  in
  match info_of_cell cell with
  | Ff     w ->
      write_fnf_func "fnf_ff_update" [words_of_width w; mem_input 1; mem_index]

  | Ffc    w ->
      write_fnf_func "fnf_ffc_update" [words_of_width w; mem_input 2; mem_index]

  | _ -> ()
;;


(** Define the calc function. *)
let output_calc cells =
  write_h "// Simulator Cycle Calculation";
  write_h "void calc_simulator(simulator_t);";
  write_h "";
  write "void calc_simulator(simulator_t simulator)";
  write "{";
  write "  unsigned long *memory;";
  write "  memory = simulator->memory;";
  List.iter calc_cell cells;
  List.iter reg_update_cell cells;
  write "}";
  write "";
;;





(** Orders the cells for execution. *)
let order_cells scope =
  let rec order remaining computed =
    if remaining = [] then
      List.rev computed
    else
      let is_ready ports =
        List.for_all (fun port ->
          let id = id_of_cell (producer_of_port port) in
          List.exists (fun cell -> id = id_of_cell cell) computed
        ) ports
      in
      let is_ready cell =
        match info_of_cell cell with
        | Dangle
        | Input _
        | Const _ -> true
        | Ff    _ -> is_ready [port_of_cell cell 0]
        | Ffc   _ -> is_ready [port_of_cell cell 0; port_of_cell cell 1]
        |       _ -> is_ready (ports_of_cell cell)
      in
      let ready, not_ready = List.partition is_ready remaining in
      if ready = [] then raise (Invalid_argument "No cells can be scheduled.  Suspecting a combinational or Q-to-D loop.");
      order not_ready (ready @ computed) 
  in
  order (all_cells scope) []
;;





(** Output a C model given a scope.  [output_c c_channer h_channel scope] *)
let output_c out_channel_c out_channel_h scope =
  channel_c := out_channel_c;
  channel_h := out_channel_h;
  let cells = order_cells scope in
  List.iter allocate_cell cells;
  write_h "#ifdef __cplusplus\nextern \"C\" {\n#endif\n";
  write "#include <stdlib.h>";
  output_sim_struct scope cells;
  output_new_delete ();
  output_primitive_functions ();
  output_init scope cells;
  output_calc cells;
  write_h "#ifdef __cplusplus\n}\n#endif\n";
  reset ()
;;

