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


(** General error reporting. *)
exception Error of string;;

exception Invalid_port_number;;

let debug msg = print_string msg; print_newline () ;;


(** The netlist is the global conection matrix. *)
type netlist = {
          root      : scope;
  mutable cells     : cell_netlist_info array;
  mutable next_cell : int
}

(** Cell type and connectivity information. *)
and cell_netlist_info = {
          scope     : scope;
          info      : cell_info;
          producers : cell array;
  mutable consumers : port list
}

(** A scope is a hierarchical object that contains cells, properties, and subscopes. *)
and  scope = {
          netlist  : netlist;
          module_  : string;
          instance : string;
          is_root  : bool;
          parent   : scope;
  mutable items    : item list
}

(** A scope item can be either a subscope, a cell, or a property. *)
and  item = Scope of scope | Cell of cell

(** A cell is a primitive logic operation. *)
and  cell = netlist * int

(** A port is an input to a cell. *)
and  port = cell * int

(** A property is temporal relation of states in a design.  Supports CTL, LTL, and CTL*. *)
and  property = int (* XXX *)

(** Cell info gives the netlist user visability to cell information. *)
and  cell_info = Input  of string * int
               | Output of string * int
               | Name   of string * int
               | Dangle
               | Const  of string
               | Buf    of int
               | Not    of int
               | And    of int
               | Xor    of int
               | Or     of int
               | Concat of int * int
               | Select of int * int list
               | Eq     of int
               | Lt     of int
               | Add    of int
               | Sub    of int
               | Mul    of int
               | Mux    of int
               | Ff     of int
               | Ffc    of int
;;



(* Internal netlist operations. *)


(** Netlist containing cell. *)
let netlist_of_cell (netlist, _) =
  netlist
;;

(** Netlist containing port. *)
let netlist_of_port ((netlist, _), _) =
  netlist
;;

(** Cell id of cell. *)
let id_of_cell (_, id) =
  id
;;

(** Cell id of port. *)
let id_of_port ((_, id), _) =
  id
;;

(** Number of port. *)
let num_of_port ((_, _), n) =
  n
;;

(** Cell of port. *)
let cell_of_port (cell, _) =
  cell
;;

(** Check that a port and a cell belong to the same netlist. *)
let assert_same_netlist cell port =
  if netlist_of_cell cell != netlist_of_port port then raise (Error "Cell and port do not belong to the same netlist.")
;;

(** Adds a consumer reference to producer. *)
let add_consumer cell port = 
  assert_same_netlist cell port;
  let consumer_info = (netlist_of_cell cell).cells.(id_of_cell cell) in
  consumer_info.consumers <- port :: consumer_info.consumers
;;

let is_ports_same ((_, c1), p1) ((_, c2), p2) =
  (c1, p1) = (c2, p2)
;;

(** Removes a consumer reference from producer. *)
let remove_consumer cell port =
  let consumer_cell_id = id_of_port port in
  let port_num = num_of_port port in
  assert_same_netlist cell port;
  let consumer_info = (netlist_of_cell cell).cells.(id_of_cell cell) in
  consumer_info.consumers <- List.filter (fun p -> not (is_ports_same p port)) consumer_info.consumers
;;


(** Adds a new cell to the netlist. *)
let add_new_cell scope info arity =
  let netlist = scope.netlist in
  let dangle  = netlist, 0 in
  (* Allocate memory if needed. *)
  if netlist.next_cell >= Array.length netlist.cells then
    netlist.cells <- Array.append netlist.cells (Array.make 1024 netlist.cells.(0));
  let cell_id = netlist.next_cell in
  netlist.next_cell <- netlist.next_cell + 1;
  let cell = netlist, cell_id in
  netlist.cells.(cell_id) <- { scope = scope; info = info; producers = Array.make arity dangle; consumers = [] };
  scope.items <- Cell cell :: scope.items;
  for i = 0 to arity - 1 do
    add_consumer dangle (cell, i)
  done;
  cell
;;

(** Cleans up the netlist. *)
let collect_garbage scope =
  () (* XXX *)
;;



(* Scope Manipulation *)

(** Creates a root scope and global netlist. *)
let create_root_scope module_name =
  let rec netlist = { 
    root      = scope;
    cells     = [| dangle |];
    next_cell = 1
    }
  and scope = {
    netlist  = netlist;
    module_  = module_name;
    instance = module_name;
    is_root  = true;
    parent   = scope;
    items    = [ Cell (netlist, 0) ]
    }
  and dangle = { scope = scope; info = Dangle; producers = [||]; consumers = [] }
  in
  scope
;;

(** Creates a sub scope. *)
let create_sub_scope parent_scope module_name instance_name =
  let scope = {
    netlist  = parent_scope.netlist;
    module_  = module_name;
    instance = instance_name;
    is_root  = false;
    parent   = parent_scope;
    items    = []
  } in
  parent_scope.items <- Scope scope :: parent_scope.items;
  scope
;;

(** Returns the parent of a scope.  If the scope is root, it returns itself. *)
let parent_of_scope scope =
  scope.parent
;;

(** Returns the root scope. *)
let root_of_scope scope =
  scope.netlist.root
;;

(** Returns the items of a scope. *)
let items_of_scope scope =
  List.rev scope.items
;;

(** Returns the module name of a scope. *)
let module_name_of_scope scope =
  scope.module_
;;

(** Returns the module name of a scope. *)
let instance_name_of_scope scope =
  scope.instance
;;

(** Returns a list of all cells. *)
let all_cells scope =
  let netlist = scope.netlist in
  let rec f sofar index =
    if index < 0 then
      sofar
    else
      f ((netlist, index) :: sofar) (index - 1)
  in
  f [] (netlist.next_cell - 1)
;;

(** Returns the enclosing scope of a cell. *)
let scope_of_cell (netlist, cell_id) =
  netlist.cells.(cell_id).scope
;;

let rec revpath_of_scope scope =
  if scope.is_root then
    [scope.instance]
  else
    scope.instance :: revpath_of_scope scope.parent
;;
    
(** Retuns the hierarchical scope path of a scope. *)
let path_of_scope scope =
  List.rev (revpath_of_scope scope)
;;

(** Retuns the hierarchical scope path of a cell. *)
let path_of_cell cell =
  path_of_scope (scope_of_cell cell)
;;



(** Cell Manipulation *)

(** Returns cell information. *)
let info_of_cell cell =
  let netlist, cell_id = cell in
  netlist.cells.(cell_id).info
;;

(** String name of cell. *)
let name_of_cell cell =
  match info_of_cell cell with
      | Input  _ -> "input"
      | Output _ -> "output"
      | Name   _ -> "name"
      | Dangle   -> "dangle"
      | Const  _ -> "const"
      | Buf    _ -> "buf"
      | Not    _ -> "not"
      | And    _ -> "and"
      | Xor    _ -> "xor"
      | Or     _ -> "or"
      | Concat _ -> "concat"
      | Select _ -> "select"
      | Eq     _ -> "eq"
      | Lt     _ -> "lt"
      | Add    _ -> "add"
      | Sub    _ -> "sub"
      | Mul    _ -> "mul"
      | Mux    _ -> "mux"
      | Ff     _ -> "ff"
      | Ffc    _ -> "ffc"
;;

(** Output width of a cell. *)
let width_of_cell cell =
  match info_of_cell cell with
  | Dangle -> raise (Error "Dangle cell does not have an output width.")
  | Input  (_, w) -> w
  | Output _ -> raise (Error "Output cell does not have an output width.")
  | Name   _ -> raise (Error "Name cell does not have an output width.")
  | Const  s -> String.length s
  | Buf    w
  | Not    w
  | And    w
  | Xor    w
  | Or     w -> w
  | Concat (a, b) -> a + b
  | Select (_, bits) -> List.length bits
  | Eq     w
  | Lt     w -> 1
  | Add    w
  | Sub    w
  | Mul    w
  | Mux    w
  | Ff     w
  | Ffc    w -> w
;;

(** Input width of a cell port. *)
let width_of_port (cell, port_num) =
  match info_of_cell cell with
  | Dangle   -> raise (Invalid_argument "Dangle has no input port.")
  | Input  _ -> raise (Invalid_argument "Input has no input port.")
  | Const  _ -> raise (Invalid_argument "Const has no input port.")

  | Output (_, w)
  | Name   (_, w)
  | Buf    w
  | Not    w
  | Select (w, _) -> if port_num = 0 then w else raise Invalid_port_number

  | And    w
  | Xor    w
  | Or     w
  | Eq     w
  | Lt     w
  | Add    w
  | Sub    w
  | Mul    w -> if port_num = 0 || port_num = 1 then w else raise Invalid_port_number

  | Concat (a, b) -> if port_num = 0 then a else if port_num = 1 then b else raise Invalid_port_number

  | Mux    w      -> if port_num = 0 then 1 else if port_num = 1 || port_num = 2 then w else raise Invalid_port_number

  | Ff     w -> if port_num = 0 then 1 else if port_num = 1 then w else raise Invalid_port_number
  | Ffc    w -> if port_num = 0 || port_num = 1 then 1 else if port_num = 2 then w else raise Invalid_port_number
;;

(** The number of input ports of a cell. *)
let arity_of_cell cell =
  match info_of_cell cell with
  | Dangle
  | Input  _
  | Const  _ -> 0

  | Output _
  | Name   _
  | Buf    _
  | Not    _
  | Select _ -> 1

  | And    _
  | Xor    _
  | Or     _
  | Concat _
  | Eq     _
  | Lt     _
  | Add    _
  | Sub    _
  | Mul    _ -> 2

  | Mux    _ -> 3

  | Ff     _ -> 2
  | Ffc    _ -> 3
;;


(** Checks if port is dangling. *)
let is_port_dangling ((netlist, cell_id), port_id) =
  let _, producer_id = netlist.cells.(cell_id).producers.(port_id) in
  producer_id = 0
;;

(** Checks if cell is a valid producer, i.e., not a dangle, output, or a name. *)
let is_cell_producer cell =
  match info_of_cell cell with
  | Output _
  | Name   _ -> false
  | _        -> true
;;

(** Ports of a cell. *)
let ports_of_cell cell =
  let arity = arity_of_cell cell in
  let rec f i =
    if i = arity then [] else (cell, i) :: f (i + 1)
  in
  f 0
;;

(** One port of a cell. *)
let port_of_cell cell num =
  if num >= arity_of_cell cell then raise Invalid_port_number;
  cell, num
;;

(** Producing cell of port. *)
let producer_of_port port =
  (netlist_of_port port).cells.(id_of_port port).producers.(num_of_port port)
;;

(** Consuming ports of a cell. *)
let consumers_of_cell cell =
  (netlist_of_cell cell).cells.(id_of_cell cell).consumers
;;
  
(** Reconnects a cell to a port. *)
let reconnect cell port =
  if not (is_cell_producer cell) then raise (Error ("Can not connect to a non producer cell: " ^ string_of_int (id_of_cell cell) ^ " -> " ^ string_of_int (id_of_port port) ^ "," ^ string_of_int (num_of_port port)));
  remove_consumer (producer_of_port port) port;
  add_consumer cell port;
  (netlist_of_port port).cells.(id_of_port port).producers.(num_of_port port) <- cell
;;
  
(** Connects a cell to a port.  Port must be dangling prior to connection. *)
let connect cell port =
  if not (is_port_dangling port) then raise (Error "Cell port is already connected.");
  reconnect cell port
;;















(* Cell Creation *)

(** The special cell that represents unconnected inputs. *)
let dangle scope = scope.netlist, 0;;

(** Creates a new input cell. *)
let create_input scope name width =
  add_new_cell scope (Input (name, width)) 0
;;

(** Creates a new output cell. *)
let create_output scope name width =
  let cell = add_new_cell scope (Output (name, width)) 1 in
  cell, (cell, 0)
;;

(** Creates a new signal name cell. *)
let create_name scope name width =
  let cell = add_new_cell scope (Name (name, width)) 1 in
  cell, (cell, 0)
;;

(** Creates a constant cell. *)
let create_const scope bits =
  add_new_cell scope (Const bits) 0
;;

(** Creates a buffer cell. *)
let create_buf scope width =
  let cell = add_new_cell scope (Buf width) 1 in
  cell, (cell, 0)
;;

(** Creates a NOT gate. *)
let create_not scope width =
  let cell = add_new_cell scope (Not width) 1 in
  cell, (cell, 0)
;;

(** Creates a AND gate. *)
let create_and scope width =
  let cell = add_new_cell scope (And width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a XOR gate. *)
let create_xor scope width =
  let cell = add_new_cell scope (Xor width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a OR gate. *)
let create_or scope width =
  let cell = add_new_cell scope (Or width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a concatenation. *)
let create_concat scope width_left width_right =
  let cell = add_new_cell scope (Concat (width_left, width_right)) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a bit selection. *)
let create_select scope width bits =
  if List.exists (fun n -> n >= width || n < 0) bits then raise (Error "Invalid bit select.");
  let cell = add_new_cell scope (Select (width, bits)) 1 in
  cell, (cell, 0)
;;

(** Creates a equal comparison. *)
let create_eq scope width =
  let cell = add_new_cell scope (Eq width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a less than comparison. *)
let create_lt scope width =
  let cell = add_new_cell scope (Lt width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates an adder. *)
let create_add scope width =
  let cell = add_new_cell scope (Add width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a subtractor. *)
let create_sub scope width =
  let cell = add_new_cell scope (Sub width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a multiplier. *)
let create_mul scope width =
  let cell = add_new_cell scope (Mul width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates a mux. *)
let create_mux scope width =
  let cell = add_new_cell scope (Mux width) 3 in
  cell, (cell, 0), (cell, 1), (cell, 2)
;;

(** Creates an ff. *)
let create_ff scope width =
  let cell = add_new_cell scope (Ff width) 2 in
  cell, (cell, 0), (cell, 1)
;;

(** Creates an ffc. *)
let create_ffc scope width =
  let cell = add_new_cell scope (Ffc width) 3 in
  cell, (cell, 0), (cell, 1), (cell, 2)
;;


