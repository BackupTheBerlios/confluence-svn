(*
    Confluence System Design Language Compiler
    Copyright (C) 2003-2005 Tom Hawkins (tomahawkins@yahoo.com)

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

(* Types *)

type producer = cell;;

type scope_holder = No_scope of string * string | Scope of scope;;

type system = System      of system *          (* Parent system. *)
                             int *             (* SysId. *)
                             scope_holder ref *    (* FNF scope. *)
                             bool ref *        (* Cre determined. *)
                             string ref *      (* Clock domain. *)
                             producer ref *    (* Reset. *)
                             producer ref *    (* Enable. *)
                             Loc.loc list      (* Call path. *)

            | System_root of int *             (* SysId. *)
                             scope *           (* FNF scope. *)
                             string *          (* Clock domain. *)
                             producer *        (* Reset. *)
                             producer          (* Enable. *)
;;

type stateful = Reg of system * port * port * port;; (* system, clock_port, reset_port, enable_port *)



(** Local Stateful Data *)

let sub_clocks  = Hashtbl.create 64;;    (* Clock name to producing cell. *)
let sub_resets  = Hashtbl.create 64;;    (* SysId to producer IdSet. *)
let sub_enables = Hashtbl.create 64;;    (* SysId to producer IdSet. *)
let next_system_id  = ref 0;;            (* Next SysId. *)
let statefuls  = ref [];;                (* list of stateful components. *)

let fnf  = create_root_scope "top";; (* XXX *)
 
let zero = create_const fnf "0";;
let one  = create_const fnf "1";;


(** System Functions *)

let rec scope_of_system system =
  match system with
  | System_root (_, scope, _, _, _) -> scope
  | System (parent, _, scope, _, _, _, _, _) ->
    (match !scope with
    | No_scope (module_name, instance_name) ->
        (*
        let new_scope = create_sub_scope (scope_of_system parent) module_name instance_name in
        *)
        let new_scope = fnf in
        scope := Scope new_scope;
        new_scope
    | Scope scope -> scope)
;;

let id_of_system system =
  match system with
  | System_root (id, _, _, _, _)
  | System (_, id, _, _, _, _, _, _) -> id
;;

let locs_of_system system =
  match system with
  | System_root _ -> []
  | System (_, _, _, _, _, _, _, locs) -> locs
;;

let new_root_system () =
  let id = !next_system_id in
  incr next_system_id;
  System_root (id, fnf, "clock", zero, one)
;;

let new_sub_system parent module_name instance_name instance_locs component_loc =
  let id = !next_system_id in
  incr next_system_id;
  System (parent, id, ref (No_scope (module_name, instance_name)), ref false, ref "", ref zero, ref one, instance_locs)
;;
  
(** Determine the clock, reset, and enable for a system. *)
let rec determine_cre system =
  (*
    clock  = pClock when clock = "" else clock
    reset  = parentReset OR localResets
    enable = parentEnable AND (parentReset OR localResets) 
  *)
  match system with
  | System_root (_, _, clock, reset, enable) -> clock, reset, enable
  | System (parent, id, space, cre_determined, clock, reset, enable, locs) ->
      if not !cre_determined then begin
        cre_determined := true;
        let parent_clock, parent_reset, parent_enable = determine_cre parent in
        let pid = id_of_system parent in
        let parent_scope = scope_of_system parent in
        let fold_or a b =
          let cell, pa, pb = create_or parent_scope 1 in
          connect a pa;
          connect b pb;
          cell
        in
        let fold_and a b =
          let cell, pa, pb = create_and parent_scope 1 in
          connect a pa;
          connect b pb;
          cell
        in

        if !clock = "" then clock := parent_clock;

        if not (Hashtbl.mem sub_resets id) then
          reset := parent_reset
        else
          reset := List.fold_left fold_or parent_reset (Hashtbl.find sub_resets id);

        if not (Hashtbl.mem sub_enables id) then
          enable := parent_enable
        else
          enable := List.fold_left fold_and parent_enable (Hashtbl.find sub_enables id);
      end;
      !clock, !reset, !enable
;;


(** Signal width. *)
let width_of_producer = width_of_cell;;



(** Synchronization Functions *)

let set_clock_domain system clock_name error =
  match system with
  | System_root _ -> error "System error.  Can not set clock domain for root system."
  | System (_, _, _, _, clock, _, _, _) ->
      if !clock <> "" then error ("System error.  System already assigned clock domain \"" ^ !clock ^ "\".  Can not be set to \"" ^ clock_name ^ "\".");
      clock := clock_name
;;

let add_sub_reset system producer error =
  match system with
  | System_root _ -> error "System error.  Can not reset root system."
  | System (_, id, _, _, _, _, _, _) ->
      if Hashtbl.mem sub_resets id then
        Hashtbl.replace sub_resets id (producer :: (Hashtbl.find sub_resets id))
      else
        Hashtbl.add sub_resets id [producer]
;;

let add_sub_enable system producer error  =
  match system with
  | System_root _ -> error "System error.  Can not enable root system."
  | System (_, id, _, _, _, _, _, _) ->
      if Hashtbl.mem sub_enables id then
        Hashtbl.replace sub_enables id (producer :: (Hashtbl.find sub_enables id))
      else
        Hashtbl.add sub_enables id [producer]
;;

(** Post Evaluation Functions *)

(** A running list of required clocks. *)
let clocks = Hashtbl.create 64;;

(** Connects clocks, resets, and enables to stateful primitives. *)
let connect_stateful stateful =
  match stateful with
    Reg (system, clock_port, reset_port, enable_port) ->
      let clock, reset, enable = determine_cre system in
      if not (Hashtbl.mem clocks clock) then Hashtbl.add clocks clock (create_input fnf clock 1);
      connect (Hashtbl.find clocks clock) clock_port;
      connect reset reset_port;
      connect enable enable_port
;;
      
(** Generates and FNF netlist. *)
let output_fnf channel =
  List.iter connect_stateful !statefuls;
  Fnf_out.output_fnf channel fnf
;;



(** Logic Primitive Constructors *)

let new_input system name width =
  create_input (scope_of_system system) name width
;;

let new_output system name producer =
  let cell, a = create_output (scope_of_system system) name (width_of_cell producer) in
  connect producer a
;;

let new_const system value =
  create_const (scope_of_system system) value
;;

let new_buf system p0 =
  let cell, a = create_buf (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  cell
;;

let new_not system p0 =
  let cell, a = create_not (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  cell
;;

let new_and system p0 p1 =
  let cell, a, b = create_and (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_xor system p0 p1 =
  let cell, a, b = create_xor (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_or system p0 p1 =
  let cell, a, b = create_or (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_concat system p0 p1 =
  let cell, a, b = create_concat (scope_of_system system) (width_of_cell p0) (width_of_cell p1) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_select system p0 bit =
  let cell, a = create_select (scope_of_system system) (width_of_cell p0) bit in
  connect p0 a;
  cell
;;

let new_equ system p0 p1 =
  let cell, a, b = create_eq (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_lt system p0 p1 =
  let cell, a, b = create_lt (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_add system p0 p1 =
  let cell, a, b = create_add (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_sub system p0 p1 =
  let cell, a, b = create_sub (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_mul system p0 p1 =
  let cell, a, b = create_mul (scope_of_system system) (width_of_cell p0) in
  connect p0 a;
  connect p1 b;
  cell
;;

let new_mux system pred t f =
  let cell, a, b, c = create_mux (scope_of_system system) (width_of_cell t) in
  connect pred a;
  connect f b;
  connect t c;
  cell
;;


(** Stateful logic primitives. *)

let new_state system width =
  let cell, _ = create_buf (scope_of_system system) width in
  cell
;;

let new_reg system data_in data_out =
  let scope = scope_of_system system in
  let width = width_of_cell data_in in
  let reg, clk, data = create_ff scope width in
  let rmux, rst, r0, r1 = create_mux scope width in
  let emux, enb, e0, e1 = create_mux scope width in
  let zero = create_const scope (String.make width '0') in
  connect data_in e1;
  connect emux r0;
  connect zero r1;
  connect rmux data;
  connect reg e0;
  connect reg (port_of_cell data_out 0);
  statefuls := Reg (system, clk, rst, enb) :: !statefuls
;;
    

