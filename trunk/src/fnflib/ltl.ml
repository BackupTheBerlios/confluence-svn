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

(** LTL Properties *)

type ltl = Variable of string
         | True
         | Clock    of ltl
         | Posedge  of string
         | Negedge  of string
         | Not      of ltl
         | And      of ltl * ltl
         | Next     of ltl       (* strong *)
         | Until    of ltl * ltl (* strong *)
;;

let variable name =
  Variable name
;;

let not_ a =
  Not a
;;

let and_ a b =
  And (a, b)
;;

let or_ a b =
  not_ (and_ (not_ a) (not_ b))
;;

let imply a b =
  or_ (not_ a) b
;;

let equivalent a b =
  and_ (imply a b) (imply b a)
;;

let next is_strong a =
  if is_strong then
    Next a
  else
    not_ (Next (not_ a))
;;

let rec until is_strong a b =
  if is_strong then
    Until (a, b)
  else
    or_ (until true a b) (always a)

and eventually a =
  until true True a

and always a =
  not_ (eventually (not_ a))
;;

let never a =
  always (not_ a)
;;

let until_overlap is_strong a b =
  until is_strong a (and_ a b)
;;

let before is_strong a b =
  until is_strong (not_ b) (and_ a (not_ b))
;;

let before_overlap is_strong a b =
  until is_strong (not_ b) a
;;

let rec next_repeat is_strong n a =
  if n < 0 then raise (Invalid_argument "Ltl.next_repeat: Count is less than 0.");
  if n = 0 then
    a
  else
    next_repeat is_strong (n - 1) (next is_strong a)
;;

let rec next_ae is_strong is_forall n0 n1 a =
  if n0 > n1 then raise (Invalid_argument "Ltl.next_ae: First count is greater than second.");
  if n0 = n1 then
    next_repeat is_strong n0 a
  else
    (if is_forall then and_ else or_) (next_repeat is_strong n0 a) (next_ae is_strong is_forall (n0 + 1) n1 a)
;;

let next_event is_strong a b =
  until is_strong (not_ a) (and_ a b)
;;

let rec next_event_repeat is_strong n a b =
  if n < 1 then raise (Invalid_argument "Ltl.next_event_repeat: Count is less than 1.");
  if n = 1 then
    next_event is_strong a b
  else
    next_event is_strong a (next is_strong (next_event_repeat is_strong (n - 1) a b))
;;

let rec next_event_ae is_strong is_forall n0 n1 a b =
  if n0 > n1 then raise (Invalid_argument "Ltl.next_event_ae: First count is greater than second.");
  if n0 = n1 then
    next_event_repeat is_strong n0 a b
  else
    (if is_forall then and_ else or_) (next_event_repeat is_strong n0 a b) (next_event_ae is_strong is_forall (n0 + 1) n1 a b)
;;

let rec clock clk a =
  match a with
  | Variable _
  | True
  | Clock    _
  | Posedge  _
  | Negedge  _   -> a
  | Not   a      -> not_ (clock clk a)
  | And   (a, b) -> and_ (clock clk a) (clock clk b)
  | Next  a      -> until true (not_ clk) (and_ clk (next true (until true (not_ clk) (and_ clk (clock clk a)))))
  | Until (a, b) -> until true (imply clk (clock clk a)) (and_ clk (clock clk b))
;;

let posedge clk a =
  Clock (clock (Posedge clk) a)
;;

let negedge clk a =
  Clock (clock (Negedge clk) a)
;;

let nusmv_of_ltl ltl name_of_signal name_of_clock =
  let rec nusmv_of_ltl ltl =
    match ltl with
    | Variable name -> name_of_signal name
    | True -> "1"
    | Posedge clock -> name_of_clock clock true
    | Negedge clock -> name_of_clock clock false
    | Clock   a -> nusmv_of_ltl a
    | Not a -> "(! " ^ nusmv_of_ltl a ^ ")"
    | And (a, b) -> "(" ^ nusmv_of_ltl a ^ " & " ^ nusmv_of_ltl b ^ ")"
    | Next a     -> "(X " ^ nusmv_of_ltl a ^ ")"
    | Until (a, b) -> "(" ^ nusmv_of_ltl a ^ " U " ^ nusmv_of_ltl b ^ ")"
  in
  "LTLSPEC " ^ nusmv_of_ltl ltl ^ ";"
;;





