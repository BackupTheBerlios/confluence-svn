(*
    Confluence System Design Language Compiler
    Copyright (C) 2003-2004 Tom Hawkins (tomahawkins@yahoo.com)

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


(** Intbig: arbitrary-precision integers. *)

open Big_int;;

type intbig = big_int;;


(** Constants. *)

let zero = zero_big_int;;
let one  = unit_big_int;;
let two  = add_big_int one one;;
let ones = minus_big_int one;;
let v16  = big_int_of_string "16";;


(** Arithmetics. *)

let abs  = abs_big_int;;
let neg  = minus_big_int;;
let add  = add_big_int;;
let sub  = sub_big_int;;
let mul  = mult_big_int;;
let div  = div_big_int;;
let modu = mod_big_int;;

let pow a b =
  if lt_big_int b zero then raise (Invalid_argument "Intbig.pow: Right operand less than 0.");
  power_big_int_positive_big_int a b;;


(** Comparisons. *)

let sign    = sign_big_int;;
let compare = compare_big_int;;
let eq      = eq_big_int;;
let ne a b  = not (eq a b);;
let le      = le_big_int;;
let ge      = ge_big_int;;
let lt      = lt_big_int;;
let gt      = gt_big_int;;


(** Shifting. *)

let shift_left value shift =
  if lt shift zero then raise (Invalid_argument "Intbig.shift_left: Right operand less than 0.");
  mul value (pow two shift);;

let shift_right value shift =
  if lt shift zero then raise (Invalid_argument "Intbig.shift_right: Right operand less than 0.");
  div value (pow two shift);;


(** Bitwise logics. *)

let bw_not a =
  sub (neg a) one;;

let rec bw_and a b =
  if eq a zero && eq b zero then zero else
  if eq a zero && eq b ones then zero else
  if eq a ones && eq b zero then zero else
  if eq a ones && eq b ones then ones else
  let (aq, ar) = quomod_big_int a two in
  let (bq, br) = quomod_big_int b two in
  add (mul (bw_and aq bq) two) (div (add ar br) two);;

let bw_or a b =
  bw_not (bw_and (bw_not a) (bw_not b));;
  
let bw_xor a b =
  bw_or (bw_and a (bw_not b)) (bw_and (bw_not a) b);;
  

(** String conversion. *)

let rec binary_string_of_intbig1 value width sofar =
  if (le width zero) then sofar else
    binary_string_of_intbig1 (div value two) (sub width one) ((if (eq (modu value two) one) then "1" else "0") ^ sofar);;

let binary_string_of_intbig value width =
  binary_string_of_intbig1 value width "";;
  
let rec intbig_of_binary_string1 str factor sofar =
  if str = "" then sofar else
    intbig_of_binary_string1 (String2.left_string str) (mul factor two) (if String2.right_char str = '0' then sofar else (add sofar factor));;

let rec intbig_of_binary_string str =
  intbig_of_binary_string1 str one zero;;
    
let hexCharToInt hexChar =
  match hexChar with
    '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' | 'a' -> 10
  | 'B' | 'b' -> 11
  | 'C' | 'c' -> 12
  | 'D' | 'd' -> 13
  | 'E' | 'e' -> 14
  | 'F' | 'f' -> 15
  | _ -> raise (Invalid_argument "Intbig.intbig_of_string: Invalid hex character.");;

let rec hexToIntbig hexStr base value =
  if hexStr = "" then value else
    hexToIntbig (String2.left_string hexStr) (mul base v16) (add (mul (big_int_of_int (hexCharToInt (String2.right_char hexStr))) base) value);;

let string_of_intbig = string_of_big_int;;

let intbig_of_string str =
  if String.length str >= 2 then
    match String2.take_left str 2 with
      "0x" -> hexToIntbig (String2.drop_left str 2) one zero
    | "1x" -> 
        let s = String2.drop_left str 2 in
        let l = String.length s in
        let v = hexToIntbig s one zero in
        let m = hexToIntbig (String.make l 'F') one zero in
        bw_or (bw_not m) v
    | "0b" -> intbig_of_binary_string (String2.drop_left str 2)
    | "1b" -> 
        let s = String2.drop_left str 2 in
        let l = String.length s in
        let v = intbig_of_binary_string s in
        let m = intbig_of_binary_string (String.make l '1') in
        bw_or (bw_not m) v
    | _    -> big_int_of_string str
  else
    big_int_of_string str;;


(** Numeric type conversion. *)

let int_of_intbig    = int_of_big_int;;
let intbig_of_int    = big_int_of_int;;

let char_of_intbig i =
  try
    char_of_int (int_of_intbig i)
  with
    Invalid_argument _ | Failure _ -> raise (Invalid_argument "Intbig.char_of_intbig: Invalid ascii character.");;

let intbig_of_char c =
  intbig_of_int (int_of_char c);;

let float_of_intbig  = float_of_big_int;;

let intbig_of_float a =
  intbig_of_int (int_of_float a);; (* XXX Not precise. *)


(** Unit tests. *)

let test n =
  Ut.assert_bool n "zero" (eq zero (intbig_of_string "0"));
  Ut.assert_bool n "one"  (eq one  (intbig_of_string "1"));
  Ut.assert_bool n "ones" (eq ones (intbig_of_string "-1"));
  Ut.assert_bool n "v16 " (eq v16  (intbig_of_string "16"));
  Ut.assert_bool n "abs1" (eq zero (abs zero));
  Ut.assert_bool n "abs2" (eq one  (abs ones));
  Ut.assert_bool n "neg1" (eq one  (neg ones));
  Ut.assert_bool n "neg2" (eq ones (neg one));
  Ut.assert_bool n "pow1" (eq (mul two two) (pow (neg two) two));
  Ut.assert_bool n "ne1"  (ne one two);
  Ut.assert_bool n "le1"  (le one one);
  Ut.assert_bool n "le2"  (le one two);
  Ut.assert_bool n "le3"  (not (le two one));
  Ut.assert_bool n "ge1"  (ge one one);
  Ut.assert_bool n "ge2"  (ge two one);
  Ut.assert_bool n "ge3"  (not (ge one two));

  Ut.assert_bool n "shift_left1" (eq (shift_left one zero) one);
  Ut.assert_bool n "shift_left2" (eq (shift_left one two) (intbig_of_string "4"));
  Ut.assert_bool n "shift_left3" (eq (shift_left ones two) (intbig_of_string "-4"));

  Ut.assert_bool n "shift_right1" (eq (shift_right one zero) one);
  Ut.assert_bool n "shift_right2" (eq (shift_right one one) (intbig_of_string "0"));
  Ut.assert_bool n "shift_right3" (eq (shift_right (intbig_of_string "-4") one) (neg two));
  Ut.assert_bool n "shift_right4" (eq (shift_right (intbig_of_string "-4") two) ones);

  Ut.assert_bool n "not1" (eq (bw_not one) (intbig_of_string "-2"));
  Ut.assert_bool n "not2" (eq (bw_not zero) ones);
  Ut.assert_bool n "not3" (eq (bw_not (intbig_of_string "0xF0")) (intbig_of_string "1x0F"));

  Ut.assert_bool n "of_string1" (eq (intbig_of_string "0xF0") (intbig_of_string "240"));
  Ut.assert_bool n "of_string2" (eq (intbig_of_string "0x00") (intbig_of_string "0"));
  Ut.assert_bool n "of_string3" (eq (intbig_of_string "1xF") (intbig_of_string "-1"));
  Ut.assert_bool n "of_string4" (eq (intbig_of_string "1xE") (intbig_of_string "-2"));
  Ut.assert_bool n "of_string5" (eq (intbig_of_string "1x0F") (intbig_of_string "-241"));
  Ut.assert_bool n "of_string6" (eq (intbig_of_string "0b10") two);
  Ut.assert_bool n "of_string7" (eq (intbig_of_string "0b00") zero);
  Ut.assert_bool n "of_string8" (eq (intbig_of_string "1b0") (intbig_of_string "-2"));
  Ut.assert_bool n "of_string9" (eq (intbig_of_string "1b1") (intbig_of_string "-1"));

  Ut.assert_bool n "binary_string_of1" (binary_string_of_intbig (intbig_of_string "0") (intbig_of_string "0") = "");
  Ut.assert_bool n "binary_string_of2" (binary_string_of_intbig (intbig_of_string "0") (intbig_of_string "1") = "0");
  Ut.assert_bool n "binary_string_of3" (binary_string_of_intbig (intbig_of_string "0") (intbig_of_string "8") = "00000000");
  Ut.assert_bool n "binary_string_of4" (binary_string_of_intbig (intbig_of_string "-1") (intbig_of_string "0") = "");
  Ut.assert_bool n "binary_string_of5" (binary_string_of_intbig (intbig_of_string "-1") (intbig_of_string "1") = "1");
  Ut.assert_bool n "binary_string_of6" (binary_string_of_intbig (intbig_of_string "-1") (intbig_of_string "8") = "11111111");
  Ut.assert_bool n "binary_string_of7" (binary_string_of_intbig (intbig_of_string "0x35") (intbig_of_string "8") = "00110101");

  Ut.assert_bool n "of_binary_string1" (eq (intbig_of_binary_string ("0")) (intbig_of_string "0"));
  Ut.assert_bool n "of_binary_string2" (eq (intbig_of_binary_string ("1")) (intbig_of_string "1"));
  Ut.assert_bool n "of_binary_string3" (eq (intbig_of_binary_string ("0000")) (intbig_of_string "0"));
  Ut.assert_bool n "of_binary_string4" (eq (intbig_of_binary_string ("0101")) (intbig_of_string "5"));
  Ut.assert_bool n "of_binary_string5" (eq (intbig_of_binary_string ("1111")) (intbig_of_string "15"));

  Ut.assert_bool n "and1" (eq (bw_and (intbig_of_string "12") (intbig_of_string "10")) (intbig_of_string "8"));
  Ut.assert_bool n "or1"  (eq (bw_or  (intbig_of_string "12") (intbig_of_string "10")) (intbig_of_string "14"));
  Ut.assert_bool n "xor1" (eq (bw_xor (intbig_of_string "12") (intbig_of_string "10")) (intbig_of_string "6"));
;;



