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


(** Common string functions. *)

let replace str cOld cNew =
  let s = String.copy str in
  let n = String.length s in
  let rec f i =
    if i != n then
      if s.[i] = cOld then begin
        s.[i] <- cNew;
        f (i + 1);
      end;
  in
  f 0;
  s;;

let take_left str count =
  String.sub str 0 count;;

let take_right str count =
  let l = String.length str in
  String.sub str (l - count) count;;

let drop_left str count =
  let l = String.length str in
  String.sub str count (l - count);;

let drop_right str count =
  let l = String.length str in
  String.sub str 0 (l - count);;

let right_char str =
  str.[String.length str - 1];;

let left_char str =
  str.[0];;

let right_string str =
  drop_left str 1;;

let left_string str =
  drop_right str 1;;

let cons_left char string =
  String.make 1 char ^ string
;;

let cons_right string char =
  string ^ String.make 1 char
;;
  

let replace_meta_chars s =
  let rec f i s1 =
    if i == (String.length s) then
      s1
    else
      match s.[i] with
          '\\' -> f (i + 1) (s1 ^ "\\\\")
        | '"'  -> f (i + 1) (s1 ^ "\\\"")
        | '\n'  -> f (i + 1) (s1 ^ "\\n")
        | '\r'  -> f (i + 1) (s1 ^ "\\r")
        | '\t'  -> f (i + 1) (s1 ^ "\\t")
        | c    -> f (i + 1) (s1 ^ String.make 1 c)
  in
    f 0 "";;

let replace_xml_chars s =
  let rec f i s1 =
    if i == (String.length s) then
      s1
    else
      match s.[i] with
          '<'  -> f (i + 1) (s1 ^ "&lt;")
        | '>'  -> f (i + 1) (s1 ^ "&gt;")
        | '&'  -> f (i + 1) (s1 ^ "&amp;")
        | '"'  -> f (i + 1) (s1 ^ "&quot;")
        | '\'' -> f (i + 1) (s1 ^ "&apos;")
        | c    -> f (i + 1) (s1 ^ String.make 1 c)
  in
    f 0 "";;

let replace_printf_chars s =
  let rec f i s1 =
    if i == (String.length s) then
      s1
    else
      match s.[i] with
          '\\' -> f (i + 1) (s1 ^ "\\\\")
        | '"'  -> f (i + 1) (s1 ^ "\\\"")
        | '\n'  -> f (i + 1) (s1 ^ "\\n")
        | '\r'  -> f (i + 1) (s1 ^ "\\r")
        | '\t'  -> f (i + 1) (s1 ^ "\\t")
        | '%'   -> f (i + 1) (s1 ^ "%%")
        | c    -> f (i + 1) (s1 ^ String.make 1 c)
  in
    f 0 "";;

let insert_meta_chars s char_map =
  let l = String.length s - 1 in
  let rec f i s1 =
    if i == l then  (* Ignores \ if last character. *)
      s1
    else if s.[i] = '\\' then
      try
        f (i + 2) (s1 ^ List.assoc s.[i + 1] char_map)
      with Not_found ->
        f (i + 1) (s1 ^ "\\")   (* If not found, just use \. *)
    else
      f (i + 1) (s1 ^ String.make 1 s.[i])
  in
    f 0 ""
;;

let rec map func str =
  if str = "" then "" else
    String.make 1 (func (left_char str)) ^ map func (right_string str);;

let rec map2 func str1 str2 =
  if str1 = "" then "" else
    String.make 1 (func (left_char str1) (left_char str2)) ^ map2 func (right_string str1) (right_string str2);;
    
(*
let rec fold_left f i str =
  if str = "" then
    i
  else
    fold_left f (f i (left_char str)) (right_string str)
;;
    
let rec fold_right f str i =
  if str = "" then
    i
  else
    fold_right f (f (right_char str) i) (left_string str)
;;
*)

let rec chars_of_string str =
  if str = "" then
    []
  else
    left_char str :: chars_of_string (right_string str)
;;

let rec string_of_chars chars =
  match chars with
  | [] -> ""
  | c :: chars -> String.make 1 c ^ string_of_chars chars
;;


(** [string_of_char char] creates a string from a single [char]. *)
let string_of_char char =
  String.make 1 char
;;

(** [join strings delimiter] joins a list of [strings] together with a [delimiter]. *)
let join strings delimiter =
  let rec join strings =
    match strings with
    | [] -> ""
    | [string] -> string
    | string :: strings -> string ^ delimiter ^ join strings
  in
  join strings
;;

(** [split string delimitiers] splits a [string] delimtied by any character specified in the [delimitiers] string. *)
let split string delimiters =
  let rec on_delimit sofar i =
    if i < 0 then
      sofar
    else
      if String.contains delimiters string.[i] then
        on_delimit sofar (i - 1)
      else
        off_delimit sofar (string_of_char string.[i]) (i - 1)
  and off_delimit sofar word i =
    if i < 0 then
      word :: sofar
    else
      if String.contains delimiters string.[i] then
        on_delimit (word :: sofar) (i - 1)
      else
        off_delimit sofar (string_of_char string.[i] ^ word) (i - 1)
  in
  on_delimit [] (String.length string - 1)
;;

(** [replace_chars_in_string string_of_char string] replaces the characters 
in a [string] defined by a function, [string_of_char]. *)
let replace_chars_in_string string_of_char string =
  let l = String.length string in
  let rec f i sofar =
    if i >= l then
      sofar
    else
      f (i + 1) (sofar ^ string_of_char string.[i])
  in
  f 0 ""
;;

   
    

(* Unit tests. *)

let test n =
  Ut.assert_bool n "replace1"  (replace "abcd" 'a' 'A' = "Abcd");

  Ut.assert_bool n "take_left1" (take_left "abcd" 0 = "");
  Ut.assert_bool n "take_left2" (take_left "abcd" 1 = "a");
  Ut.assert_bool n "take_left3" (take_left "abcd" 2 = "ab");

  Ut.assert_bool n "take_right1" (take_right "abcd" 0 = "");
  Ut.assert_bool n "take_right2" (take_right "abcd" 1 = "d");
  Ut.assert_bool n "take_right3" (take_right "abcd" 2 = "cd");

  Ut.assert_bool n "drop_left1" (drop_left "abcd" 0 = "abcd");
  Ut.assert_bool n "drop_left2" (drop_left "abcd" 1 = "bcd");
  Ut.assert_bool n "drop_left3" (drop_left "abcd" 2 = "cd");

  Ut.assert_bool n "drop_right1" (drop_right "abcd" 0 = "abcd");
  Ut.assert_bool n "drop_right2" (drop_right "abcd" 1 = "abc");
  Ut.assert_bool n "drop_right3" (drop_right "abcd" 2 = "ab");

  Ut.assert_bool n "left_char 1" (left_char "abcd" = 'a');
  Ut.assert_bool n "left_char 2" (left_char "a" = 'a');
  Ut.assert_bool n "right_char 1" (right_char "abcd" = 'd');
  Ut.assert_bool n "right_char 2" (right_char "a" = 'a');
  Ut.assert_bool n "left_string 1" (left_string "abcd" = "abc");
  Ut.assert_bool n "left_string 2" (left_string "a" = "");
  Ut.assert_bool n "right_string 1" (right_string "abcd" = "bcd");
  Ut.assert_bool n "right_string 2" (right_string "a" = "");

  Ut.assert_bool n "replace_meta_chars1" (replace_meta_chars "abc" = "abc");
  Ut.assert_bool n "replace_meta_chars2" (replace_meta_chars "a\nbc" = "a\\nbc");
  Ut.assert_bool n "replace_meta_chars3" (replace_meta_chars "a\nb\\\"c" = "a\\nb\\\\\\\"c");

  Ut.assert_bool n "map 1" (map (fun a -> a) "abc" = "abc");
  Ut.assert_bool n "map 2" (map (fun a -> a) "" = "");

  Ut.assert_bool n "map2 1" (map2 (fun a b -> b) "" "" = "");
  Ut.assert_bool n "map2 2" (map2 (fun a b -> b) "abc" "def" = "def");

  Ut.assert_bool n "chars_of_string 1" (chars_of_string "abcd" = ['a'; 'b'; 'c'; 'd']);
  Ut.assert_bool n "chars_of_string 2" (chars_of_string "" = []);

  Ut.assert_bool n "string_of_chars 1" (string_of_chars ['a'; 'b'; 'c'; 'd'] = "abcd");
  Ut.assert_bool n "string_of_chars 2" (string_of_chars [] = "");

  Ut.assert_bool n "join 1" (join ["a"; "b"; "c"] ", " = "a, b, c");

;;



