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


(* Common list functions. *)

let mapi func elements =
  let rec f i sofar elements =
    match elements with
      [] -> sofar
    | element :: elements -> f (i + 1) (func i element :: sofar) elements
  in
  List.rev (f 0 [] elements)
;;

let iteri func elements =
  let rec f i elements =
    match elements with
      [] -> ()
    | element :: elements ->
        func i element;
        f (i + 1) elements
  in
  f 0 elements
;;
        
let rec index element elements num =
  match elements with
    [] -> raise Not_found
  | e :: elements -> if e = element then num else index element elements (num + 1);;

let rec indexq element elements num =
  match elements with
    [] -> raise Not_found
  | e :: elements -> if e == element then num else indexq element elements (num + 1);;

let rec indexf pred elements num =
  match elements with
  | [] -> raise Not_found
  | e :: elements -> if pred e then num else indexf pred elements (num + 1);;

let rec range first last =
  if first = last then
    [last]
  else
    if first < last then
      first :: range (first + 1) last
    else
      first :: range (first - 1) last;;

let rec take l n =
  if n <= 0 then [] else List.hd l :: take (List.tl l) (n - 1);;

let rec drop l n =
  if n <= 0 then l else drop (List.tl l) (n - 1);;

let rec make l element =
  if l <= 0 then [] else element :: make (l - 1) element;;


