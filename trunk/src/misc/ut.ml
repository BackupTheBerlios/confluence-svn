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

(** Unit testing and assertions. *)
                                                                                                                                                         
let pass = ref 0;;
                                                                                                                                                         
let fail = ref 0;;
                                                                                                                                                         
(** Asserts that a boolean expression is true. *)
let assert_bool test_name assert_name test =
  if not test then begin
    incr fail;
    print_string ("** Assertion Failure: " ^ test_name ^ "/" ^ assert_name);
    print_newline ()
  end else
    incr pass
;;
                                                                                                                                                         
(** Runs a test given the parent test name, the sub test name, and the test function. *)
let run_test parent_name test_name test =
  test (parent_name ^ "/" ^ test_name)
;;
                                                                                                                                                         
(** Reports the test results and resets the test counters. *)
let report () =
  print_string ("Total Assertions: " ^ string_of_int (!pass + !fail) ^ "  Passed: " ^ string_of_int !pass ^ "  Failed: " ^ string_of_int !fail);
  print_newline ();
  pass := 0;
  fail := 0
;;
                                                                                                                                                         
let print_msg msg =
  print_string msg;
  print_newline ()
;;

