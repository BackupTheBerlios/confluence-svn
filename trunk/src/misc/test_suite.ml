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

open Ut;;

let test_suite n =
  run_test n "Intbig" Intbig.test;
  run_test n "String2" String2.test;
;;

let run () =
  run_test "" "Top" test_suite;
  report ()
;;

