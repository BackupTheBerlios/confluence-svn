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

let help () =
  print_string ("
  NAME
      fnf

  SYNOPSIS
      fnf [options]

  DESCRIPTION
      FNF (Free Netlist Format) is an elaborated, hierarchical, register transfer level (RTL)
      netlist format used to communicate design information between frontend EDA tools.

      The FNF tool translates an FNF netlist to Verilog, VHDL, C, and NuSMV.

  OPTIONS
      Options are processed in the order they are received.

      -h OR -help
        Prints this information then exits.

      -read_fnf file
        Read in an FNF netlist.

      -write_fnf file
        Write out an FNF netlist.

      -write_nusmv file
        Write out an NuSMV description.

      -write_verilog file
        Write out a Verilog netlist.

      -write_vhdl file
        Write out a VHDL netlist.

      -write_c file
        Write out a C model.  Appends '.c' and '.h' to file name.

      -write_jhdl class
        Write out a JHDL netlist.  Appends .java to class name.

  EXAMPLES
    Building an FNF netlist from Verilog using Icarus:

      $ iverilog -Wall -t fnf -o my_netlist.fnf my_verilog.v

    Use FNF to produce a Verilog and C model:

      $ fnf -read_fnf my_netlist.fnf -write_verilog my_netlist.v -write_c my_netlist

    Use FNF to produce an NuSMV model:

      $ fnf -read_fnf my_netlist.fnf -write_nusmv my_netlist.smv

    Use FNF to produce another FNF netlist:

      $ fnf -read_fnf my_netlist.fnf -write_fnf my_netlist2.fnf

  KNOWN LIMITATIONS
      General
        - No tristate support.
        - No memory support.
        - No division or modulo operators.

      Icarus Verilog FNF Code Generator
        - Assumes ports and named signals have [n:0] ordering.
        - \"always\" blocks are constrained to the Icarus Verilog synthesizable subset.
        - All register clocks and asynchronous resets must be senitive on the rising edge.
          WARNING: No errors will be issued if a design contains \"negedge\".
        - All arithmetic operations must be unsigned.
          WARNING: No errors will be issued if a design contains signed operations.
        - Multipliers can not be embbeded in concatenations.

      Verilog and VHDL Model Writer
        - Netlist is flat.

      NuSMV Model Writer
        - No multiplier support.
        - 2-value model.  No X's or Z's.
        - Inputs assumed to init to 0.
        - Registers are initialized to 0.

  VERSION
      " ^ Version.version ^ "

  AUTHOR
      Tom Hawkins

  SEE ALSO
      FNF and Confluence : http://www.confluent.org/
      Icarus Verilog     : http://www.icarus.com/eda/verilog/
      NuSMV              : http://nusmv.irst.itc.it/

  COPYRIGHT
      Copyright (C) 2004-2005 Tom Hawkins

");
  print_newline ();
  exit 0
;;


type current_scope = None | Scope of Fnf_core.scope;;
    

let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if  argc = 1 then help ();

  let i = ref 1 in

  let error msg =
    prerr_string ("** ERROR:  " ^ msg);
    prerr_newline ();
    exit 1
  in

  let debug msg =
    print_string msg;
    print_newline ()
  in

  let open_out filename =
    try
      open_out filename
    with Sys_error _ ->
      error ("Could not open file " ^ filename ^ " for writing.");
      raise (Invalid_argument "no file")
  in

  let open_in filename =
    try
      open_in filename
    with Sys_error _ ->
      error ("Could not open file " ^ filename ^ " for reading.");
      raise (Invalid_argument "no file")
  in

  let next_arg arg_type =
    if !i >= argc then error ("Invalid Argument.  Expecting " ^ arg_type ^ ".");
    let arg = argv.(!i) in
    incr i;
    arg
  in

  let scope = ref None in

  let get_scope () =
    match !scope with
    | None -> error ("No netlist currently loaded.")
    | Scope scope -> scope
  in

  while !i < argc do
    match next_arg "command option" with
    | "-h" | "-help"    -> help () 
 
    | "-read_fnf"  ->
      let file = next_arg "write_fnf file" in
      let channel = open_in file in
      scope := Scope (try Parser_util.parse_channel channel file Fnf_lexer.token Fnf_parser.netlist with Parser_util.Error msg -> error msg);
      close_in channel

    | "-write_fnf" ->
      let file = next_arg "write_fnf file" in
      let channel = open_out file in
      Fnf_out.output_fnf channel (get_scope ());
      close_out channel

    | "-write_verilog" ->
      let file = next_arg "verilog file" in
      let channel = open_out file in
      Fnf_verilog.output_verilog channel (get_scope ());
      close_out channel

    | "-write_vhdl" ->
      let file = next_arg "vhdl file" in
      let channel = open_out file in
      Fnf_vhdl.output_vhdl channel (get_scope ());
      close_out channel

    | "-write_jhdl" ->
      let file = next_arg "jhdl file" in
      let channel = open_out (file ^ ".java") in
      Fnf_jhdl.output_jhdl file channel (get_scope ());
      close_out channel

    | "-write_nusmv" ->
      let file = next_arg "nusmv file" in
      let channel = open_out file in
      Fnf_nusmv.output_nusmv channel (get_scope ());
      close_out channel

    | "-write_c" ->
      let file = next_arg "nusmv file" in
      let channel_c = open_out (file ^ ".c") in
      let channel_h = open_out (file ^ ".h") in
      Fnf_c.output_c channel_c channel_h (get_scope ());
      close_out channel_c;
      close_out channel_h

    | arg -> error ("Invalid argument: " ^ arg)

  done
;;

Printexc.print main ();;
  
