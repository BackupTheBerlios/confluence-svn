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

let help () =
  print_string ("
  NAME
      cf - Confluence compiler

  SYNOPSIS
      cf [options] [file] [arguments]

  DESCRIPTION
      Confluence is a functional programming language for reactive system design, including
      digital logic systems (ASIC, FPGA) and control oriented hard real-time software.

      The Confluence compiler (cf) translates a Confluence description into
      Verilog and VHDL (for digital logic synthesis), C (for simulation and
      software targets), and NuSMV (for formal verification, via model checking).

  OPTIONS
      -h OR -help
        Prints this information then exits.

      -b filename OR -base_env filename
        Sets the base environment for the compilation.  Overrides the CF_ENV
        environment variable.  If CF_ENV not defined and -base_env option not
        set, the base environment defaults to /usr/lib/confluence/base.cf.

      -e integer OR -error_limit integer
        Sets the maximum number of reported errors.  A negative number
        reports all errors.

      -c OR -compile_only
        Parses and compiles, but does not evaluate a program.

      -o name
        Sets the output file name.  Default is out.fnf.

      -test
        Run the built in unit tests.

  ENVIRONMENT VARIABLES
      CF_ENV
        A filename that specifies the base environment (commonly base.cf).

      CF_LIB
        The directory location of the Confluence standard library (optional).

  VERSION
      " ^ Version.version ^ "

  AUTHOR
      Tom Hawkins (tomahawkins@yahoo.com)

  SEE ALSO
      http://www.confluent.org/

  COPYRIGHT
      Copyright (C) 2003-2005 Tom Hawkins

");
  print_newline ();
  exit 0
;;
    

let parse_cmd_args () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc = 1 then help ();
  let parsing = ref true in
  let i = ref 0 in

  let base_env = ref "" in
  let compile_only = ref false in
  let program = ref "" in
  let output = ref "out.fnf" in

  while !parsing do
    incr i;
    if argc = !i then Report.fatal ("** Invalid Argument: Expecting program filename.");
    match argv.(!i) with
    | "-h"
    | "-help" -> help ()

    | "-b"
    | "-base_env" ->
        if argc = !i + 1 then Report.fatal ("** Invalid Argument: Expecting base environment filename.");
        base_env := argv.(!i + 1);
        incr i

    | "-c"
    | "-compile_only" -> compile_only := true

    | "-e"
    | "-error_limit" ->
        if argc = !i + 1 then Report.fatal ("** Invalid Argument: Expecting error limit number.");
        (try
          Report.set_error_limit (int_of_string argv.(!i + 1))
        with _ -> Report.fatal ("** Invalid Argument: Expecting integer error limit."));
        incr i

    | "-o" ->
        if argc = !i + 1 then Report.fatal ("** Invalid Argument: Expecting output filename.");
        output := argv.(!i + 1);
        incr i

    | "-test" ->
        Test_suite.run ();
        exit 0

    | file ->
        program := file;
        parsing := false
  done;
  incr i;
  CfPrims.set_argv_position !i;
  if !base_env = "" then base_env := (try Sys.getenv "CF_ENV" with Not_found -> "/usr/lib/confluence/base.cf");
  CfParserUtil.set_base_env !base_env;
  !program, !compile_only, !output
;;

let open_out filename =
  try
    open_out filename
  with Sys_error _ ->
    Report.fatal ("** Error: Could not open file " ^ filename ^ " for writing.");
    raise (Invalid_argument "no file")
;;

let main () =

  let (program, compile_only, output_name) = parse_cmd_args () in

  let ast = CfParserUtil.parse_program program CfLexer.token CfParser.file in
  let task = CfCompiler.compileApplication ast in

  if not compile_only then begin
    CfTypes.readyTask task;
    CfTypes.executeTasks ();

    CfTypes.checkAllPortsDetermined ();

    if Report.errorReported () then Report.fatal "Errors reported.  Exiting.";

    let channel = open_out output_name in
    Cf_fnf.output_fnf channel;
    close_out channel
  end;

  exit 0
;;

Printexc.print main ();;

