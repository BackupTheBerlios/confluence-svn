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


(** Stateful Parser Data *)

let to_parse = Queue.create ();; (* Fifo of file names to parse. *)
let asts = Hashtbl.create 8;;    (* File name => ast. *)
let subs = Hashtbl.create 8;;    (* File name => subenvs. *)
let top_files = ref [];;
let (files : string list ref) = ref [];;

let base_env = ref "/usr/lib/confluence/base.cf";;

let set_base_env file =
  base_env := file
;;

(* Position information. *)
let parent           = ref "";;
let error_file       = ref "";;
let error_line       = ref 0;;
let error_col        = ref 0;;
let current_file     = ref "";;
let current_line     = ref 0;;
let current_col      = ref 0;;
let current_token    = ref "";;

(* Clear stateful information. *)
let clear_stateful () =
  Hashtbl.clear asts;
  Hashtbl.clear subs;
  top_files := [];
  files := [];
;;


(* Parsing error reporting. *)

let error msg =
  Report.fatal (
    "** Syntax Error\n\n" ^
    "  May Be Near --> " ^ Loc.toString (Loc.create !error_file !error_line !error_col) ^ "\n\n" ^ msg);;


(* Main parser helpers. *)

let format_name new_name =
  let format_path () =
    try
      let i = String.rindex !current_file '/' in
        String.sub !current_file 0 (i + 1)
    with Not_found -> ""
  in
  if new_name = "" then error "File reference is an empty string.";
  if new_name.[0] = '/' then
    new_name
  else if new_name.[0] = '$' then begin
    let (env, path) = 
      try
        let i = String.index new_name '/' in
        (String.sub new_name 1 (i - 1), String.sub new_name i (String.length new_name - i))
      with Not_found ->
        (String.sub new_name 1 (String.length new_name - 1), "")
    in
    try
      let env_val = Sys.getenv env in
      if String.length env_val = 0 || env_val.[0] <> '/' then
        error ("Environment variable is not an absolute path: " ^ env ^ " = " ^ env_val);
      env_val ^ path
    with Not_found ->
      error ("Environment variable not found: " ^ env);
      ""
  end else
    format_path () ^ new_name
;;

let set_sub_env file =
  if !parent = "" then
    top_files := file :: !top_files
  else begin
    try
      Hashtbl.replace subs !parent (file :: (Hashtbl.find subs !parent))
    with Not_found ->
      Hashtbl.add subs !parent [file]
  end;;

let rec parse_all_files parse_channel =
  if not (Queue.is_empty to_parse) then
    let file = Queue.take to_parse in
    if not (Hashtbl.mem asts file) then begin
      files        := file :: !files;
      parent       := "";
      current_file := file;
      current_line := 1;
      current_col  := 1;
      error_file   := file;
      error_line   := 1;
      error_col    := 1;
      try
        let channel = open_in file in
        Hashtbl.add asts file (parse_channel channel);
        close_in channel;
        set_sub_env file
      with
        Sys_error _ -> error ("File not found: " ^ file)
    end;
    parse_all_files parse_channel;
;;

let rec insert_sub_asts ast_apply files =
  match files with
    [] -> ast_apply
  | file :: files ->
      insert_sub_asts (CfAst.add_sub_env ast_apply (build_sub_ast file) ("_file_" ^ file)) files

and build_sub_ast file =
  let ast = Hashtbl.find asts file
  in
    let subFiles =
      try
        Hashtbl.find subs file
      with Not_found -> []
    in
      insert_sub_asts ast subFiles;;

let rec build_file_names files =
  match files with
    [] -> []
  | file :: files -> ("_file_" ^ file) :: build_file_names files;;
  
let free_arg_list loc names =
  List2.make (List.length names) (CfAst.Free loc)
;;

(** Main parser. *)
let parse_program main_file lexer parser =
  let parse_channel channel =
    parser lexer (Lexing.from_channel channel)
  in
  let loc = Loc.create main_file 0 0 in
  Queue.add main_file to_parse;
  parse_all_files parse_channel;
  let file_names = build_file_names !files in
  let main_ast = insert_sub_asts (CfAst.Apply (loc, "", CfAst.Comp (loc, "", file_names, []), free_arg_list loc file_names)) !top_files in
  clear_stateful ();
  main_ast
;;



(* Misc Parser and Lexer functions. *)

let current_file_loc () =
  Loc.create !current_file 0 0;;

let set_env env_file =
  if env_file = "" then begin
    parent := !base_env;
    Queue.add !base_env to_parse;
  end else begin
    let new_file = format_name env_file in
    parent := new_file;
    Queue.add new_file to_parse;
  end;;

let add_import new_import =
  let new_file = format_name new_import in
  Queue.add new_file to_parse;
  "_file_" ^ new_file;;

let update_positions c =
  if c == '\n' then
    begin
      current_line := !current_line + 1;
      current_col  := 1
    end
  else
    current_col  := !current_col  + 1;;

let next_lex lexbuf =
  error_file := !current_file;
  error_line := !current_line;
  error_col  := !current_col;
  let loc = Loc.create !current_file !current_line !current_col
  in
    let token = Lexing.lexeme lexbuf in
      current_token := token;
      String.iter update_positions token;
      (loc, token);;

let get_current_token () =
  !current_token;;


(* Parser Transformations *)

(** Transforms a namespace in an expression. *)
let application_expr_of_namespace loc annotation names stmts =
  CfAst.Apply (loc, annotation, CfAst.Comp (loc, annotation, names, stmts), free_arg_list loc names)
;;

(** Transforms a namespace in a statement. *)
let application_of_namespace loc annotation names stmts =
  CfAst.ApplyStmt (application_expr_of_namespace loc annotation names stmts)
;;

(** Selects position from system or record expression. *)
let select_position loc expr position =
  CfAst.DotPosition (loc, expr, Intbig.intbig_of_int position)
;;

(** Connects a name to an expression. *)
let connect_name loc (name_loc, name) expr =
  CfAst.Connect (loc, CfAst.Name (name_loc, name), expr)
;;

(** Creates a statement from a named component. *)
let stmt_of_comp_named comp_loc (name_loc, name) ports stmts =
  CfAst.ConnectStmt (connect_name comp_loc (name_loc, name) (CfAst.Comp (comp_loc, name, ports, stmts)))
;;

let application_of_prefix (op_loc, op_name) a =
  select_position op_loc (CfAst.Apply (op_loc, "", CfAst.Name (op_loc, "(" ^ op_name ^ ")"), [a; CfAst.Free op_loc])) 2
;;

let application_of_infix (op_loc, op_name) a b =
  select_position op_loc (CfAst.Apply (op_loc, "", CfAst.Name (op_loc, "(" ^ op_name ^ ")"), [a; b; CfAst.Free op_loc])) 3
;;

let application_of_trifix (op_loc, op_name) a b c =
  select_position op_loc (CfAst.Apply (op_loc, "", CfAst.Name (op_loc, "(" ^ op_name ^ ")"), [a; b; c; CfAst.Free op_loc])) 4
;;

let conditional_of_ifelse loc p ts fs =
  (* _ <- p ? {comp ts end} : {comp fs end} *)
  CfAst.ConnectStmt (CfAst.Connect (loc, CfAst.Free loc, (CfAst.Cond
    (loc,
    p,
    (CfAst.Apply (loc, "", CfAst.Comp (loc, "", [], ts), [])),
    (CfAst.Apply (loc, "", CfAst.Comp (loc, "", [], fs), []))
    ))));;
  
let format_string  s =
  (*
  Remove quotes.
  Convert \ charaters.
  *)
  let rec f i s1 =
    if i == (String.length s) - 1 then
      s1
    else
      if String.get s i == '\\' then
        match String.get s (i + 1) with
          '\\' -> f (i + 2) (s1 ^ "\\")
        | '"'  -> f (i + 2) (s1 ^ "\"")
        | 'n'  -> f (i + 2) (s1 ^ "\n")
        | 'r'  -> f (i + 2) (s1 ^ "\r")
        | 't'  -> f (i + 2) (s1 ^ "\t")
        | _    -> begin error "Invalid String"; "" end 
      else
        f (i + 1) (s1 ^ String.make 1 (String.get s i))
  in
    f 1 "";;

let rec application_of_string loc s =
  (* "abc" -> {`:: a {`:: b {`:: c [] $} $} $} *)
    if String.length s == 0 then
      CfAst.Record (loc, [])
    else
      select_position loc (CfAst.Apply (loc, "", CfAst.Name (loc, "(::)"),
        [CfAst.Integer (loc, Intbig.intbig_of_char (String.get s 0));
         application_of_string loc (String.sub s 1 (String.length s - 1));
         CfAst.Free loc]
      )) 3
;;

