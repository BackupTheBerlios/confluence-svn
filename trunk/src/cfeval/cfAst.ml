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


exception CfAstError;;

type expr     = Apply       of Loc.loc * expr * expr list        (* location, comp, arg list. *)
              | Connect     of Loc.loc * expr * expr             (* location, sink expr, source expr. *)
              | Cond        of Loc.loc * expr * expr * expr                    (* predicate, true, false *)
              | Name        of Loc.loc * string
              | Comp        of Loc.loc * string list * stmt list         (* ports, statements *)
              | Prim        of Loc.loc * string * string list            (* primitive name, ports *)
              | DotName     of Loc.loc * expr * string
              | DotPosition of Loc.loc * expr * Intbig.intbig
              | Integer     of Loc.loc * Intbig.intbig
              | Float       of Loc.loc * float
              | Boolean     of Loc.loc * bool
              | Vector      of Loc.loc * string
              | Record      of Loc.loc * (string * expr) list
              | Free        of Loc.loc

and  stmt     = ApplyStmt   of expr
              | ConnectStmt of expr
;;

let expr_loc expr =
  match expr with
    Apply       (loc, _, _)
  | Connect     (loc, _, _)
  | Cond        (loc, _, _, _)
  | Name        (loc, _)
  | Comp        (loc, _, _)
  | Prim        (loc, _, _)
  | DotName     (loc, _, _)
  | DotPosition (loc, _, _)
  | Integer     (loc, _)
  | Float       (loc, _)
  | Boolean     (loc, _)
  | Vector      (loc, _)
  | Record      (loc, _)
  | Free        loc            -> loc
;;

let stmt_loc stmt =
  match stmt with
    ApplyStmt   apply   -> expr_loc apply
  | ConnectStmt connect -> expr_loc connect
;;


let add_sub_env apply_parent apply_sub sub_name =
  let loc = expr_loc apply_sub in
  match apply_parent with
    Apply (app_loc, Comp (comp_loc, ports, l), free_args) ->
      Apply (app_loc, Comp (comp_loc, ports, (l @ [ConnectStmt (Connect (loc, Name (loc, sub_name), apply_sub))])), free_args)
  | _ -> Report.fatal "Invalid file application."; raise CfAstError;;


(* Apply Ast to Channel. *)

let write_channel = ref stdout;;

let write str =
  output_string !write_channel str;;

let id i =
  i ^ "  ";;

let rec write_exprs exprs i =
  match exprs with
    [] -> ()
  | expr :: exprs ->
      write_expr expr i;
      write (i ^ ";\n");
      write_exprs exprs i

and writeStringList names =
  match names with
    [] -> write ("[]")
  | name :: names ->
      write ("\"" ^ String2.replace_meta_chars name ^ "\" :: ");
      writeStringList names

and write_expr a i =
  match a with
    Apply (l, e, ports) ->
      write (i ^ "CfAst.Apply (\n");
      write_loc l (id i); write (",\n");
      write_expr e (id i);
      write (id i ^ ",\n");
      write (id i ^ "[\n");
      write_exprs ports (id i);
      write (id i ^ "],\n");
      write (i ^ ")\n")

  | Connect (l, e1, e2) ->
      write (i ^ "CfAst.Connect (");
      write_loc l (id i); write (",\n");
      write_expr e1 (id i);
      write ((id i) ^ ",\n");
      write_expr e2 (id i);
      write (i ^ ")\n")

  | Cond (l, p, t, f) ->
      write (i ^ "CfAst.Cond (\n");
      write_loc l (id i); write (",\n");
      write_expr p (id i);
      write ((id i) ^ ",\n");
      write_expr t (id i);
      write ((id i) ^ ",\n");
      write_expr f (id i);
      write (i ^ ")\n")

  | Name (l, b) ->
      write (i ^ "CfAst.Name (\n");
      write_loc l (id i); write (",\n");
      write ((id i) ^ "\"" ^ String2.replace_meta_chars b ^ "\"\n");
      write (i ^ ")\n")

  | Comp (l, ports, s) ->
      write (i ^ "CfAst.Comp (\n");
      write_loc l (id i); write (",\n");
      write (id i); writeStringList ports;  write (",\n");
      write (id i ^ "[\n");
      write_stmts s (id i);
      write (id i ^ "]\n");
      write (i ^ ")\n")

  | Prim (l, n, ports) ->
      write (i ^ "CfAst.Prim (\n");
      write_loc l (id i); write (",\n");
      write ((id i) ^ "\"" ^ n ^ "\",\n");
      write (id i); writeStringList ports;  write (",\n");
      write (i ^ ")\n")

  | DotName (l, e1, name) ->
      write (i ^ "CfAst.DotName (\n");
      write_loc l (id i); write (",\n");
      write_expr e1 (id i);
      write (id i ^ ",\n");
      write (id i ^ "\"" ^ name ^ "\"\n");
      write (i ^ ")\n")

  | DotPosition (l, e1, position) ->
      write (i ^ "CfAst.DotPosition (\n");
      write_loc l (id i); write (",\n");
      write_expr e1 (id i);
      write (id i ^ ",\n");
      write (id i ^ Intbig.string_of_intbig position ^ "\n");
      write (i ^ ")\n")

  | Integer (l, n) ->
      write (i ^ "CfAst.Integer (");
      write_loc l "";
      write (", Intbig.intbig_of_string \"" ^ Intbig.string_of_intbig n ^ "\")\n")
   
  | Float (l, n) ->
      write (i ^ "CfAst.Float (");
      write_loc l "";
      write (", " ^ string_of_float n ^ ")\n")
   
  | Boolean (l, b) ->
      write (i ^ "CfAst.Boolean (");
      write_loc l "";
      write (", " ^ string_of_bool b ^ ")\n")
   
  | Vector (l, s) ->
      write (i ^ "CfAst.Vector (");
      write_loc l "";
      write (", \"" ^ s ^ "\")\n")

  | Record (l, fields) ->
      write (i ^ "CfAst.Record (");
      write_loc l "";
      List.iter (function (label, value) ->
        write (i ^ "  " ^ label ^ " :\n"); 
        write_expr value (i ^ "  ")
      ) fields;
      write (i ^ ")\n")

  | Free l ->
      write (i ^ "CfAst.Free (");
      write_loc l "";
      write (")\n")
   

and write_stmt a i =
  match a with
    ApplyStmt   b ->
      write (i ^ "CfAst.ApplyStmt (\n");
      write_expr b (id i);
      write (i ^ ");\n")

  | ConnectStmt b ->
      write (i ^ "CfAst.ConnectStmt (\n");
      write_expr b (id i);
      write (i ^ ");\n")

and write_stmts stmts i =
  match stmts with
    [] -> ()
  | stmt :: stmts ->
      write_stmt stmt i;
      write_stmts stmts i;

and write_loc loc i =
  match loc with
    Loc.Unify    (s, i0, i1)     -> write (i ^ "Loc.Unify (\"" ^ s ^ "\", " ^ string_of_int i0 ^ ", " ^ string_of_int i1 ^ ")")
  | Loc.Argument (s, i0, i1, i2) -> write (i ^ "Loc.Argument (\"" ^ s ^ "\", " ^ string_of_int i0 ^ ", " ^ string_of_int i1 ^ ", " ^ string_of_int i2 ^ ")")
  | Loc.Unknown  s               -> write (i ^ "Loc.Unknown \"" ^ s ^ "\"")

;;

let write_apply a i channel =
  write_channel := channel;
  write_expr a i;;



