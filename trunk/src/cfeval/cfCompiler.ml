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


exception Error of string;;


(* Compilation error handling. *)

let lastLocation = ref (Loc.unknown "Report Default");;

let setLocation loc =
  if Loc.isKnown loc then lastLocation := loc;;

let error msg =
  Report.fatal(
    "** Compilation Error\n\n" ^
    "  May Be Near --> " ^ Loc.toString !lastLocation ^ "\n\n" ^ msg);;


(** Compiler environment. *)

type cenv = Env of cenv * string list  (* Parent env, port names. *)
          | EnvRoot
;;

let rec checkUniqueNames1 name names =
  match names with
    [] -> ()
  | n :: names ->
      if name = n then raise (Error ("Name duplicated in lexical scope: " ^ name ^ "."));
      checkUniqueNames1 name names
;;

let rec checkUniqueNames names =
  match names with
    [] -> ()
  | name :: names ->
      checkUniqueNames1 name names;
      checkUniqueNames names
;;

let extendEnv cenv names =
  checkUniqueNames names;
  Env (cenv, names)
;;

let rec findIndex count name names =
  match names with
    [] -> raise Not_found
  | n :: names -> if n = name then count else findIndex (count + 1) name names;;

let rec findName cenv name = (* Returns (frameIndex, portIndex).  frameIndex starts at 0, portIndex starts at 0. *)
  match cenv with
    EnvRoot -> raise (Error ("Name not found in lexical scope:  " ^ name))
  | Env (parent, names) ->
      try
        (0, findIndex 0 name names)
      with Not_found -> begin
        match findName parent name with
          (frameIndex, portIndex) -> (frameIndex + 1, portIndex)
      end
;;



(** Runtime helpers. *)

(** Evaluations and connects instance arguments. *)
let rec evalArgs argExprs renv portValues index =
  match argExprs with
    [] -> ()
  | argExpr :: argExprs ->
      argExpr renv portValues.(index);
      evalArgs argExprs renv portValues (index + 1)
;;
      
(** Applies a new instance. *)
let applyComponent appRenv appLoc comp argCount argExprs =
  let compRenv, compLoc, arity, portNames, compStmts = CfTypes.getCompInfo comp in
  if argCount <> arity  then raise (CfTypes.Error ("Number of instantiation arguments (" ^ string_of_int argCount ^ ") does not match number of component ports (" ^ string_of_int arity ^ ").\n\n  Instance Location: " ^ Loc.toString appLoc ^ "\n\n  Component Location: " ^ Loc.toString compLoc));
  let subRenv = CfTypes.extendEnv appRenv appLoc (compRenv, compLoc, arity, portNames, compStmts) in
  evalArgs argExprs appRenv (CfTypes.getRenvValues subRenv) 0;
  compStmts subRenv;
  subRenv
;;


(** Recursive compilation functions. *)

(*
stmt(s) : renv -> unit
expr    : renv -> variable -> unit
*)

let rec compExpr cenv ast =
  let expr =
    match ast with
    | CfAst.Apply       (loc, comp, args)       -> compApply       cenv loc comp args
    | CfAst.Connect     (loc, expr0, expr1)     -> compConnect     cenv loc expr0 expr1
    | CfAst.Cond        (loc, p, t, f)          -> compCond        cenv loc p t f
    | CfAst.Name        (loc, name)             -> compName        cenv loc name
    | CfAst.DotName     (loc, sys, name)        -> compDotName     cenv loc sys name
    | CfAst.DotPosition (loc, sys, position)    -> compDotPosition cenv loc sys position
    | CfAst.Comp        (loc, ports, stmts)     -> compComponent   cenv loc ports stmts
    | CfAst.Prim        (loc, name, ports)      -> compPrimitive   cenv loc name ports
    | CfAst.Integer     (loc, i)                -> compInteger     cenv loc i
    | CfAst.Float       (loc, f)                -> compFloat       cenv loc f
    | CfAst.Boolean     (loc, b)                -> compBoolean     cenv loc b
    | CfAst.Vector      (loc, s)                -> compVector      cenv loc s
    | CfAst.Record      (loc, fields)           -> compRecord      cenv loc fields
    | CfAst.Free        loc                     -> compFree        cenv loc
  in
  let taskedExpr renv variable =
    CfTypes.readyTask (renv, fun () -> expr renv variable)
  in
    taskedExpr

and compStmt cenv astStmt =
  match astStmt with
    CfAst.ApplyStmt astApply ->
      let exprApply = compExpr cenv astApply in
      let stmt renv =
        exprApply renv (CfTypes.newFree ())
      in
        stmt

  | CfAst.ConnectStmt astConnect ->
      let exprConnect = compExpr cenv astConnect in
      let stmt renv =
        exprConnect renv (CfTypes.newFree ())
      in
        stmt

and compStmts cenv astStmts =
  match astStmts with
    [] -> (fun renv -> ())
  | astStmt :: astStmts ->
      let stmt  = compStmt  cenv astStmt
      and stmts = compStmts cenv astStmts
      in
        (fun renv -> stmt renv; stmts renv)

and compApply cenv loc astComp astArgs =
  setLocation loc;
  let exprComp = compExpr cenv astComp in
  let exprArgs = List.map (function astArg  -> compExpr cenv astArg)  astArgs in
  let argCount = List.length astArgs in

  let expr renv system =
    CfTypes.setLocation loc;
    let comp = CfTypes.newFreeWithTask renv (fun comp ->
      CfTypes.unify system (CfTypes.newSystem (applyComponent renv loc comp argCount exprArgs))
    ) in
    exprComp renv comp
  in
  expr

and compConnect cenv loc astExpr0 astExpr1 =
  setLocation loc;
  let expr0 = compExpr cenv astExpr0 in
  let expr1 = compExpr cenv astExpr1 in
  let expr renv result =
    CfTypes.setLocation loc;
    expr0 renv result;
    expr1 renv result
  in
  expr

and compCond cenv loc astPred astTrue astFalse =
  setLocation loc;
  let exprPred  = compExpr cenv astPred in
  let exprTrue  = compExpr cenv astTrue in
  let exprFalse = compExpr cenv astFalse in
  let expr renv result =
    CfTypes.setLocation loc;
    let pred = CfTypes.newFreeWithTask renv (fun pred ->  (* XXX How to assign variable dependency? *)
      if CfTypes.getBoolean pred then
        exprTrue renv result
      else
        exprFalse renv result
    ) in
    exprPred renv pred
  in
    expr

and compName cenv loc name =
  setLocation loc;
  let (envIndex, valueIndex) = findName cenv name in
  let expr renv result =
    CfTypes.setLocation loc;
    CfTypes.unify result (CfTypes.getRenvValues (CfTypes.getRelativeEnv renv envIndex)).(valueIndex)
  in
    expr

and compDotName cenv loc astSysOrRec name =
  setLocation loc;
  let exprSysOrRec = compExpr cenv astSysOrRec in
  let expr renv result =
    CfTypes.setLocation loc;
    let sysOrRec = CfTypes.newFreeWithTask renv (fun sysOrRec ->
      CfTypes.unify result (CfTypes.getFieldByName (if CfTypes.isSystem sysOrRec then (CfTypes.getPorts sysOrRec) else sysOrRec) name)
    ) in
    exprSysOrRec renv sysOrRec
  in
    expr

and compDotPosition cenv loc astSysOrRec position =
  setLocation loc;
  let exprSysOrRec = compExpr cenv astSysOrRec in
  let index = Intbig.int_of_intbig position - 1 in
  let expr renv result =
    CfTypes.setLocation loc;
    let sysOrRec = CfTypes.newFreeWithTask renv (fun sysOrRec ->
      CfTypes.unify result (CfTypes.getFieldByIndex (if CfTypes.isSystem sysOrRec then (CfTypes.getPorts sysOrRec) else sysOrRec) index)
    ) in
    exprSysOrRec renv sysOrRec
  in
    expr
    
and compComponent cenv loc ports astStmts =
  setLocation loc;
  let subCenv = extendEnv cenv ports in
  let ports = Array.of_list ports in
  let stmts = compStmts subCenv astStmts in
  let arity = Array.length ports in
  let expr renv result =
    CfTypes.setLocation loc;
    CfTypes.unify result (CfTypes.newComp (renv, loc, arity, ports, stmts))
  in
    expr

and compPrimitive cenv loc name ports =
  setLocation loc;
  let subCenv = extendEnv cenv ports in
  let ports = Array.of_list ports in
  let arity = Array.length ports in
  let stmts =
    try
      CfPrims.compilePrimitive name arity
    with
      CfTypes.Error msg -> raise (Error msg)
  in
  let expr renv result =
    CfTypes.setLocation loc;
    CfTypes.unify result (CfTypes.newComp (renv, loc, arity, ports, stmts))
  in
    expr

and compInteger cenv loc i =
  setLocation loc;
  let i = CfTypes.newInteger i in
  let expr renv result =
    CfTypes.setLocation loc;
    CfTypes.unify result i
  in
    expr

and compFloat cenv loc f =
  setLocation loc;
  let f = CfTypes.newFloat f in
  let expr renv result =
    CfTypes.setLocation loc;
    CfTypes.unify result f
  in
    expr

and compBoolean cenv loc b =
  setLocation loc;
  let b = CfTypes.newBoolean b in
  let expr renv result =
    CfTypes.setLocation loc;
    CfTypes.unify result b
  in
    expr

and compVector cenv loc s =
  setLocation loc;
  CfTypes.checkBits s;
  let value = Intbig.intbig_of_binary_string s in
  let width = Intbig.intbig_of_int (String.length s) in
  let vector = CfTypes.newVectorConst value width in
  let expr renv result =
    CfTypes.setLocation loc;
    CfTypes.unify result (vector (CfTypes.getEnvId renv))
  in
    expr

and compRecord cenv loc fields =
  setLocation loc;
  let arity = List.length fields in
  let (names, values) = List.split fields in
  checkUniqueNames names;
  let names = Array.of_list names in
  let exprs = Array.of_list (List.map (fun value -> compExpr cenv value) values) in
  let expr renv result =
    CfTypes.setLocation loc;
    let values = Array.make arity (CfTypes.newFree ()) in
    for i = 0 to arity - 1 do
      let var = CfTypes.newFree () in
      values.(i) <- var;
      exprs.(i) renv var
    done;
    CfTypes.unify result (CfTypes.newRecord arity names values)
  in
    expr

and compFree cenv loc =
  setLocation loc;
  let free = CfTypes.newFreeLoc loc "_" in
  let expr renv result =
    CfTypes.unify result (free renv)
  in
    expr
;;


let compileApplication astApply =
  try
    let expr = compExpr EnvRoot astApply in
    let renv = CfTypes.newEnvRoot () in
    let run () =
      expr renv (CfTypes.newFree ())
    in
    (renv, run)
  with
    Error msg -> error msg; raise (Error "")
;;


