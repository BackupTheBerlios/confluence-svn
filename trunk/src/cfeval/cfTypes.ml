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


(** Exceptions *)

(** General runtime error exception. *)
exception Error of string;;

(** Main types. *)

type renv = {
    renvId        : Cf_fnf.system;
    renvParent    : renv;
    renvCompLoc   : Loc.loc;
    renvAppLocs   : Loc.loc list;
    renvPorts     : variable;
  }

and variable = value ref

and value    = Free      of slot list ref * variable list ref
             | Integer   of Intbig.intbig
             | Float     of float
             | Boolean   of bool
             | Vector    of Cf_fnf.producer
             | Vector0
             | Record    of int * string array * variable array
             | System    of renv
             | Comp      of renv * Loc.loc * int * string array * (renv -> unit)
             | Property  of property

and property = PropertyVector of Cf_fnf.producer
             | PropertyNot    of property
             | PropertyOr     of property * property
             | PropertyNext   of property
             | PropertyUntil  of property * property

and slot = renv * int ref * (unit -> unit)
;;


(** Root environment. *)
let rec rootRenv =
  {
    renvId      = Cf_fnf.new_root_system ();
    renvParent  = rootRenv;
    renvCompLoc = Loc.unknown "Root environment.";
    renvAppLocs = [];
    renvPorts   = ref (Record (0, [||], [||]));
  }
;;


(** Runtime error reporting. *)

let lastLocation = ref (Loc.unknown "Report Default");;
let lastRenv = ref rootRenv;;

let setLocation loc =
  if Loc.isKnown loc then lastLocation := loc
;;


let rec instanceTrace locs =
  match locs with
  | [] -> ""
  | loc :: locs -> instanceTrace locs ^ "    " ^ Loc.toString loc ^ "\n"
;;

let error msg =
  Report.error (
    "** Runtime Error\n\n" ^
    msg ^ "\n\n" ^
    "  Error May Be Near:\n\n    " ^ Loc.toString !lastLocation ^ "\n\n" ^
    "  Instantiation Trace:\n\n" ^ instanceTrace !lastRenv.renvAppLocs ^ "\n")
;;


(** Helpers. *)

let rec typeToString v =
  match !v with
  | Free       (t, f) -> "Free(slots:" ^ string_of_int (List.length !t) ^ "  frees:" ^ string_of_int (List.length !f) ^ ")"
  | Integer    i -> "Integer(" ^ Intbig.string_of_intbig i ^ ")"
  | Float      f -> "Float(" ^ string_of_float f ^ ")"
  | Boolean    b -> "Boolean(" ^ (if b then "true" else "false") ^ ")"
  | Vector     p -> "Vector(" ^ string_of_int (Cf_fnf.width_of_producer p) ^ ")"
  | Vector0      -> "Vector(0)"
  | Record     (a, n, v) -> "Record(" ^ List.fold_left2 (fun a n v -> a ^ " " ^ n ^ ":" (*^ typeToString v*)) "" (Array.to_list n) (Array.to_list v) ^ " )"
  | System     s -> "System(" ^ typeToString s.renvPorts ^ ")"
  | Comp       _ -> "Component"
  | Property   p -> "Property(" ^ propertyToString p ^ ")"

(*
and renvToString renv =
  "renv:  CompLoc = " ^ Loc.toString renv.renvCompLoc ^ "  Names = ( " ^ List.fold_left (fun s n -> s ^ n ^ " ") "" (Array.to_list renv.renvNames) ^ ")"
  (*
                                                      ^ "  Values = ( " ^ List.fold_left (fun s n -> s ^ typeToString n ^ " ") "" (Array.to_list renv.renvValues) ^ ")"
                                                        *)
*)

and propertyToString p =
  match p with
  | PropertyVector prod -> typeToString (ref (Vector prod))
  | PropertyNot p -> "(! " ^ propertyToString p ^ ")"
  | PropertyOr (p0, p1) -> "(" ^ propertyToString p0 ^ " || " ^ propertyToString p1 ^ ")"
  | PropertyNext p -> "(X " ^ propertyToString p ^ ")"
  | PropertyUntil (p0, p1) -> "(" ^ propertyToString p0 ^ " U " ^ propertyToString p1 ^ ")"
;;

let typeError v exp = 
  "Type error.  Expecting " ^ exp ^ ".  Got " ^ typeToString v ^ ".";;


(** Free variable monitor. *)

let freeVariableMemory = ref [];;
let freeVariableDepend = ref [];;
let freeVariableDeterminedThreshold = 10000;;
let freeVariableDeterminedCounter = ref freeVariableDeterminedThreshold;;

let freeVariableClean () =
  freeVariableMemory := List.fold_left (fun mem varData ->
    let (_, _, _, var) = varData in
    match !var with
    | Free _ -> varData :: mem
    | _      -> mem
  ) [] !freeVariableMemory;
  freeVariableDepend := List.fold_left (fun dep_mem (var, deps) ->
    match !var with
    | Free _ -> (var, deps) :: dep_mem
    | _      -> dep_mem
  ) [] !freeVariableDepend;
;;

let freeVariableAdd (renv, loc, name, var) =
  freeVariableMemory := (renv, loc, name, var) :: !freeVariableMemory
;;

let setVariableDependence var depvars =
  freeVariableDepend := (var, depvars) :: !freeVariableDepend
;;

let freeVariableDetermined () =
  decr freeVariableDeterminedCounter;
  if !freeVariableDeterminedCounter = 0 then begin
    freeVariableDeterminedCounter := freeVariableDeterminedThreshold;
    freeVariableClean ()
  end
;;


(** Task queue. *)

let taskList = ref [];;

let readyTask (renv, task) =
  taskList := (renv, task) :: !taskList
;;

let executeTasks () =
  while !taskList <> [] do
    let (renv, task) = List.hd !taskList in
    taskList := List.tl !taskList;
    try
      lastRenv := renv;
      task ()
    with
      Error msg -> error msg
  done
;;


(** Sync slots. *)

let newSlot renv count task =
  (renv, ref count, task)
;;

let sync slot =
  let (renv, count, task) = slot in
  decr count;
  if !count <= 0 then readyTask (renv, task)
;;

let incrSlot slot =
  let (renv, count, task) = slot in
  incr count
;;




(** Variable unification. *)

(** Unification error. *)
let unifyError var0 var1 =
  raise (Error ("Unification Error: " ^ typeToString var0 ^ " != " ^ typeToString var1))
;;

(** Recursive unification. *)
let rec unify var0 var1 upSet =        (* upSet can go to a set if O(n) lookup is too expensive. *)
  let (val0, val1) = (!var0, !var1) in
  if not (val0 == val1 || List.exists (function (a, b) -> (a == val0 && b == val1) || (a == val1 && b == val0)) upSet) then
    let upSet = (val0, val1) :: upSet in
    match (val0, val1) with

      (Free (slots0, frees0), Free (slots1, frees1)) ->
        let slotsNew = List.rev_append !slots0 !slots1 in
        let freesNew = List.rev_append !frees0 !frees1 in
        let valNew = Free (ref slotsNew, ref freesNew) in
        List.iter (fun var -> var := valNew) freesNew

    | (Free (slots, frees), value)
    | (value, Free (slots, frees)) ->
        List.iter (fun var -> var := value; freeVariableDetermined ()) !frees;
        List.iter (fun slot -> sync slot) !slots

    | (Record (arity0, names0, variables0), Record (arity1, names1, variables1)) ->
        if arity0 <> arity1 || names0 <> names1 then unifyError var0 var1;
        for i = 0 to arity0 - 1 do
          unify variables0.(i) variables1.(i) upSet
        done
        
    | (Integer i0, Integer i1) ->
        if Intbig.ne i0 i1 then unifyError var0 var1;
        
    | (Float f0, Float f1) ->
        if f0 <> f1 then unifyError var0 var1;
        
    | (Boolean b0, Boolean b1) ->
        if b0 <> b1 then unifyError var0 var1;
        
    | (Vector p0, Vector p1) ->
        unifyError var0 var1

    | (Vector0, Vector0) ->
        unifyError var0 var1
        
    | (Property _, Property _) ->
        unifyError var0 var1
        
    | (System _, System _) ->
        unifyError var0 var1
        
    | (Comp _, Comp _) ->
        unifyError var0 var1
        
    | (_, _) ->
        unifyError var0 var1
;;

(** Unification front end. *)
let unify v0 v1 =
  unify v0 v1 []
;;

(** Adds a slot to a variable.
    If free, variable stores slot.
    If record, all elements must be determined before sync.
    Else slot is synced. *)
let rec slotVariableStrict slot var varSet =
  if List.exists (fun var0 -> !var == !var0) varSet then
    sync slot
  else
    match !var with
      Free (slots, frees) ->
        let (renv, count, task) = slot in
        let slotNew = (renv, ref 1, (fun () -> slotVariableStrict slot var varSet)) in
        let slotsNew = slotNew :: !slots in
        List.iter (fun var ->
          match !var with
            Free (slots, _) -> slots := slotsNew
          | _ -> raise (Error "CfTypes.slotVariableStrict:  Should not get here.  Free variable references a non free variable.  (1)")
        ) !frees

    | Record (arity, names, values) ->
        let varSet = var :: varSet in
        for i = 0 to arity - 1 do
          incrSlot slot;
          slotVariableStrict slot values.(i) varSet
        done;
        sync slot

    | _ -> sync slot
;;

(** SlotVariable front end. *)
let slotVariableStrict slot var =
  slotVariableStrict slot var []
;;

(** SlotVariable partial.
    Syncs slot if not type is not free.
    Will sync even if type is a record that is not fully determined.  *)
let slotVariableLenient slot var =
  match !var with
  | Free (slots, frees) ->
      let slotsNew = slot :: !slots in
      List.iter (fun var ->
        match !var with
          Free (slots, _) -> slots := slotsNew
        | _ -> raise (Error "CfTypes.slotVariableLenient:  Should not get here.  Free variable references a non free variable.  (1)")
      ) !frees
  | _ -> sync slot
;;


(** Free Variables Functions *)

(** Creates a new free variable. *)
let newFree () =
  let rec var = ref (Free (ref [], ref [var])) in
  var
;;

(** Creates a new free variable with a location. *)
let newFreeLoc loc name renv =
  let var = newFree () in
  freeVariableAdd (renv, loc, name, var);
  var
;;

(** Assigns a slot of a new free variable. *)
let newFreeWithTask renv task =
  let rec var = ref (Free (ref [(renv, ref 1, fun () -> task var)], ref [var])) in
  var
;;

let isFree v =
  match !v with
    Free _ -> true
  | _ -> false
;;


(** Boolean Variable Functions *)

let newBoolean b =
  ref (Boolean b);;

let isBoolean v =
  match !v with
    Boolean _ -> true
  | _ -> false;;

let getBoolean v =
  match !v with
    Boolean i -> i
  | _ -> raise (Error (typeError v "Boolean"));;


(** Recursive comparision. *)
let rec isEqual renv var0 var1 varBool upSet =
  let (val0, val1) = (!var0, !var1) in
  if val0 == val1 || List.exists (function (a, b) -> (a == val0 && b == val1) || (a == val1 && b == val0)) upSet then
    unify varBool (ref (Boolean true))
  else
    let upSet = (val0, val1) :: upSet in
    match (val0, val1) with

      (Free _, _) ->
        slotVariableLenient (renv, ref 1, fun () -> isEqual renv var0 var1 varBool upSet) var0

    | (_, Free _) ->
        slotVariableLenient (renv, ref 1, fun () -> isEqual renv var0 var1 varBool upSet) var1

    | (Integer i0, Integer i1) ->
        if Intbig.eq i0 i1 then
          unify varBool (newBoolean true)
        else 
          unify varBool (newBoolean false)
      
    | (Float f0, Float f1) ->
        if f0 = f1 then
          unify varBool (newBoolean true)
        else 
          unify varBool (newBoolean false)
      
    | (Boolean b0, Boolean b1) ->
        if b0 = b1 then
          unify varBool (newBoolean true)
        else 
          unify varBool (newBoolean false)
      
    | (Vector _, Vector _) ->
        unify varBool (newBoolean false)
      
    | (Vector0, Vector0) ->
        unify varBool (newBoolean false)
      
    | (Record (arity0, names0, values0), Record (arity1, names1, values1)) ->
        if arity0 <> arity1 || names0 <> names1 then
          unify varBool (newBoolean false)
        else
          let bools = Array.make arity0 (newFree ()) in
          let slot = (renv, ref (arity0 + 1), fun () -> unify varBool (newBoolean (List.for_all getBoolean (Array.to_list bools)))) in
          sync slot;  (* In case arity is 0. *)
          for i = 0 to arity0 - 1 do
            let free = newFree () in
            bools.(i) <- free;
            slotVariableLenient slot free;
            isEqual renv values0.(i) values1.(i) free upSet
          done

    | (System _, System _) ->
        unify varBool (newBoolean false)
      
    | (Comp _, Comp _) ->
        unify varBool (newBoolean false)
      
    | (_, _) ->
        unify varBool (newBoolean false)
;;

(** Comparision front end. *)
let isEqual renv var0 var1 varBool =
  isEqual renv var0 var1 varBool []
;;



(* Integer Variable Functions *) 

let newInteger i =
  ref (Integer i);;

let isInteger v =
  match !v with
    Integer _ -> true
  | _ -> false;;

let getInteger v =
  match !v with
    Integer i -> i
  | _ -> raise (Error (typeError v "Integer"));;

let getInt v =
  let i = getInteger v in
  try
    Intbig.int_of_intbig i
  with
    Failure _ -> raise (Error ("Integer is too big to convert to fixed width integer: " ^ Intbig.string_of_intbig i));;


(* Float Variable Functions *)

let newFloat f =
  ref (Float f);;

let isFloat v =
  match !v with
    Float _ -> true
  | _ -> false;;

let getFloat v =
  match !v with
    Float i -> i
  | _ -> raise (Error (typeError v "Float"));;



(* Vector Variable Functions *) 

let zero_width_vector = ref Vector0;;

let is_zero_width_vector v =
  match !v with
  | Vector0 -> true
  | Vector _ -> false
  | _ -> raise (Error (typeError v "Vector"))
;;

let checkBits bitStr =
  let checkBit c =
    match c with
      '0' -> ()
    | '1' -> ()
    | a   -> raise (Error "Vector constant requires string containing only 1's and 0's.")
  in
    String.iter checkBit bitStr;;

let newVector producer =
  ref (Vector producer)
;;

let newVectorConst value width system =  (* System is last argument for partial application in compiler. *)
  if Intbig.eq width Intbig.zero then
    ref Vector0
  else
    ref (Vector (Cf_fnf.new_const system (Intbig.binary_string_of_intbig value width)))
;;

let isVector v =
  match !v with
  | Vector _
  | Vector0 -> true
  | _ -> false
;;

let getWidth v =
  match !v with
  | Vector p -> Cf_fnf.width_of_producer p
  | Vector0  -> 0
  | _ -> raise (Error (typeError v "Vector"))
;;

let getProducer v =
  match !v with
  | Vector p -> p
  | Vector0  -> raise (Error "Zero width vector does not have a producer.")
  | _ -> raise (Error (typeError v "Vector"))
;;

let checkWidth v =
  if getWidth v < 0 then raise (Error "Invalid vector width.  Width less than zero.");;

let checkWidthNotZero v =
  if getWidth v <= 0 then raise (Error "Invalid vector width.  Vector instance requires width greater than zero.");;

let checkWidthIs v w =
  if getWidth v <> w || w < 0 then raise (Error ("Invalid vector width.  Expecting " ^ string_of_int w ^ ", not " ^ string_of_int (getWidth v) ^ "."));;

let checkWidthsAreSame v1 v2 =
  checkWidth v1;
  if getWidth v1 <> getWidth v2 then raise (Error "Invalid vector width.  Vector pairs must have matching widths.");;
      

(** Property Variable Functions *)

let isProperty v =
  match !v with
  | Vector _   -> getWidth v = 1
  | Property _ -> true
  | _ -> false
;;

let newPropertyVector v =
  match !v with
  | Vector prod -> checkWidthIs v 1; ref (Property (PropertyVector prod))
  | _ -> raise (Error (typeError v "Vector"))
;;

let newPropertyNot v =
  match !v with
  | Property p -> ref (Property (PropertyNot p))
  | _ -> raise (Error (typeError v "Property"))
;;

let newPropertyOr v0 v1 =
  match !v0, !v1 with
  | Property p0, Property p1 -> ref (Property (PropertyOr (p0, p1)))
  | _, Property _ -> raise (Error (typeError v0 "Property"))
  | Property _, _ -> raise (Error (typeError v1 "Property"))
  | _, _ -> raise (Error (typeError v0 "Property"))
;;

let newPropertyNext v =
  match !v with
  | Property p -> ref (Property (PropertyNext p))
  | _ -> raise (Error (typeError v "Property"))
;;

let newPropertyUntil v0 v1 =
  match !v0, !v1 with
  | Property p0, Property p1 -> ref (Property (PropertyUntil (p0, p1)))
  | _, Property _ -> raise (Error (typeError v0 "Property"))
  | Property _, _ -> raise (Error (typeError v1 "Property"))
  | _, _ -> raise (Error (typeError v0 "Property"))
;;


(** Record functions. *)

let newRecord arity names values =
  ref (Record (arity, names, values))
;;

let isRecord v =
  match !v with
    Record _ -> true
  | _ -> false
;;

let newNil () =
  newRecord 0 [||] [||]
;;

let newPair head tail =
  newRecord 2 [|"hd"; "tl"|] [|head; tail|]
;;
    
let rec findNameIndex index name names =
  if names.(index) = name then
    index
  else
    findNameIndex (index + 1) name names
;;

let getFieldByName v name =
  match !v with
    Record (_, names, values) ->
      (try
        values.(findNameIndex 0 name names)
      with
        Invalid_argument _ ->
          raise (Error ("Record error.  Record has no field named \"" ^ name ^ "\".")))
  | _ -> raise (Error (typeError v "Record"))
;;

let getFieldByIndex v index =
  match !v with
    Record (arity, _, values) ->
      if index < 0      then raise (Error "Record error.  Index is less than or equal to zero (references start at 1).");
      if index >= arity then raise (Error "Record error.  Index exceeds arity.");
      values.(index)
  | _ -> raise (Error (typeError v "Record"))
;;


(* List Functions *)

let isList v =
  match !v with
    Record (arity, names, _) -> arity = 0 || names = [| "hd"; "tl" |]
  | _ -> false
;;

let rec listToString v =
  if not (isList v) then raise (Error (typeError v "List"));
  match !v with
    Record (arity, _, values) ->
      if arity = 0 then
        ""
      else
        (match !(values.(0)) with
          Integer i ->
            begin
              try
                String.make 1 (Intbig.char_of_intbig i)
              with _ ->
                raise (Error "Type error.  List contains non printable characters.  Integers outside ascii range.")
            end
        | _ -> raise (Error "Type error.  List contains non printable characters.  Non integer list elements.")
        ) ^ listToString values.(1)
  | _ -> raise (Error (typeError v "List"))
;;

let rec stringToList a =
  match a with
    "" -> newNil ()
  | _  -> newPair (newInteger (Intbig.intbig_of_char a.[0])) (stringToList (String2.right_string a))
;;

let rec recordInfo0 pairs =
  match pairs with
    [] ->  newNil ()
  | (name, value) :: pairs ->
      newPair (newRecord 2 [|"name"; "value"|] [|stringToList name; value|]) (recordInfo0 pairs)
;;
      
let recordInfo v =
  match !v with
    Record (arity, names, values) ->
      recordInfo0 (List.combine (Array.to_list names) (Array.to_list values))
  | _ -> raise (Error (typeError v "Record"))
;;

let rec listToIntegerList v =
  if not (isList v) then raise (Error (typeError v "List"));
  match !v with
    Record (arity, _, values) ->
      if arity = 0 then
        []
      else
        getInteger values.(0) :: listToIntegerList values.(1)
  | _ -> raise (Error (typeError v "List"))
;;
    
let rec listToIntList v =
  if not (isList v) then raise (Error (typeError v "List"));
  match !v with
    Record (arity, _, values) ->
      if arity = 0 then
        []
      else
        getInt values.(0) :: listToIntList values.(1)
  | _ -> raise (Error (typeError v "List"))
;;


(* Component (and Primitive) Functions *)

let newComp (defRenv, compLoc, arity, portNames, stmt) =
  ref (Comp (defRenv, compLoc, arity, portNames, stmt))
;;

let isComp v =
  match !v with
    Comp _ -> true
  | _ -> false
;;

let getCompInfo v =
  match !v with
    Comp (defRenv, compLoc, arity, portNames, stmt) -> (defRenv, compLoc, arity, portNames, stmt)
  | _ -> raise (Error (typeError v "Component"))
;;


(** System Functions *)

let newSystem renv =
  ref (System renv)
;;

let isSystem v =
  match !v with
    System _ -> true
  | _ -> false
;;

let getRenv v =
  match !v with
    System renv -> renv
  | _ -> raise (Error (typeError v "System"))
;;

let getPorts v =
  match !v with
    System renv -> renv.renvPorts
  | _ -> raise (Error (typeError v "System"))
;;


(** Environment Functions *)

let getEnvId renv =
  renv.renvId
;;

let newEnvRoot () =
  rootRenv
;;

let extendEnv appRenv appLoc (compRenv, compLoc, arity, portNames, _) =
  let portValues = Array.make arity (newFree ()) in
  let appLocs = appLoc :: appRenv.renvAppLocs in
  let renv = {
    renvId = Cf_fnf.new_sub_system (getEnvId appRenv) appLocs compLoc;
    renvParent = compRenv;
    renvCompLoc = compLoc;
    renvAppLocs = appLocs;
    renvPorts = newRecord arity portNames portValues;
  } in
  Array.iteri (fun i portName -> portValues.(i) <- newFreeLoc compLoc portName renv) portNames;
  renv
;;

let rec getRelativeEnv renv index =
  if index <= 0 then
    renv
  else
    getRelativeEnv renv.renvParent (index - 1)
;;

let getRenvValues renv =
  match !(renv.renvPorts) with
    Record (_, _, values) -> values
  | _ -> raise (Error "System error.  System ports is not a record.  Should not get here.")
;;



(* Check all port variables are determined. *)

let sortCompare var0 var1 =
  match (var0, var1) with
    ((renv0, _, _, _), (renv1, _, _, _)) ->
      compare (List.length renv0.renvAppLocs) (List.length renv1.renvAppLocs)
;;

let rec consolidateResiduals residuals sofar =
  match residuals with
    [] -> sofar
  | (renv, _, _, var) as varData :: residuals ->
      let (group, other) = List.partition (function (renv0, _, _, var0) -> !var == !var0) residuals in
      begin match !var with
        Free (slots, _) ->
          if !slots = [] then
            consolidateResiduals other sofar
          else
            consolidateResiduals other (List.sort sortCompare (varData :: group) :: sofar)
      | _ ->
        consolidateResiduals other sofar
      end
;;

let buildRootDependencyList () =
  List.fold_left (fun sofar (_, vars) ->
    List.fold_left (fun sofar var ->
      if not (isFree var) || List.exists (fun listvar -> !listvar == !var) sofar then
        sofar
      else
        var :: sofar
    ) sofar vars
  ) [] !freeVariableDepend
;;

let buildDependencyGraph freeVariableDepend =
  let t1 = List.map (fun (var, depvars) -> var, List.filter isFree depvars) freeVariableDepend in
  let t2 =
    List.fold_left (fun sofar (_, depvars) ->
      List.fold_left (fun sofar depvar ->
        if List.exists (fun (var, _) -> !var == !depvar) t1 then
          sofar
        else
          (depvar, []) :: sofar
      ) sofar depvars
    ) [] t1
  in
  let t3 = t2 @ t1 in
  let i_of_var var =
    List2.indexf (fun (listvar, _) -> !listvar == !var) t3 0
  in

  let freevars = Array.make (List.length t3) (ref (Boolean true)) in
  List2.iteri (fun i (var, _) -> freevars.(i) <- var) t3;

  let connect = Array.make (List.length t3) [||] in
  List2.iteri (fun i (var, depvars) ->
    connect.(i) <- Array.make (List.length depvars) 0;
    List2.iteri (fun j depvar -> connect.(i).(j) <- i_of_var depvar) depvars
  ) t3;
  freevars, connect
;;

let printValueDependencies graph =
  let (_, connect) = graph in
  print_string "** Free Value Dependencies\n\n  <dependent_id> : <id_0> <id_1> ...)\n\n";
  Array.iteri (fun i deps ->
    print_string ("  " ^ string_of_int i ^ " :");
    Array.iter (fun j -> print_string (" " ^ string_of_int j)) deps;
    print_string "\n"
  ) connect;
  print_newline ();
;;


let reportFreeVariableSet msg set id_of_freevar =
  let (_, _, _, a_var) = List.hd set in
    Report.error ("** Free Variable Error\n\n" ^ msg ^ "\n\n" ^
      (try "  Value id: " ^ string_of_int (id_of_freevar a_var) ^ "\n\n" with Not_found -> "") ^
      List.fold_left (fun a varData ->
        let (renv, loc, name, var) = varData in
        a ^ "  " ^ name ^ "  " ^ Loc.toString loc ^ "\n"
      ) "" set ^ "\n  Instantiation Trace:\n\n" ^ instanceTrace (let (renv, _, _, _) = List.hd set in renv.renvAppLocs) ^ "\n")
;;



let freeVariableReport () =
  freeVariableClean ();
  let freeSets = consolidateResiduals !freeVariableMemory [] in
  let rootDepends = buildRootDependencyList () in
  let errorFound = ref false in
  let graph = buildDependencyGraph !freeVariableDepend in

  let id_of_freevar var =
    let freevars, _ = graph in
    let found = ref false in
    let i = ref 0 in
    while not !found do
      if !i >= Array.length freevars then raise Not_found;
      if !(freevars.(!i)) == !var then
        found := true
      else
        incr i
    done;
    !i
  in
      
  (* Checking unset root vars. *) 
  let freevar, connect = graph in
  Array.iteri (fun i deps -> 
    if Array.length deps = 0 then
      try
        let set = List.find (fun set -> List.exists (fun (_, _, _, setvar) -> !(freevar.(i)) == !setvar) set) freeSets in
        errorFound := true;
        reportFreeVariableSet "The following unified variables are required (root dependence):" set id_of_freevar
      with
        Not_found -> Report.error "** Free Variable Error\n\nRequired variable not in free set list.  (should not get here)\n\n"
  ) connect;


  (* XXX Add cyclic checking here. *)

  if not !errorFound then begin
    List.iter (fun freeSet -> reportFreeVariableSet "The following unified variables are required (single dependence):" freeSet id_of_freevar) freeSets;
  end;

  if List.length freeSets > 0 then printValueDependencies graph;
;;  
  
let checkAllPortsDetermined () =
  if not (Report.errorReported ()) then freeVariableReport ();
;;

