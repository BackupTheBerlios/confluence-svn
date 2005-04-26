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


exception CfPrimsError;;

(** Compiler of primitives. *)

(** Strict primitive builders. *)

let stmt_of_prim_l prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    let slot = CfTypes.newSlot renv 1 (fun () -> prim ports.(0)) in
    CfTypes.slotVariableLenient slot ports.(0)
;;

let stmt_of_prim_s prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    let slot = CfTypes.newSlot renv 1 (fun () -> prim ports.(0)) in
    CfTypes.slotVariableStrict slot ports.(0)
;;

let stmt_of_prim_ll prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    let slot = CfTypes.newSlot renv 2 (fun () -> prim ports.(0) ports.(1)) in
    CfTypes.slotVariableLenient slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1)
;;

let stmt_of_prim_ss prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    let slot = CfTypes.newSlot renv 2 (fun () -> prim ports.(0) ports.(1)) in
    CfTypes.slotVariableStrict slot ports.(0);
    CfTypes.slotVariableStrict slot ports.(1)
;;

let stmt_of_prim_sl prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    let slot = CfTypes.newSlot renv 2 (fun () -> prim ports.(0) ports.(1)) in
    CfTypes.slotVariableStrict  slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1)
;;

let stmt_of_prim_x prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.unify ports.(0) (prim ())
;;

let stmt_of_prim_lx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(1) [ports.(0)];
    let slot = CfTypes.newSlot renv 1 (fun () -> CfTypes.unify ports.(1) (prim ports.(0))) in
    CfTypes.slotVariableLenient slot ports.(0)
;;

let stmt_of_prim_sx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(1) [ports.(0)];
    let slot = CfTypes.newSlot renv 1 (fun () -> CfTypes.unify ports.(1) (prim ports.(0))) in
    CfTypes.slotVariableStrict slot ports.(0)
;;

let stmt_of_prim_llx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(2) [ports.(0); ports.(1)];
    let slot = CfTypes.newSlot renv 2 (fun () -> CfTypes.unify ports.(2) (prim ports.(0) ports.(1))) in
    CfTypes.slotVariableLenient slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1)
;;


(** Vector primitive builders. *)

let stmt_of_prim_rll prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    let slot = CfTypes.newSlot renv 2 (fun () -> prim (CfTypes.getEnvId renv) ports.(0) ports.(1)) in
    CfTypes.slotVariableLenient slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1)
;;
let stmt_of_prim_rsl prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    let slot = CfTypes.newSlot renv 2 (fun () -> prim (CfTypes.getEnvId renv) ports.(0) ports.(1)) in
    CfTypes.slotVariableStrict  slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1)
;;

let stmt_of_prim_rlx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(1) [ports.(0)];
    let slot = CfTypes.newSlot renv 1 (fun () -> CfTypes.unify ports.(1) (prim (CfTypes.getEnvId renv) ports.(0))) in
    CfTypes.slotVariableLenient slot ports.(0)
;;

let stmt_of_prim_rllx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(2) [ports.(0); ports.(1)];
    let slot = CfTypes.newSlot renv 2 (fun () -> CfTypes.unify ports.(2) (prim (CfTypes.getEnvId renv) ports.(0) ports.(1))) in
    CfTypes.slotVariableLenient slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1)
;;

let stmt_of_prim_rlsx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(2) [ports.(0); ports.(1)];
    let slot = CfTypes.newSlot renv 2 (fun () -> CfTypes.unify ports.(2) (prim (CfTypes.getEnvId renv) ports.(0) ports.(1))) in
    CfTypes.slotVariableLenient slot ports.(0);
    CfTypes.slotVariableStrict  slot ports.(1)
;;

let stmt_of_prim_rlllx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(3) [ports.(0); ports.(1); ports.(2)];
    let slot = CfTypes.newSlot renv 3 (fun () -> CfTypes.unify ports.(3) (prim (CfTypes.getEnvId renv) ports.(0) ports.(1) ports.(2))) in
    CfTypes.slotVariableLenient slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1);
    CfTypes.slotVariableLenient slot ports.(2)
;;

let stmt_of_prim_rslx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(2) [ports.(0); ports.(1)];
    let slot = CfTypes.newSlot renv 2 (fun () -> CfTypes.unify ports.(2) (prim (CfTypes.getEnvId renv) ports.(0) ports.(1) )) in
    CfTypes.slotVariableStrict  slot ports.(0);
    CfTypes.slotVariableLenient slot ports.(1)
;;

let stmt_of_prim_rsslx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(3) [ports.(0); ports.(1); ports.(2)];
    let slot = CfTypes.newSlot renv 3 (fun () -> CfTypes.unify ports.(3) (prim (CfTypes.getEnvId renv) ports.(0) ports.(1) ports.(2))) in
    CfTypes.slotVariableStrict  slot ports.(0);
    CfTypes.slotVariableStrict  slot ports.(1);
    CfTypes.slotVariableLenient slot ports.(2)
;;

let stmt_of_prim_rssllx prim =
  fun renv ->
    let ports = CfTypes.getRenvValues renv in
    CfTypes.setVariableDependence ports.(4) [ports.(0); ports.(1); ports.(2); ports.(3)];
    let slot = CfTypes.newSlot renv 4 (fun () -> CfTypes.unify ports.(4) (prim (CfTypes.getEnvId renv) ports.(0) ports.(1) ports.(2) ports.(3))) in
    CfTypes.slotVariableStrict  slot ports.(0);
    CfTypes.slotVariableStrict  slot ports.(1);
    CfTypes.slotVariableLenient slot ports.(2);
    CfTypes.slotVariableLenient slot ports.(3)
;;


(* Primitive information. *)

type prim =
  | Error
  | IsEqual
  | IsComponent

  | IsSystem
  | SystemPorts

  | IsInteger
  | IntegerToString
  | StringToInteger
  | IntegerToFloat
  | IntegerLt
  | IntegerGt  (* XXX *)
  | IntegerLe  (* XXX *)
  | IntegerGe  (* XXX *)
  | IntegerAdd
  | IntegerSub
  | IntegerMul
  | IntegerDiv
  | IntegerMod
  | IntegerPow
  | IntegerNot
  | IntegerAnd
  | IntegerXor
  | IntegerOr
  | IntegerShiftLeft
  | IntegerShiftRight

  | IsFloat
  | FloatToString
  | StringToFloat
  | FloatToInteger
  | FloatLt
  | FloatGt (* XXX *)
  | FloatLe (* XXX *)
  | FloatGe (* XXX *)
  | FloatAdd
  | FloatSub
  | FloatMul
  | FloatDiv
  | FloatPow
  | FloatCeil
  | FloatFloor
  | FloatExp
  | FloatLog
  | FloatSqrt (* XXX *)
  | FloatSin
  | FloatCos
  | FloatTan
  | FloatAsin
  | FloatAcos
  | FloatAtan
  | FloatAtan2

  | IsBoolean
  | BooleanNot

  | IsRecord
  | RecordInfo

  | IsList
  | ListPrint
  | ListArgv

  | IsVector
  | VectorWidth
  | VectorClock
  | VectorEnable
  | VectorReset
  | VectorInput
  | VectorOutput
  (*
  | VectorAssert
  | VectorPrint
  *)
  | VectorConst
  | VectorAnd
  | VectorOr
  | VectorXor
  | VectorNot
  | VectorConcat
  | VectorZero
  | VectorSelect
  | VectorEqu
  | VectorLt
  | VectorAdd
  | VectorSub
  | VectorMul
  | VectorMux
  (*
  | VectorTristate
  | VectorExtComb
  | VectorExtSequ
  | VectorExtSoft
  *)
  | VectorReg
  | VectorBbox
  (*
  | VectorRom
  | VectorRam
  *)

  | IsProperty
  | PropertyNot
  | PropertyOr
  | PropertyNext
  | PropertyUntil

;;

let totalArity prim =
  match prim with
  | Error           -> 1
  | IsEqual         -> 3
  | IsComponent     -> 2

  | IsSystem        -> 2
  | SystemPorts     -> 2

  | IsInteger       -> 2
  | IntegerToString -> 2
  | StringToInteger -> 2
  | IntegerToFloat  -> 2
  | IntegerLt       -> 3
  | IntegerGt       -> 3
  | IntegerLe       -> 3
  | IntegerGe       -> 3
  | IntegerAdd      -> 3
  | IntegerSub      -> 3
  | IntegerMul      -> 3
  | IntegerDiv      -> 3
  | IntegerMod      -> 3
  | IntegerPow      -> 3
  | IntegerNot      -> 2
  | IntegerAnd      -> 3
  | IntegerXor      -> 3
  | IntegerOr       -> 3
  | IntegerShiftLeft -> 3
  | IntegerShiftRight -> 3

  | IsFloat         -> 2
  | FloatToString   -> 2
  | StringToFloat   -> 2
  | FloatToInteger  -> 2
  | FloatLt         -> 3
  | FloatGt         -> 3
  | FloatLe         -> 3
  | FloatGe         -> 3
  | FloatAdd        -> 3
  | FloatSub        -> 3
  | FloatMul        -> 3
  | FloatDiv        -> 3
  | FloatPow        -> 3
  | FloatCeil       -> 2
  | FloatFloor      -> 2
  | FloatExp        -> 2
  | FloatLog        -> 2
  | FloatSqrt       -> 2
  | FloatSin        -> 2
  | FloatCos        -> 2
  | FloatTan        -> 2
  | FloatAsin       -> 2
  | FloatAcos       -> 2
  | FloatAtan       -> 2
  | FloatAtan2      -> 3

  | IsBoolean       -> 2
  | BooleanNot      -> 2

  | IsRecord        -> 2
  | RecordInfo      -> 2

  | IsList          -> 2
  | ListPrint       -> 1
  | ListArgv        -> 1

  | IsVector        -> 2
  | VectorWidth     -> 2
  | VectorClock     -> 2
  | VectorEnable    -> 2
  | VectorReset     -> 2
  | VectorInput     -> 3
  | VectorOutput    -> 2
  (*
  | VectorAssert    -> 2
  | VectorPrint     -> 2
  *)
  | VectorConst     -> 3
  | VectorAnd       -> 3
  | VectorOr        -> 3
  | VectorXor       -> 3
  | VectorNot       -> 2
  | VectorConcat    -> 3
  | VectorZero      -> 1
  | VectorSelect    -> 3
  | VectorEqu       -> 3
  | VectorLt        -> 3
  | VectorAdd       -> 3
  | VectorSub       -> 3
  | VectorMul       -> 3                       
  | VectorMux       -> 4
  (*
  | VectorTristate  -> 3
  | VectorExtComb   -> 5
  | VectorExtSequ   -> 5
  | VectorExtSoft   -> 4
  *)
  | VectorReg       -> 3
  | VectorBbox      -> 5
  (*
  | VectorRom       -> 4
  | VectorRam       -> 4
  *)

  | IsProperty      -> 2
  | PropertyNot     -> 2
  | PropertyOr      -> 3
  | PropertyNext    -> 2
  | PropertyUntil   -> 3

;;

let toString prim =
  match prim with
  | Error           -> "Error"
  | IsEqual         -> "IsEqual"
  | IsComponent     -> "IsComponent"

  | IsSystem        -> "IsSystem"
  | SystemPorts     -> "SystemPorts"
 
  | IsInteger       -> "IsInteger"
  | IntegerToString -> "IntegerToString"
  | StringToInteger -> "StringToInteger"
  | IntegerToFloat  -> "IntegerToFloat"
  | IntegerLt       -> "IntegerLt"
  | IntegerGt       -> "IntegerGt"
  | IntegerLe       -> "IntegerLe"
  | IntegerGe       -> "IntegerGe"
  | IntegerAdd      -> "IntegerAdd"
  | IntegerSub      -> "IntegerSub"
  | IntegerMul      -> "IntegerMul"
  | IntegerDiv      -> "IntegerDiv"
  | IntegerMod      -> "IntegerMod"
  | IntegerPow      -> "IntegerPow"
  | IntegerNot      -> "IntegerNot"
  | IntegerAnd      -> "IntegerAnd"
  | IntegerXor      -> "IntegerXor"
  | IntegerOr       -> "IntegerOr"
  | IntegerShiftLeft -> "IntegerShiftLeft"
  | IntegerShiftRight -> "IntegerShiftRight"
 
  | IsFloat         -> "IsFloat"
  | FloatToString   -> "FloatToString"
  | StringToFloat   -> "StringToFloat"
  | FloatToInteger  -> "FloatToInteger"
  | FloatLt         -> "FloatLt"
  | FloatGt         -> "FloatGt"
  | FloatLe         -> "FloatLe"
  | FloatGe         -> "FloatGe"
  | FloatAdd        -> "FloatAdd"
  | FloatSub        -> "FloatSub"
  | FloatMul        -> "FloatMul"
  | FloatDiv        -> "FloatDiv"
  | FloatPow        -> "FloatPow"
  | FloatCeil       -> "FloatCeil"
  | FloatFloor      -> "FloatFloor"
  | FloatExp        -> "FloatExp"
  | FloatLog        -> "FloatLog"
  | FloatSqrt       -> "FloatSqrt"
  | FloatSin        -> "FloatSin"
  | FloatCos        -> "FloatCos"
  | FloatTan        -> "FloatTan"
  | FloatAsin       -> "FloatAsin"
  | FloatAcos       -> "FloatAcos"
  | FloatAtan       -> "FloatAtan"
  | FloatAtan2      -> "FloatAtan2"
 
  | IsBoolean       -> "IsBoolean"
  | BooleanNot      -> "BooleanNot"

  | IsRecord        -> "IsRecord"
  | RecordInfo      -> "RecordInfo"

  | IsList          -> "IsList"
  | ListPrint       -> "ListPrint"
  | ListArgv        -> "ListArgv"

  | IsVector        -> "IsVector"
  | VectorWidth     -> "VectorWidth"
  | VectorClock     -> "VectorClock"
  | VectorEnable    -> "VectorEnable"
  | VectorReset     -> "VectorReset"
  | VectorInput     -> "VectorInput"
  | VectorOutput    -> "VectorOutput"
  (*
  | VectorAssert    -> "VectorAssert"
  | VectorPrint     -> "VectorPrint"
  *)
  | VectorConst     -> "VectorConst"
  | VectorAnd       -> "VectorAnd"
  | VectorOr        -> "VectorOr"
  | VectorXor       -> "VectorXor"
  | VectorNot       -> "VectorNot"
  | VectorConcat    -> "VectorConcat"
  | VectorZero      -> "VectorZero"
  | VectorSelect    -> "VectorSelect"
  | VectorEqu       -> "VectorEqu"
  | VectorLt        -> "VectorLt"
  | VectorAdd       -> "VectorAdd"
  | VectorSub       -> "VectorSub"
  | VectorMul       -> "VectorMul"
  | VectorMux       -> "VectorMux"
  (*
  | VectorTristate  -> "VectorTristate"
  | VectorExtComb   -> "VectorExtComb"
  | VectorExtSequ   -> "VectorExtSequ"
  | VectorExtSoft   -> "VectorExtSoft"
  *)
  | VectorReg       -> "VectorReg"
  | VectorBbox      -> "VectorBbox"
  (*
  | VectorRom       -> "VectorRom"
  | VectorRam       -> "VectorRam"
  *)

  | IsProperty      -> "IsProperty"
  | PropertyNot     -> "PropertyNot"
  | PropertyOr      -> "PropertyOr"
  | PropertyNext    -> "PropertyNext"
  | PropertyUntil   -> "PropertyUntil"
;;

let fromString name =
  match name with
  | "Error" -> Error
  | "IsEqual" -> IsEqual
  | "IsComponent" -> IsComponent

  | "IsSystem" -> IsSystem
  | "SystemPorts" -> SystemPorts

  | "IsInteger" -> IsInteger
  | "IntegerToString" -> IntegerToString
  | "StringToInteger" -> StringToInteger
  | "IntegerToFloat" -> IntegerToFloat
  | "IntegerLt" -> IntegerLt
  | "IntegerGt" -> IntegerGt
  | "IntegerLe" -> IntegerLe
  | "IntegerGe" -> IntegerGe
  | "IntegerAdd" -> IntegerAdd
  | "IntegerSub" -> IntegerSub
  | "IntegerMul" -> IntegerMul
  | "IntegerDiv" -> IntegerDiv
  | "IntegerMod" -> IntegerMod
  | "IntegerPow" -> IntegerPow
  | "IntegerNot" -> IntegerNot
  | "IntegerAnd" -> IntegerAnd
  | "IntegerXor" -> IntegerXor
  | "IntegerOr" -> IntegerOr
  | "IntegerShiftLeft" -> IntegerShiftLeft
  | "IntegerShiftRight" -> IntegerShiftRight

  | "IsFloat" -> IsFloat
  | "FloatToString" -> FloatToString
  | "StringToFloat" -> StringToFloat
  | "FloatToInteger" -> FloatToInteger
  | "FloatLt" -> FloatLt
  | "FloatGt" -> FloatGt
  | "FloatLe" -> FloatLe
  | "FloatGe" -> FloatGe
  | "FloatAdd" -> FloatAdd
  | "FloatSub" -> FloatSub
  | "FloatMul" -> FloatMul
  | "FloatDiv" -> FloatDiv
  | "FloatPow" -> FloatPow
  | "FloatCeil" -> FloatCeil
  | "FloatFloor" -> FloatFloor
  | "FloatExp" -> FloatExp
  | "FloatLog" -> FloatLog
  | "FloatSqrt" -> FloatSqrt
  | "FloatSin" -> FloatSin
  | "FloatCos" -> FloatCos
  | "FloatTan" -> FloatTan
  | "FloatAsin" -> FloatAsin
  | "FloatAcos" -> FloatAcos
  | "FloatAtan" -> FloatAtan
  | "FloatAtan2" -> FloatAtan2

  | "IsBoolean" -> IsBoolean
  | "BooleanNot" -> BooleanNot

  | "IsRecord" -> IsRecord
  | "RecordInfo" -> RecordInfo

  | "IsList" -> IsList
  | "ListPrint" -> ListPrint
  | "ListArgv" -> ListArgv

  | "IsVector" -> IsVector
  | "VectorWidth" -> VectorWidth
  | "VectorClock" -> VectorClock
  | "VectorEnable" -> VectorEnable
  | "VectorReset" -> VectorReset
  | "VectorInput" -> VectorInput
  | "VectorOutput" -> VectorOutput
  (*
  | "VectorAssert" -> VectorAssert
  | "VectorPrint" -> VectorPrint
  *)
  | "VectorConst" -> VectorConst
  | "VectorAnd" -> VectorAnd
  | "VectorOr" -> VectorOr
  | "VectorXor" -> VectorXor
  | "VectorNot" -> VectorNot
  | "VectorConcat" -> VectorConcat
  | "VectorZero"   -> VectorZero
  | "VectorSelect" -> VectorSelect
  | "VectorEqu" -> VectorEqu
  | "VectorLt" -> VectorLt
  | "VectorAdd" -> VectorAdd
  | "VectorSub" -> VectorSub
  | "VectorMul" -> VectorMul
  | "VectorMux" -> VectorMux
  (*
  | "VectorTristate" -> VectorTristate
  | "VectorExtComb" -> VectorExtComb
  | "VectorExtSequ" -> VectorExtSequ
  | "VectorExtSoft" -> VectorExtSoft
  *)
  | "VectorReg"    -> VectorReg
  | "VectorBbox"   -> VectorBbox
  (*
  | "VectorRom"   -> VectorRom
  | "VectorRam"      -> VectorRam
  *)

  | "IsProperty"      -> IsProperty
  | "PropertyNot"     -> PropertyNot
  | "PropertyOr"      -> PropertyOr
  | "PropertyNext"    -> PropertyNext
  | "PropertyUntil"   -> PropertyUntil

  | _ -> raise CfPrimsError
;;


(* Remote error handling. *)

let remoteError msg =
  raise (CfTypes.Error msg);;


(* Helpers. *)

let rec checkBits msg values =
  let checkBit bit =
    if not (bit = '1' || bit = '0') then raise (CfTypes.Error (msg ^ "  Invalid bit (not 0 nor 1)."));
  in
    match values with
      [] -> ()
    | value :: values ->
        String.iter checkBit value;
        checkBits msg values;;


(* General primitives. *)

let isEqual renv =
  let ports = CfTypes.getRenvValues renv in
  CfTypes.setVariableDependence ports.(2) [ports.(0); ports.(1)];
  CfTypes.isEqual renv ports.(0) ports.(1) ports.(2)
;;

let error msg =
  raise (CfTypes.Error ("Raised Error: " ^ CfTypes.listToString msg))
;;

(** Component primitives. *)

let isComponent a =
  CfTypes.newBoolean (CfTypes.isComp a)
;;


(** System primtitives. *)

let isSystem a =
  CfTypes.newBoolean (CfTypes.isSystem a)
;;

let systemPorts system =
  CfTypes.getPorts system
;;


(** Integer primitives. *)

let isInteger a =
  CfTypes.newBoolean (CfTypes.isInteger a);;

let integerToString a =
  CfTypes.stringToList (Intbig.string_of_intbig (CfTypes.getInteger a));;

let stringToInteger str =
  CfTypes.newInteger (Intbig.intbig_of_string (CfTypes.listToString str))
;;

let integerToFloat a =
  CfTypes.newFloat (Intbig.float_of_intbig (CfTypes.getInteger a));;

let integerLt a b =
  CfTypes.newBoolean (Intbig.lt (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerGt a b =
  CfTypes.newBoolean (Intbig.gt (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerLe a b =
  CfTypes.newBoolean (Intbig.le (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerGe a b =
  CfTypes.newBoolean (Intbig.ge (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerAdd a b =
  CfTypes.newInteger (Intbig.add (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerSub a b =
  CfTypes.newInteger (Intbig.sub (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerMul a b =
  CfTypes.newInteger (Intbig.mul (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerDiv a b =
  CfTypes.newInteger (Intbig.div (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerMod a b =
  CfTypes.newInteger (Intbig.modu (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerPow a b =
  CfTypes.newInteger (Intbig.pow (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerNot a =
  CfTypes.newInteger (Intbig.bw_not (CfTypes.getInteger a));;

let integerAnd a b =
  CfTypes.newInteger (Intbig.bw_and (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerXor a b =
  CfTypes.newInteger (Intbig.bw_xor (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerOr  a b =
  CfTypes.newInteger (Intbig.bw_or  (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerShiftLeft a b =
  CfTypes.newInteger (Intbig.shift_left  (CfTypes.getInteger a) (CfTypes.getInteger b));;

let integerShiftRight a b =
  CfTypes.newInteger (Intbig.shift_right  (CfTypes.getInteger a) (CfTypes.getInteger b));;


(** Float primitives. *)

let isFloat a =
  CfTypes.newBoolean (CfTypes.isFloat a);;

let floatToString a =
  let s = string_of_float (CfTypes.getFloat a) in
  let s =
    if String2.right_char s = '.' then
      s ^ "0"
    else
      s
  in
  CfTypes.stringToList s
;;

let stringToFloat str =
  CfTypes.newFloat (float_of_string (CfTypes.listToString str))
;;

let floatToInteger a =
  CfTypes.newInteger (Intbig.intbig_of_float (CfTypes.getFloat a));;

let floatLt a b =
  CfTypes.newBoolean (CfTypes.getFloat a < CfTypes.getFloat b);;

let floatGt a b =
  CfTypes.newBoolean (CfTypes.getFloat a > CfTypes.getFloat b);;

let floatLe a b =
  CfTypes.newBoolean (CfTypes.getFloat a <= CfTypes.getFloat b);;

let floatGe a b =
  CfTypes.newBoolean (CfTypes.getFloat a >= CfTypes.getFloat b);;

let floatAdd a b =
  CfTypes.newFloat (CfTypes.getFloat a +. CfTypes.getFloat b);;

let floatSub a b =
  CfTypes.newFloat (CfTypes.getFloat a -. CfTypes.getFloat b);;

let floatMul a b =
  CfTypes.newFloat (CfTypes.getFloat a *. CfTypes.getFloat b);;

let floatDiv a b =
  CfTypes.newFloat (CfTypes.getFloat a /. CfTypes.getFloat b);;

let floatPow a b =
  CfTypes.newFloat (CfTypes.getFloat a ** CfTypes.getFloat b);;

let floatCeil a =
  CfTypes.newFloat (ceil (CfTypes.getFloat a));;

let floatFloor a =
  CfTypes.newFloat (floor (CfTypes.getFloat a));;

let floatExp a =
  CfTypes.newFloat (exp (CfTypes.getFloat a));;

let floatLog a =
  CfTypes.newFloat (log (CfTypes.getFloat a));;

let floatSqrt a =
  CfTypes.newFloat (sqrt (CfTypes.getFloat a));;

let floatSin a =
  CfTypes.newFloat (sin (CfTypes.getFloat a));;

let floatCos a =
  CfTypes.newFloat (cos (CfTypes.getFloat a));;

let floatTan a =
  CfTypes.newFloat (tan (CfTypes.getFloat a));;

let floatAsin a =
  CfTypes.newFloat (asin (CfTypes.getFloat a));;

let floatAcos a =
  CfTypes.newFloat (acos (CfTypes.getFloat a));;

let floatAtan a =
  CfTypes.newFloat (atan (CfTypes.getFloat a));;

let floatAtan2 a b =
  CfTypes.newFloat (atan2 (CfTypes.getFloat a) (CfTypes.getFloat b));;


(** Boolean primtives. *)

let isBoolean a =
  CfTypes.newBoolean (CfTypes.isBoolean a);;

let booleanNot a =
  CfTypes.newBoolean (not (CfTypes.getBoolean a));;


(** Record primitives. *)

let isRecord a =
  CfTypes.newBoolean (CfTypes.isRecord a)
;;

let recordInfo r =
  CfTypes.recordInfo r
;;


(** List primitives. *)

let isList a =
  CfTypes.newBoolean (CfTypes.isList a)
;;

let listPrint l =
  print_string (CfTypes.listToString l);
  print_newline ()
;;

let argv_position = ref 0;;

let set_argv_position pos = 
  argv_position := pos
;;

let listArgv () =
  let rec f args =
    match args with
      [] -> CfTypes.newRecord 0 [||] [||]
    | arg :: args -> CfTypes.newRecord 2 [|"hd"; "tl"|] [|(CfTypes.stringToList arg); (f args)|]
  in
    f (List2.drop (Array.to_list Sys.argv) !argv_position)
;;

(** Vector primitives. *)

let isVector a =
  CfTypes.newBoolean (CfTypes.isVector a);;

let vectorWidth a =
  CfTypes.newInteger (Intbig.intbig_of_int (CfTypes.getWidth a));;

let vectorClock clockName system =
  Cf_fnf.set_clock_domain (CfTypes.getEnvId (CfTypes.getRenv system)) (CfTypes.listToString clockName) remoteError
;;

let vectorEnable enableVector system =
  CfTypes.checkWidthIs enableVector 1;
  Cf_fnf.add_sub_enable (CfTypes.getEnvId (CfTypes.getRenv system)) (CfTypes.getProducer enableVector) remoteError;;

let vectorReset resetVector system =
  CfTypes.checkWidthIs resetVector 1;
  Cf_fnf.add_sub_reset (CfTypes.getEnvId (CfTypes.getRenv system)) (CfTypes.getProducer resetVector) remoteError;;

let vectorInput sysId name width =
  if CfTypes.getInt width <= 0 then raise (CfTypes.Error "Vector error.  Input width must be greater than 0.");
  let s = CfTypes.newVector (Cf_fnf.new_input sysId (CfTypes.listToString name) (CfTypes.getInt width)) in
  CfTypes.checkWidth s;
  s
;;

let vectorOutput sysId name output =
  CfTypes.checkWidthNotZero output;
  Cf_fnf.new_output sysId (CfTypes.listToString name) (CfTypes.getProducer output)
;;

(*
let vectorAssert sysId msg test = 
  CfTypes.checkWidthIs test 1;
  Cf_fnf.new_assert sysId (CfTypes.getProducer test) (CfTypes.listToString msg)
;;

let vectorPrint sysId enable msg = 
  CfTypes.checkWidthIs enable 1;
  CfTypes.checkWidthNotZero msg;
  Cf_fnf.new_print sysId (CfTypes.getProducer enable) (CfTypes.getProducer msg);;
*)

let vectorConst sysId value width =
  CfTypes.newVectorConst (CfTypes.getInteger value) (CfTypes.getInteger width) sysId;;

let vectorBuf sysId a =
  CfTypes.checkWidthNotZero a;
  CfTypes.newVector (Cf_fnf.new_buf sysId (CfTypes.getProducer a));;

let vectorNot sysId a =
  CfTypes.checkWidthNotZero a;
  CfTypes.newVector (Cf_fnf.new_not sysId (CfTypes.getProducer a));;

let vectorAnd sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_and sysId (CfTypes.getProducer a) (CfTypes.getProducer b));;

let vectorOr sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_or sysId (CfTypes.getProducer a) (CfTypes.getProducer b));;

let vectorXor sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_xor sysId (CfTypes.getProducer a) (CfTypes.getProducer b));;

let vectorConcat sysId a b =
  if CfTypes.is_zero_width_vector a then
    b
  else if CfTypes.is_zero_width_vector b then
    a
  else
    CfTypes.newVector (Cf_fnf.new_concat sysId (CfTypes.getProducer a) (CfTypes.getProducer b))
;;

let vectorZero () =
  CfTypes.zero_width_vector
;;

let vectorSelect sysId input bit =
  let bit = CfTypes.getInt bit in
  let width = CfTypes.getWidth input in
  if bit < 0      then raise (CfTypes.Error "Vector error.  Bit selection requires positive integers.");
  if bit >= width then raise (CfTypes.Error "Vector error.  Bit selection requires integers < width - 1.");
  CfTypes.newVector (Cf_fnf.new_select sysId (CfTypes.getProducer input) bit)
;;

let vectorEqu sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_equ sysId (CfTypes.getProducer a) (CfTypes.getProducer b))
;;

let vectorLt sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_lt sysId (CfTypes.getProducer a) (CfTypes.getProducer b))
;;

let vectorAdd sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_add sysId (CfTypes.getProducer a) (CfTypes.getProducer b));;

let vectorSub sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_sub sysId (CfTypes.getProducer a) (CfTypes.getProducer b));;

let vectorMul sysId a b =
  CfTypes.checkWidthNotZero a;
  CfTypes.checkWidthNotZero b;
  CfTypes.checkWidthsAreSame a b;
  CfTypes.newVector (Cf_fnf.new_mul sysId (CfTypes.getProducer a) (CfTypes.getProducer b));;

let vectorMux sysId pred t f =
  CfTypes.checkWidthIs pred 1;
  CfTypes.checkWidthsAreSame t f;
  CfTypes.checkWidthNotZero t;
  CfTypes.newVector (Cf_fnf.new_mux sysId (CfTypes.getProducer pred) (CfTypes.getProducer t) (CfTypes.getProducer f));;

(*
let vectorExtComb sysId name params dataIn widthOut =
  CfTypes.checkWidthNotZero dataIn;
  if CfTypes.getInt widthOut <= 0 then raise (CfTypes.Error ("VectorExtComb error.  External combinatorial component requires output width > 0."));
  CfTypes.newVector (Cf_fnf.new_ExtComb sysId (CfTypes.listToString name) (CfTypes.listToIntList params) (CfTypes.getProducer dataIn) (CfTypes.getInt widthOut))
;;

let vectorExtSequ sysId name params dataIn widthOut =
  CfTypes.checkWidthNotZero dataIn;
  if CfTypes.getInt widthOut <= 0 then raise (CfTypes.Error ("VectorExtSequ error.  External sequential component requires output width > 0."));
  CfTypes.newVector (Cf_fnf.new_ExtSequ sysId (CfTypes.listToString name) (CfTypes.listToIntList params) (CfTypes.getProducer dataIn) (CfTypes.getInt widthOut))
;;

let vectorExtSoft sysId name params width =
  if CfTypes.getInt width <= 0 then raise (CfTypes.Error ("VectorExtSoft error.  External soft component requires width > 0."));
  CfTypes.newVector (Cf_fnf.new_ExtSoft sysId (CfTypes.listToString name) (CfTypes.listToIntList params) (CfTypes.getInt width))
;;
*)


(* Stateful logic primitives. *)

let vectorRegStmt renv =
  let sysId = CfTypes.getEnvId renv in
  let ports = CfTypes.getRenvValues renv in
  let width   = ports.(0) in
  let dataIn  = ports.(1) in
  let dataOut = ports.(2) in
  CfTypes.setVariableDependence dataOut [width; dataIn];
  let slot = CfTypes.newSlot renv 1 (fun () ->
    let width = CfTypes.getInt width in
    CfTypes.unify dataOut (CfTypes.newVector (Cf_fnf.new_state sysId width));
    CfTypes.checkWidthNotZero dataOut;
    let slot = CfTypes.newSlot renv 1 (fun () ->
      CfTypes.checkWidthsAreSame dataIn dataOut;
      Cf_fnf.new_reg sysId (CfTypes.getProducer dataIn) (CfTypes.getProducer dataOut);
    ) in
    CfTypes.slotVariableStrict slot dataIn
  ) in
  CfTypes.slotVariableStrict slot width
;;

let vectorBboxStmt renv =
  let sysId = CfTypes.getEnvId renv in
  let ports = CfTypes.getRenvValues renv in
  let name    = ports.(0) in
  let width   = ports.(1) in
  let params  = ports.(2) in
  let dataIn  = ports.(3) in
  let dataOut = ports.(4) in
  CfTypes.setVariableDependence dataOut [name; width; params; dataIn];
  let slot = CfTypes.newSlot renv 1 (fun () ->
    let width = CfTypes.getInt width in
    CfTypes.unify dataOut (CfTypes.newVector (Cf_fnf.new_state sysId width));
    CfTypes.checkWidthNotZero dataOut;
    let slot = CfTypes.newSlot renv 3 (fun () ->
      CfTypes.checkWidthNotZero dataIn;
      Cf_fnf.new_bbox sysId (CfTypes.listToString name) (CfTypes.listToIntList params) (CfTypes.getProducer dataIn) (CfTypes.getProducer dataOut);
    ) in
    CfTypes.slotVariableStrict slot name;
    CfTypes.slotVariableStrict slot params;
    CfTypes.slotVariableStrict slot dataIn
  ) in
  CfTypes.slotVariableStrict slot width
;;


(*
let bits a =
  if a < 0 then raise (CfTypes.Error "Bits: Count must be >= 0.")
  else if a = 0 then 0
  else if a = 1 then 1
  else
    int_of_float (ceil (log (float_of_int a) /. log 2.0))
;;

let vectorRomStmt renv =
  let sysId = CfTypes.getEnvId renv in
  let ports = CfTypes.getRenvValues renv in
  let width  = ports.(0) in
  let values = ports.(1) in
  let addr   = ports.(2) in
  let data   = ports.(3) in
  CfTypes.setVariableDependence data [width; values; addr];
  let slot = CfTypes.newSlot renv 1 (fun () ->
    let width = CfTypes.getInt width in
    CfTypes.unify data (CfTypes.newVector (Cf_fnf.new_State sysId width));
    CfTypes.checkWidthNotZero data;
    let slot = CfTypes.newSlot renv 2 (fun () ->
      let values = CfTypes.listToIntegerList values in
      let depth = List.length values in
      let addrWidth = CfTypes.getWidth addr in
      let dataWidth = CfTypes.getWidth data in
      if addrWidth < bits depth then raise (CfTypes.Error ("VectorRom error.  Expecting address width of greater than or equal to " ^ string_of_int (bits depth) ^ ".  Got " ^ string_of_int addrWidth ^ "."));
      let values = values @ List2.make (int_of_float (2.0 ** float_of_int addrWidth -. float_of_int depth)) Intbig.zero in
      CfTypes.checkWidthNotZero addr;
      Cf_fnf.new_Rom sysId dataWidth values (CfTypes.getProducer addr) (CfTypes.getProducer data)
    ) in
    CfTypes.slotVariableStrict slot values;
    CfTypes.slotVariableStrict slot addr;
  ) in
  CfTypes.slotVariableStrict slot width
;;
      
let vectorRamStmt renv =
  let sysId = CfTypes.getEnvId renv in
  let ports = CfTypes.getRenvValues renv in
  let width  = ports.(0) in
  let values = ports.(1) in
  let write  = ports.(2) in
  let read   = ports.(3) in
  CfTypes.setVariableDependence write [width; values];
  CfTypes.setVariableDependence read  [width; values];
  let slot = CfTypes.newSlot renv 2 (fun () ->
    let width = CfTypes.getInt width in
    let values = CfTypes.listToIntegerList values in
    let depth = List.length values in
    let addrWidth = bits depth in
    let values = values @ List2.make (int_of_float (2.0 ** float_of_int addrWidth -. float_of_int depth)) Intbig.zero in
    let memory = Cf_fnf.new_RamMem sysId width values in
    let writeStmt renv =
      let sysId = CfTypes.getEnvId renv in
      let ports = CfTypes.getRenvValues renv in
      let addr    = ports.(0) in
      let data    = ports.(1) in
      CfTypes.setVariableDependence data [addr];
      let slot = CfTypes.newSlot renv 2 (fun () ->
        CfTypes.checkWidthIs addr addrWidth;
        CfTypes.checkWidthIs data width;
        let addr   = CfTypes.getProducer addr in
        let data   = CfTypes.getProducer data in
        Cf_fnf.new_RamWrite sysId memory addr data;
      ) in
      CfTypes.slotVariableStrict slot addr;
      CfTypes.slotVariableStrict slot data;
    in
    let readStmt renv =
      let sysId = CfTypes.getEnvId renv in
      let ports = CfTypes.getRenvValues renv in
      let rbw    = ports.(0) in
      let addr   = ports.(1) in
      let data   = ports.(2) in
      CfTypes.setVariableDependence data [rbw; addr];
      CfTypes.unify data (CfTypes.newVector (Cf_fnf.new_State sysId width));
      let slot = CfTypes.newSlot renv 2 (fun () ->
        CfTypes.checkWidthIs addr addrWidth;
        CfTypes.checkWidthIs data width;
        let rbw    = CfTypes.getBoolean rbw in
        let addr   = CfTypes.getProducer addr in
        let data   = CfTypes.getProducer data in
        Cf_fnf.new_RamRead sysId memory rbw addr data;
      ) in
      CfTypes.slotVariableStrict slot rbw;
      CfTypes.slotVariableStrict slot addr;
    in
    CfTypes.unify write (CfTypes.newComp (renv, Loc.unknown "Internal RamWrite", 2, [|"addr"; "data"|], writeStmt));
    CfTypes.unify read  (CfTypes.newComp (renv, Loc.unknown "Internal RamRead", 3, [|"read_before_write"; "addr"; "data"|], readStmt))
  ) in
  CfTypes.slotVariableStrict slot width;
  CfTypes.slotVariableStrict slot values;
;;
*)


(** Properties. *)

let property p =
  if CfTypes.isVector p then CfTypes.newPropertyVector p else p
;;

let isProperty p =
  CfTypes.newBoolean (CfTypes.isVector p && CfTypes.getWidth p = 1 || CfTypes.isProperty p)
;;

let propertyNot p =
  CfTypes.newPropertyNot (property p)
;;

let propertyOr p0 p1 =
  CfTypes.newPropertyOr (property p0) (property p1)
;;

let propertyNext p =
  CfTypes.newPropertyNext (property p)
;;

let propertyUntil p0 p1 =
  CfTypes.newPropertyUntil (property p0) (property p1)
;;



(* Primitive selector. *)

let compilePrimitive primName arity =
  try
    let prim = fromString primName
    in
      if arity  <> totalArity prim then raise (CfTypes.Error ("Illegal number of primitive ports.  " ^ primName ^ " needs " ^ string_of_int (totalArity prim) ^ ", not " ^ string_of_int arity));
      begin
        match prim with
        | IsEqual           -> isEqual
        | Error             -> stmt_of_prim_s  error
        | IsComponent       -> stmt_of_prim_lx isComponent

        | IsSystem          -> stmt_of_prim_lx isSystem
        | SystemPorts       -> stmt_of_prim_lx systemPorts

        | IsInteger         -> stmt_of_prim_lx  isInteger
        | IntegerToString   -> stmt_of_prim_lx  integerToString
        | StringToInteger   -> stmt_of_prim_sx  stringToInteger
        | IntegerToFloat    -> stmt_of_prim_lx  integerToFloat
        | IntegerLt         -> stmt_of_prim_llx integerLt
        | IntegerGt         -> stmt_of_prim_llx integerGt
        | IntegerLe         -> stmt_of_prim_llx integerLe
        | IntegerGe         -> stmt_of_prim_llx integerGe
        | IntegerAdd        -> stmt_of_prim_llx integerAdd
        | IntegerSub        -> stmt_of_prim_llx integerSub
        | IntegerMul        -> stmt_of_prim_llx integerMul
        | IntegerDiv        -> stmt_of_prim_llx integerDiv
        | IntegerMod        -> stmt_of_prim_llx integerMod
        | IntegerPow        -> stmt_of_prim_llx integerPow
        | IntegerNot        -> stmt_of_prim_lx  integerNot
        | IntegerAnd        -> stmt_of_prim_llx integerAnd
        | IntegerXor        -> stmt_of_prim_llx integerXor
        | IntegerOr         -> stmt_of_prim_llx integerOr
        | IntegerShiftLeft  -> stmt_of_prim_llx integerShiftLeft
        | IntegerShiftRight -> stmt_of_prim_llx integerShiftRight

        | IsFloat           -> stmt_of_prim_lx  isFloat
        | FloatToString     -> stmt_of_prim_lx  floatToString
        | StringToFloat     -> stmt_of_prim_sx  stringToFloat
        | FloatToInteger    -> stmt_of_prim_lx  floatToInteger
        | FloatLt           -> stmt_of_prim_llx floatLt
        | FloatGt           -> stmt_of_prim_llx floatGt
        | FloatLe           -> stmt_of_prim_llx floatLe
        | FloatGe           -> stmt_of_prim_llx floatGe
        | FloatAdd          -> stmt_of_prim_llx floatAdd
        | FloatSub          -> stmt_of_prim_llx floatSub
        | FloatMul          -> stmt_of_prim_llx floatMul
        | FloatDiv          -> stmt_of_prim_llx floatDiv
        | FloatPow          -> stmt_of_prim_llx floatPow
        | FloatCeil         -> stmt_of_prim_lx  floatCeil
        | FloatFloor        -> stmt_of_prim_lx  floatFloor
        | FloatExp          -> stmt_of_prim_lx  floatExp
        | FloatLog          -> stmt_of_prim_lx  floatLog
        | FloatSqrt         -> stmt_of_prim_lx  floatSqrt
        | FloatSin          -> stmt_of_prim_lx  floatSin
        | FloatCos          -> stmt_of_prim_lx  floatCos
        | FloatTan          -> stmt_of_prim_lx  floatTan
        | FloatAsin         -> stmt_of_prim_lx  floatAsin
        | FloatAcos         -> stmt_of_prim_lx  floatAcos
        | FloatAtan         -> stmt_of_prim_lx  floatAtan
        | FloatAtan2        -> stmt_of_prim_llx floatAtan2

        | IsBoolean         -> stmt_of_prim_lx isBoolean
        | BooleanNot        -> stmt_of_prim_lx booleanNot

        | IsRecord          -> stmt_of_prim_lx isRecord
        | RecordInfo        -> stmt_of_prim_sx recordInfo

        | IsList            -> stmt_of_prim_lx isList
        | ListPrint         -> stmt_of_prim_s  listPrint
        | ListArgv          -> stmt_of_prim_x  listArgv

        | IsVector          -> stmt_of_prim_lx     isVector
        | VectorWidth       -> stmt_of_prim_lx     vectorWidth
        | VectorClock       -> stmt_of_prim_sl     vectorClock
        | VectorEnable      -> stmt_of_prim_ll     vectorEnable
        | VectorReset       -> stmt_of_prim_ll     vectorReset
        | VectorInput       -> stmt_of_prim_rslx   vectorInput
        | VectorOutput      -> stmt_of_prim_rsl    vectorOutput
        (*
        | VectorAssert      -> stmt_of_prim_rsl    vectorAssert
        | VectorPrint       -> stmt_of_prim_rll    vectorPrint
        *)
        | VectorConst       -> stmt_of_prim_rllx   vectorConst
        | VectorAnd         -> stmt_of_prim_rllx   vectorAnd
        | VectorOr          -> stmt_of_prim_rllx   vectorOr
        | VectorXor         -> stmt_of_prim_rllx   vectorXor
        | VectorNot         -> stmt_of_prim_rlx    vectorNot
        | VectorConcat      -> stmt_of_prim_rllx   vectorConcat
        | VectorZero        -> stmt_of_prim_x      vectorZero
        | VectorSelect      -> stmt_of_prim_rlsx   vectorSelect
        | VectorEqu         -> stmt_of_prim_rllx   vectorEqu
        | VectorLt          -> stmt_of_prim_rllx   vectorLt
        | VectorAdd         -> stmt_of_prim_rllx   vectorAdd
        | VectorSub         -> stmt_of_prim_rllx   vectorSub
        | VectorMul         -> stmt_of_prim_rllx   vectorMul
        | VectorMux         -> stmt_of_prim_rlllx  vectorMux
        (*
        | VectorTristate    -> stmt_of_prim_rllx   vectorTristate
        | VectorExtComb     -> stmt_of_prim_rssllx vectorExtComb
        | VectorExtSequ     -> stmt_of_prim_rssllx vectorExtSequ
        | VectorExtSoft     -> stmt_of_prim_rsslx  vectorExtSoft
        *)
        | VectorReg         -> vectorRegStmt
        | VectorBbox        -> vectorBboxStmt
        (*
        | VectorRom         -> vectorRomStmt
        | VectorRam         -> vectorRamStmt
        *)

        | IsProperty        -> stmt_of_prim_lx  isProperty
        | PropertyNot       -> stmt_of_prim_lx  propertyNot
        | PropertyOr        -> stmt_of_prim_llx propertyOr
        | PropertyNext      -> stmt_of_prim_lx  propertyNext
        | PropertyUntil     -> stmt_of_prim_llx propertyUntil

      end
  with CfPrimsError ->
    raise (CfTypes.Error ("Unknown primitive: " ^ primName));;







