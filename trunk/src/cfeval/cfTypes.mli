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

(** Datatypes and Runtime Tools *)


(** Exceptions *)

exception Error of string;;

(** Types *)

type variable;;
type renv;;


(** Runtime Error Reporting *)

(** Sets current location and environment. *)
val setLocation : Loc.loc -> unit;;

(** Specifies a variable dependency. *)
val setVariableDependence : variable -> variable list -> unit;;

(** Reports an error with an instantiation trace. *)
val error       : string -> unit;;

val typeToString : variable -> string;;

(** Task Queue *)

(** Sends a task into the ready queue. *)
val readyTask : renv * (unit -> unit) -> unit;;

(** Executues tasks in queue. *)
val executeTasks : unit -> unit;;


(** Sync slots. *)

type slot;;

(** Creates a new slot. *)
val newSlot : renv -> int -> (unit -> unit) -> slot;;

(** Syncs a slot. *)
val sync : slot -> unit;;

(** Increments slots counter. *)
val incrSlot : slot -> unit;;

(** Slots a variable.  Records must me complete. *)
val slotVariableStrict : slot -> variable -> unit;;

(** Slots a variable.  Records may be incomplete. *)
val slotVariableLenient : slot -> variable -> unit;;


(** Unifies two variables. *)
val unify : variable -> variable -> unit;;


(** Compares two variables. *)
val isEqual : renv -> variable -> variable -> variable -> unit;;


(** Free Values *)

val newFree    : unit -> variable;;
val newFreeLoc : Loc.loc -> string -> renv -> variable;;
val newFreeWithTask : renv -> (variable -> unit) -> variable;;


(** Integers *) 

val newInteger : Intbig.intbig -> variable;;
val isInteger  : variable -> bool;;
val getInteger : variable -> Intbig.intbig;;
val getInt     : variable -> int;;


(** Floats *)

val newFloat   : float -> variable;;
val isFloat    : variable -> bool;;
val getFloat   : variable -> float;;


(** Booleans *)

val newBoolean : bool -> variable;;
val isBoolean  : variable -> bool;;
val getBoolean : variable -> bool;;


(** Vectors *)

val zero_width_vector    : variable;;
val is_zero_width_vector : variable -> bool;;
val checkBits            : string -> unit;;
val newVector            : Cf_fnf.producer -> variable;;
val newVectorConst       : Intbig.intbig -> Intbig.intbig -> Cf_fnf.system -> variable;;
val isVector             : variable -> bool;;
val getWidth             : variable -> int;;
val getProducer          : variable -> Cf_fnf.producer;;
val checkWidth           : variable -> unit;;
val checkWidthNotZero    : variable -> unit;;
val checkWidthIs         : variable -> int -> unit;;
val checkWidthsAreSame   : variable -> variable -> unit;;


(** Properties *)

val isProperty        : variable -> bool;;
val newPropertyVector : variable -> variable;;
val newPropertyNot    : variable -> variable;;
val newPropertyOr     : variable -> variable -> variable;;
val newPropertyNext   : variable -> variable;;
val newPropertyUntil  : variable -> variable -> variable;;


(** Records *)

val newRecord       : int -> string array -> variable array -> variable;;
val isRecord        : variable -> bool;;
val recordInfo      : variable -> variable;;
val getFieldByName  : variable -> string -> variable;;
val getFieldByIndex : variable -> int -> variable;;


(** Lists *)

val isList            : variable -> bool;;
val listToString      : variable -> string;;
val stringToList      : string -> variable;;
val listToIntegerList : variable -> Intbig.intbig list;;
val listToIntList     : variable -> int list;;


(** Components *)

val newComp     : (renv * Loc.loc * int * string * string array * (renv -> unit)) -> variable;;
val isComp      : variable -> bool;;
val getCompInfo : variable -> (renv * Loc.loc * int * string * string array * (renv -> unit))


(** Systems *)

val newSystem   : renv -> variable;;
val isSystem    : variable -> bool;;
val getRenv     : variable -> renv;;
val getPorts    : variable -> variable;;


(** Runtime environment (renv). *)

val getEnvId       : renv -> Cf_fnf.system;;
val newEnvRoot     : unit -> renv;;
val extendEnv      : renv -> Loc.loc -> (renv * Loc.loc * int * string * string array * 'a) -> string -> renv;;
val getRelativeEnv : renv -> int -> renv;;
val getRenvValues  : renv -> variable array;;


(** Check all ports determined. *)

val checkAllPortsDetermined : unit -> unit;;

