(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005
              The University of Iowa
              Universitaet Koblenz-Landau 

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)


(** universal and parameteric variables

  {e Variable} is an ambiguous term in the context of the model evolution calculus,
  as there exist the usual {e universal} variables,
  and special variables called {e parameters}.
  Thus variables might be used to talk only about universal variables,
  but also to talk about universal variables and parameters.
  This ambiguity is from case to case clarified by context or explicit naming.

  Note: the substitution tree module is not supported for the time being.
  The term indexing provided by the module [Substitution_tree]
  needs another type differentiation.
  Here, a variable may also be an {e indicator} variable.
  This is orthogonal to {e universal} and {e parametric}.

  A variable is identified by an identification number.
  Thus two variables are considered to be equal
  if they are of the same type (universal or parameteric, indicator or non-indicator)
  and if they have the same id.
*)

(** {6 Types} *)

(** a universal variable or a parameter *)
type var

(** a specialized [Hashtbl] with variables as keys. *)
module VarTable : Hashtbl.S with type key = var


(** {6 Functions} *)


(** {2 Creation} *)


(** creation of a universal variable by id. *)
val create_universal: int -> var

(** creation of a parametric variable by id. *)
val create_parametric: int -> var
  
(** creation of a universal indicator variable by id. *)
(*val create_universal_indicator: int -> var*)

(** creation of a parametric indicator variable by id. *)
(*val create_parametric_indicator: int -> var*)

(** [Var.clone_renumbered var id] creates a variable of the same type 
  (i.e. variable or parameter) as [var] with the new id [id].

  (Just a shortcut for the combination of
  {!Var.is_universal} with {!Var.create_universal}
  resp. {!Var.is_parametric} with {!Var.create_parametric}).
  (Used e.g. in term normalization).
*)
val clone_renumbered: var -> int -> var

(** transforms [var] into an indicator variable. *)
(*val clone_as_indicator: var -> var*)

(** transforms [var] into a non-indicator variable. *)
(*val clone_as_non_indicator: var -> var*)

(** {2 Decomposition} *)

(** retrieval of a variable's id. *)
val id_of_var: var -> int

(** a good hash value for a variable. *)
val hash_of_var: var -> int


(** is this a universal variable? *)
val is_universal: var -> bool

(** is this a parametric variable? *)
val is_parametric: var -> bool

(** is this an indicator variable? *)
(*val is_indicator: var -> bool*)


(** {2 Comparison} *)

(** see module description above. *)
val equal: var -> var -> bool

(** total order. *)
val compare: var -> var -> int

(** {2 Representation} *)

(** the string representation of [var] with [id] is
- {b _}[id] for a universal variable
- {b =}[id] for a parameter
and a {b *} as an additional prefix for an indicator variable. *)
val to_string: var -> string
