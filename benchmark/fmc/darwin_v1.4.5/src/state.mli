(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005, 2006
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


(** choice point (decision level) management

  A {e choice point} represents/manages a Split decision (plus subsequent Propagations)
  and the dependencies (explanation) of this choice point on other choice points
  (see the Alexander Fuchs' master thesis for a longer explanation).

  The choice points are ordered in a sequence
  which represents the current derivation branch.
  The first choice point of the sequence (the {e root choice point})
  is not really a choice point,
  as it is not created by a split decision,
  and merely extended by the assertion of the unit clauses of the input clause set.
  Left Splits create new choice points,
  Right Splits, Unit Splits, and Asserts work on the current choice point,
  i.e. the most recently created choice point (the {e active choice point}).

  A choice point is {e older} than another choice point
  if it comes earlier in the branch,
  that is the root choice point is the oldest one.

  If a choice point becomes {e invalid} (i.e. retracted in backtracking),
  all older choice points, and all dependent context literals and remainders
  become also invalid (transitive closure).
*)



(** {6 Types} *)

type config = Config.config
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst

(** see module description. *)
type choice_point

(** the explanation of a choice point are the choice points it depends on. *)
type explanation = choice_point list

(** inference rule which produced the context literal. *)
type literal_type =
  | Propagation
      (** assert or unit split. *)
  | SplitLeft
      (** left split. *)
  | SplitRight
      (** right split. *)

(** the information associated with a context literal needed
    for backtracking, dependency analysis, lemma generation, ...

    the implication graph of the context literals
    is given by the link [li_literal] -> [li_context_literals] *)
type literal_info = private {
  li_literal: literal;
  (** the context literal. *)

  li_type: literal_type;
  (** type of context literal *)

  li_choice_point: choice_point;
  (** the choice point in which the literal was added to the context. *)

  (** the following basically encodes the context unifier. *)

  li_context_unifier: subst;
  (** the actual context unifier *)

  li_clause: clause;
  (** the context unifier is based on this clause *)

  li_constrained: clause;
  (** and this is the constraint part of the clause (see {!Const.fd_constraint_solver}). *)

  li_clause_vars: Subst.var list;
  (** the variables occuring in the clause *)

  li_context_literals: literal array;
  (** ... these context literals.
      for a left split this is the empty array,
      for a right split the explanation of the left split,
      and for a propagation the context literals used in the context unifier. *)

  li_clause_index: int;
  (** ... with the clause literal at this position instantiated to [li_literal],
      counting from 0 *)

  li_clause_utility: unit -> unit;
  (** called when the clause contributes (directly or indirectly by dependencies)
      to a closing context unifier. supposed to increase the utility value of the clause
      for further candidate selection. *)

  mutable li_explanation: explanation option;
  (** the explanation of this context literal based on bn_context_literals,
      to be memoized after the first computation.  *)
}


(** manages a derivation branch, i.e. a sequence of choice points,
    and their dependencies, via a set of branch_nodes *)
type state




(** {6 Functions} *)


(** {2 Constants} *)

(** a valid choice_point. *)
val valid_choice_point: choice_point

(** an invalid choice point. *)
val invalid_choice_point: choice_point



(** {2 Creation } *)

(** creates a [literal_info] value. *)
val create_literal_info: literal -> literal_type -> choice_point ->
  subst -> clause -> clause -> Subst.var list -> literal array -> int -> (unit -> unit) ->
  explanation option -> literal_info



(** {2 Access } *)

(** returns the root choice point. *)
val root_choice_point: state -> choice_point

(** returns the active choice point,
    i.e. the most recently created (not backtracked) one. *)
val active_choice_point: state -> choice_point

(** returns the left split literal used to create this choice point. *)
val left_split_literal: choice_point -> literal

(** returns the right split literals applied in this choice point
    in order of application. *)
val right_split_literals: choice_point -> literal list

(** returns the current derivation branch,
    i.e. the current valid choice points in reversed order of application
    (active one first) *)
val get_branch: state -> choice_point list

(** returns the information associated with this context literal.
    @raise Not_found if this context literal is not part of the derivation. *)
val get_literal_info: state -> literal -> literal_info




(** {2 Application } *)

(** creates a new derivation branch. *)
val create: config -> state

(** [apply_split_left state context_literal context_unifier clause clause_vars clause_index utility_function]
    creates a new choice point initiated by a left split on the new [context_literal].

    the context literal has been computed with some context literals
    against [clause] at position [clause_index] within [clause].
    see {!State.literal_info}.[li_clause_utility] for [utility_function].
*)
val apply_split_left: state -> literal -> subst -> clause -> clause -> Subst.var list -> int -> (unit -> unit) -> choice_point

(** [apply_split_right state context_literal context_unifier clause clause_index utility_function explanation]
    is similar to {!State.apply_split_left}, but applies a right split.

    Therefore, no new choice point is created,
    and the right split depends on other context literals which are given in [explanation].
*)
val apply_split_right: state -> literal -> subst -> clause -> clause -> Subst.var list -> int -> (unit -> unit) -> literal array -> unit

(** [apply_propagation state context_literal context_unifier clause clause_index utility_function context_literals]
    is similar to {!State.apply_split_right}, but applies an assert or unit split.

    The explanation is implicitely given by the [context_literals]
    used to compute the assert/unit split literal.
*)
val apply_propagation: state -> literal -> subst -> clause -> clause -> Subst.var list -> int -> (unit -> unit) -> literal array -> unit



(** {2 Manipulation } *)

(** invalidates a choice point. *)
val invalidate_choice_point: choice_point -> unit

(** replaces the current derivation branch with this choice point list.
    must be in chronological order of creation,
    and all choice points must be valid. *)
val set_branch: state -> choice_point list -> unit

(** removes all literal_info entries which have been retracted,
    i.e. which correspond to invalid states. *)
val backtrack_literal_info: state -> unit



(** {2 Properties } *)

(** the id of the choice point as used by {!State.compare_age} *)
val id_of_choice_point: choice_point -> int

(** is this choice point valid? *)
val is_choice_point_valid: choice_point -> bool

(** is this choice point invalid? *)
val is_choice_point_invalid: choice_point -> bool

(** computes and returns the explanation of a set of context literals. *)
val get_explanation: state -> literal array -> choice_point list



(** {2 Comparison} *)

(** are two choice points equal? *)
val choice_point_equal: choice_point -> choice_point -> bool

(** compares the age of two choice points.
  returns
    - -1 if the first choice point is older,
    - 1 if the second is older,
    - 0 if the choice points are identical *)
val compare_age: choice_point -> choice_point -> int

(** depends the first choice point on the second int the sense that,
    if the second is retracted in backtracking,
    is the first retrackted as well? *)
val backtracking_depends_on: choice_point -> choice_point -> bool







(** {2 Representation} *)

(** string representation of a choice point, e.g. [0]. *)
val choice_point_to_string: choice_point -> string

(** string representation of an explanation, e.g. [0, 2]. *)
val explanation_to_string: explanation -> string

(** if used with the id of a choice point this give the same representation
    as using {!State.choice_point_to_string} directly. *)
val id_to_string: int -> string

(** string representation of the active choice point.
    Shortcut for {!State.choice_point_to_string} {!State.active_choice_point}. *)
val active_choice_point_to_string: state -> string

(** extended string representation of the derivation branch,
    including validity and dependency information. *)
val branch_to_string: state -> string

