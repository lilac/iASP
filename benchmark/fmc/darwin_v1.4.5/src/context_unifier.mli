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


(** basic types for and operations on context unifiers

    contains also the types for the search for context unifiers,
    but this is actually done in
    {!Context_unifier_space} and {!Context_unifier_search}.
*)


(** {6 Types} *)

type var = Var.var
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type choice_point = State.choice_point
type state = State.state
type 'a stack = 'a Stack.stack

(** when a literal is added to the context
    all or only specific context unifiers can be computed. *)
type candidate_type =
  | Close
      (** compute only Close context unifiers. *)

  | Assert
      (** compute only Assert context unifiers. *)
      
  | Split
      (** compute only Split context unifiers. *)

  | CloseAssert
      (** compute only Close and Assert context unifiers. *)

  | All
      (** compute all context unifiers. *)

(** a context literal paired to an input clause literal. *)
type context_partner = private {
  cp_element: Context.element;
  (** the context literal. *)

  cp_partial_context_unifier: subst option;
  (** the partial context unifier,
      has to be recomputed with the paired clause literal,
      if not stored (memory explosion). *)

  cp_empty_remainder: bool;
  (** has this partial context unifier an empty remainder,
      i.e. no parameter is mapped to a non-parameter? *)
}

(** the context literals paired with an input clause,
    i.e. a {!Context_unifier.context_unifier_space}.
    A {!Context_unifier.input_partner} is paired with the context literal
    at [context_partners.(input_partner.ip_index)].

    In an earlier implementation this ensured
    that [context_partners] remains valid,
    even if the [cus_input_partners] of a [context_unifier_space],
    i.e. the literals of a clause, are reordered.
    Now [cus_input_partners] are not reordered directly,
    this is done indirectly by [cus_input_partners_ordering].
*)
type context_partners =
    context_partner array


(** a literal of an input clause, with some properties
    and the results of its unification with all context literals. *)
type input_partner = private {
  ip_index: int;
  (** the literal is the i.th's literal of the clause
    (the actual order in the datastructure is dynamic during the computation) *)

  ip_literal: literal;
  (** the input literal *)

  ip_vars: Subst.var list;
  (** the variables of the literal *)

  mutable ip_context_partners: context_partner stack;
  (** all current context literals (negated) unifiable with this input literal *)

  mutable ip_resolved: context_partner option;
  (** the context literal resolving this input literal. *)
}

(** represents an input clause and its relationship with the context,
i.e. unifiable literal pairs, ...

  See {!Context_unifier.context_unifier_space}. *)
type context_unifier_space = private {
  cus_id: int;
  (** unique id per clause *)

  cus_clause: clause;
  (** the input clause *)

  cus_lemma: bool;
  (** is this a lemma, i.e. a clause learnt during the derivation? *)

  mutable cus_lemma_learned: int;
  (** this clause has been learned this many times as a lemma.
      this is counted for a clause and a lemma.
      see {!Config.lemma_parametric_assert}. *)

  cus_vars: Subst.var list;
  (** the variables occuring in the clause. *)

  cus_shared_vars: Subst.var list;
  (** the variables occuring in more than one clause literal. *)

  cus_local_vars: Subst.var list;
  (** the variables occuring only in one clause literal. *)

  cus_input_partners: input_partner array;
  (** the clause literals as input_partners. *)

  cus_input_partners_ordering: int array;
  (** a reordering laid over the input partners,
      in order of decreasing number of context partners per input partner.
      context unifiers are computing in this order. *)

  cus_process_close_candidate: (raw_context_unifier -> unit);
  (** to be called on each found closing context unifier
     (see {!Context_unifier_check.check_close}). *)

  cus_process_assert_candidate: (raw_context_unifier -> bool);
  (** to be called on each found assert candidate
     (see {!Context_unifier_check.check_assert}). *)

  cus_process_split_candidate: (raw_context_unifier -> unit);
  (** to be called on each found split candidate
      (see {!Context_unifier_check.check_split}). *)

  cus_is_element_incomplete_assert: Context.element -> bool;
  (** checks with {!Selection_assert.is_element_incomplete}
      if this element is already incomplete. *)

  cus_is_element_incomplete_split: Context.element -> bool;
  (** checks with {!Selection_split.is_element_incomplete}
      if this element is already incomplete. *)

  cus_utility: int ref;
  (** the utility of this clause, i.e. its value in found conflicts. *)

  cus_constraints: Finite_domain_constraints.constraints option;
  (** finite domain: equality/domain restriction
      (see {!Const.fd_constraint_solver}). *)
}


(** the minimal information needed to recompute a context unifier.

  See {!Context_unifier.context_unifier_space}. *)
and raw_context_unifier = private {
  rcu_space: context_unifier_space;
  (** the clause of the context unifier. *)

  rcu_context_partners: context_partners;
  (** the context literals of the context unifier. *)

  rcu_exceeding_complexity: bool;
  (** do all applicable Split/Assert literals of this context unifier
      exceed the current depth bound? ({!Bound}). *)

  (** constraint solution in finite domain mode,
      see Const.fd_constraint_solver *)
  rcu_constraints: Finite_domain_constraints.solution option;
}



(** refutation found *)
exception UNSATISFIABLE

(** raised if {e Close} is triggered by the given closing context unifier. *)
exception CLOSE of raw_context_unifier



(** {6 Functions} *)




(** {2 Constants} *)

(** used for initialization. *)
val null_partner: context_partner

(** used as the gap marker in an 'assert context unifier'. *)
val assert_partner: context_partner

(** used for initialization. *)
val null_context_unifier: raw_context_unifier
  


(** {2 Creation} *)

(** [create_context_unifier rcu_space rcu_context_partners rcu_exceeding_complexity rcu_constraints] *)
val create_context_unifier: context_unifier_space -> context_partners -> bool -> Finite_domain_constraints.solution option ->raw_context_unifier

(** [create_context_partner cp_element cp_p_preserving cp_element] *)
val create_context_partner: Context.element -> subst option -> bool -> context_partner

(** [create_input_partner ip_index ip_literal ip_vars ip_context_partners] *)
val create_input_partner: int -> literal -> Subst.var list -> input_partner

(** [create_space cus_id cus_clause cus_vars cus_shared_vars cus_local_vars cus_input_partners cus_input_partners_ordering cus_process_close_candidate cus_process_assert_candidate cus_process_split_candidate cus_is_element_incomplete_assert cus_is_element_incomplete_split cus_lemma sorts]
    
    if sorts is given, and Const.fd_constraint_solver is true,
    then the equality/domain restriction literals are removed
    from 
*)
val create_space: int -> clause -> Subst.var list -> Subst.var list -> Subst.var list -> input_partner array -> int array ->
  (raw_context_unifier -> unit) ->
  (raw_context_unifier -> bool) ->
  (raw_context_unifier -> unit) ->
  (Context.element -> bool) ->
  (Context.element -> bool) ->
  bool ->
  Finite_domain_constraints.constraints option ->
  context_unifier_space
  


(** {2 Modification} *)

(** set the [input_partner] as resolved by the [context_partner]. *)
val set_resolved: input_partner -> context_partner option -> unit

(** register that this clause has been learned as a lemma. *)
val increase_lemma_learned: context_unifier_space -> unit



(** {2 Misc} *)

(** has the raw_context_unifier become invalid after backtracking? *)
val is_raw_context_unifier_invalid: raw_context_unifier -> bool

(** [extend_partial_context_unifier partial_context_unifier input_partner context_partner]
    extends the [partial_context_unifier] with the partial context unifier between
    [input_partner] and [context_partner].
    for lemmas only matching instead of unification is used,
    thus missing some context unifiers, but being faster.

    May change [partial_context_unifier].
*)
val extend_partial_context_unifier: recompute:bool -> p_preserving:bool -> subst -> input_partner -> context_partner -> subst

(** recomputes the substitution of the [raw_context_unifier].
    equivalent to using {!Context_unifier.extend_partial_context_unifier} on all
    clause / context literal pairings.

    this is not a full context unifier as in the ME paper,
    it might miss bindings of clause literal variables,
    it is only guaranteed to instantiate from the clause literals
    to the actual context unifier instance literals (remainder and co).
*)
val recompute_unifier: recompute:bool -> raw_context_unifier -> subst

(** recomputes the full context unifier in the ME paper sense. *)
val recompute_full_unifier: recompute:bool -> raw_context_unifier -> subst


(** [is_remainder context_unifier context_literal_parameters input_literal_index].
  does the context literal,
  which contains the parameters [context_literal_parameters],
  and is paired with the [input_literal_index].th input literal
  in the [context_unifier],
  produce a remainder literal?
  I.e., is one of its parameters bound to a non-parameter? *)

(** [is_remainder subst element offset]
    checks if the substitution maps a parameter contained in [element]
    with the given [offset] to a non-parameter. *)
val is_remainder: subst -> Context.element -> int -> bool

(** checks if the substitution maps a parameter to a non-parameter. *)
val is_remainder': subst -> bool

(** maps each clause / context literal pairing to true,
    if it corresponds to a remainder literal,
    false otherwise.
    the order follows the one given in {!Context_unifier.context_partners}.
*)
val compute_remainder_states: raw_context_unifier -> subst -> bool array

(** depth of the dependency tree of the context literals
    needed to compute this context unifier,
    i.e. 'how deep' in the derivation this candidate has been computed. *)
val generation_of_candidate: context_partners -> int

(** the context literals of the context partners
    in the same order as paired to the clause
    in its initial order. *)
val get_context_literals: context_partners -> literal array

(** the choice point in which this unifier has been computed *)
val get_creation_choice_point: context_partners -> choice_point

(** the most recent (youngest) context element used to compute the context unifier.
    that is, the one which triggered its computation. *)
val creating_context_element: raw_context_unifier -> Context.element

(** [is_permanently_blocking blocking blocked]
    checks if the candidate represented by [blocked]
    is always present as long as the candidate represented by [blocking] is present.
    I.e., if [blocking] is retracted this implies
    that [blocked] has been retracted as well.

    Meant to be used when [blocking] invalidates [blocked],
    e.g. p-subsumes it,
    to determine if [blocked] can be savely and permanently dropped.

    this could be extended [blocking] to also use the implication point of a candidate,
    i.e. an Assert or Unit Split candidate,
    instead of only the point where it was added to the context.
    but, this only works for Unit Split candidates,
    as otherwise due to the eager Assert strategy
    the creation (addition) and implication point are identical.
    this doesn't justify the new code.
*)
val is_permanently_blocking: Context.element -> context_partners -> bool

(** creates a function that increments the utility value of the context_unifier_space. *)
val create_space_utility_inc: context_unifier_space -> (unit -> unit)

(** a total ordering on context unifiers
    based on the clause and context literal ids.
    if [different] then a failure exception is raised
    if the two context unifiers are equal. *)
val compare_context_unifiers: different:bool -> raw_context_unifier -> raw_context_unifier -> int

(** returns the constrained part of the clause
    (from cus_constraints),
    if there is one *)
val get_constrained_clause: raw_context_unifier -> clause


(** {2 Representation} *)

(** string representation of the literal pairing of the context unifier. *)
val raw_context_unifier_to_string: raw_context_unifier -> string

(** same as [raw_context_unifier_to_string] *)
val context_unifier_to_string: context_unifier_space -> context_partners -> string
