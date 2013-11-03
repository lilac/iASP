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


(** checking of problem literals against new context literals

    Contains all literals of the problem set (input clause set).
  
    As the same literal may occur in different clauses
    at the same as well as different positions,
    it is stored paired with the index of its position.
    Identical combinations of literals and positions
    are stored only once.

    Checks for each new context literal its unifiability
    with all problem literals, stores the information
    (substitution, resolved, subsumed, ...),
    and calls {!Context_unifier_search.Search.search_context_unifiers}
    for each clause where the new context literal unifies.

    Applies the {e Resolve} and {e Subsume} rule
    of the model evolution calculus.
    As this module contains no information about clauses,
    {e Resolve} is only applied in a restricted - clause independent - way,
    i.e. as subsumption of the problem literal.
*)


(** {6 Types} *)



type config = Config.config
type bound = Bound.bound
type statistic = Statistic.statistic
type literal = Term.literal
type subst = Subst.subst
type choice_point = State.choice_point
type state = State.state
type context = Context.context
type context_partner = Context_unifier.context_partner
type input_partner = Context_unifier.input_partner
type context_unifier_space = Context_unifier.context_unifier_space

type problem_literals


(** {6 Functions} *)


(** {2 Creation} *)

(** creation of the [problem_literals] data structure within a given environment. *)
val create: config -> bound -> statistic -> state ->  context -> problem_literals


(** {2 Registration} *)

(** Clauses have to register their literals
    in order to get notified when a relevant literal is added to the context,
    i.e. a context literal that unifies with one of the clause literals.
*)

(** [register_literal problem_literals literal index]
    retrieves (or implicitly creates)
    the input_partner representing the problem [literal]
    used at the [index] position in a clause.
*)
val register_literal: problem_literals -> literal -> int -> input_partner
  

(** [register_input_partner problem_literals input_partner context_unifier_space]
    registers the [context_unifier_space] as containing the literal [input_partner],
    so that if a context literal unifies with this literal
    [context_unifier_space] is searched for new context unifiers.

    [input_partner] must have been previously retrieved
    with {!Problem_literals.register_literal}.
*)
val register_input_partner: problem_literals -> input_partner -> context_unifier_space -> unit

(** undoes the effects of the calls to {!Problem_literals.register_input_partner} with this space.
    also undoes the effects of {!Problem_literals.register_input_partner},
    if it has been only called with input_partners from this space. *)
val unregister_space: problem_literals -> context_unifier_space -> unit




(** {2 Context Unifier Computation} *)

(** [add problem_literals context_literal candidate_type]
    checks the new [context_literal] against all stored problem literals,
    and calls {!Context_unifier_search.Search.search_context_unifiers}
    for each match.

    Based on [candidate_type] the computation of assert or split candidates
    might be deferred until computed with {!Problem_literals.compute_deferred}.
    Close candidates are always computed.
    [candidate_type] must be [Close], [CloseAssert], or [All].
*)
val add: problem_literals -> Context.element -> Context_unifier.candidate_type -> unit

(** [compute_deferred problem_literals candidate_type]
    computes all context unifiers with the context based on [candidate_type],
    where [candidate_type] must be [Assert] or [Split].

    to be used in conjunction with {!Problem_literals.add},
    most prominently to compute first only [CloseAssert]
    and later the previously ignored [Split] candidates.
*)
val compute_deferred: problem_literals -> Context_unifier.candidate_type -> unit

(** [compute_for_element problem_literals context_element candidate_type]
    computes all context unifiers with the context based on [candidate_type],
    where [candidate_type] must be [Assert] or [Split]
    which are created by [context_element].
    That is, [context_element] must be used in the context unifier,
    and no context literal younger than [context_element] can be used.
*)
val compute_for_element: problem_literals -> Context.element -> Context_unifier.candidate_type -> unit


(** {2 Subsumption} *)

(** is the given clause subsumed by the context? *)
val is_space_subsumed: problem_literals -> context_unifier_space -> bool

(** try to subsume a new clause *)
val check_space_subsumed: problem_literals -> context_unifier_space -> unit


(** {2 Backtracking} *)

(** removes all information based on retracted context literals. 

    {b DEPENDENCY}: {!Context.backtrack} must have been executed before.
*)
val backtrack: problem_literals -> unit
