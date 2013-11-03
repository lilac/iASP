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


(** a problem clause

    Stores a clause with some additional information
    like a unique id, shared variables, and so on,
    as specified in {!Context_unifier.context_unifier_space}.

    Registers all clause literals at {!Problem_literals},
    and initiates the search for new context unifiers
    when informed by {!Problem_literals} that a new literal
    has been added to the context.
*)

(** {6 Types} *)

type config = Config.config
type statistic = Statistic.statistic
type bound = Bound.bound
type literal = Term.literal
type clause = Term.clause
type state = State.state
type context_unifier_space = Context_unifier.context_unifier_space
type raw_context_unifier = Context_unifier.raw_context_unifier
type problem_literals = Problem_literals.problem_literals
type finite_domain = Finite_domain.finite_domain


(** used to create [context_unifier_space] values. *)
type space_registry

(** {6 Functions} *)

(** create a [space_registry] *)
val create: config -> bound -> space_registry

(** {2 Creation} *)

(** [create: space_registry problem_literals clause
    process_close_candidate process_assert_candidate process_split_candidate
    is_assert_incomplete is_split_incomplete  is_lemma]

  - creates a {!Context_unifier_space.context_unifier_space} for a {!Term.clause}
    using [process_close_candidate] resp. [process_assert_candidate]
    resp. [process_split_candidate]
    as callback functions to be called
    when assert resp. split candidates are computed.
    see {!Context_unifier.create_space} for details.

  - and registers the literals with {!Problem_literals}
    if [register] is true (default: true).

    does call [process_split_candidate] if an initial candidate
    can be computed with {e v} only.
*)
val create_space: ?register:bool -> space_registry ->
  problem_literals -> clause ->
  (raw_context_unifier -> unit) -> (raw_context_unifier -> bool) -> (raw_context_unifier -> unit) ->
  (Context.element -> bool) -> (Context.element -> bool) -> bool ->
  finite_domain option ->
  context_unifier_space


(** returns a unique id for a [context_unifier_space].
    Should be used if a [context_unifier_space] is created manually
    with {!Context_unifier.create_space} *)
val get_id: space_registry -> int


(** {2 Backtracking} *)

(** 
    {b DEPENDENCY}: {!Problem_literals.backtrack} must have been executed before,
    dependent on resolve information managed there.
*)
val backtrack: context_unifier_space -> unit
