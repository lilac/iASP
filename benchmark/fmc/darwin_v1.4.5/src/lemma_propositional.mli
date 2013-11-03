(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006
              The University of Iowa

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


(** propositional 'lemma' generation

    treats a conflict set,
    i.e. the negation of a set of splits leading to a conflict,
    as a lemma and performs propagations and closures on it.

    See {!Lemma} for details.
*)


(** {6 Types} *)

type config = Config.config
type bound = Bound.bound
type literal = Term.literal
type clause = Term.clause
type state = State.state
type context = Context.context
type choice_point = State.choice_point
type explanation = State.explanation
type propositional_lemmas
type raw_context_unifier = Context_unifier.raw_context_unifier
type candidates = Selection_assert.candidates
type selected = Selection_types.selected
type space_registry = Context_unifier_space.space_registry

type lemmas_propositional



(** {6 Functions} *)

(** [create config state context candidates process_close_candidate process_assert_candidate space_registry]
    creates a lemmas_propositional structure.
    process_close_candidate and process_split_candidate correspond
    to the elements of the same name in {!Context_unifier.context_unifier_space},
    and are used by {!Lemma_propositional.extend_context}
    to propagate candidates.
*)
val create: config -> state -> context -> candidates ->
  (raw_context_unifier -> unit) -> (raw_context_unifier -> bool) -> space_registry ->
  lemmas_propositional

(** [add_lemma lemmas_propositional lemma global]
    adds the [lemma].

    if [global] then the lemma is independent of the current derivation
    and can be reused after a restart. *)
val add_lemma: lemmas_propositional -> clause -> bool -> unit

(** compute assert and close candidates for the new context element
    and propagate them *)
val extend_context: lemmas_propositional -> Context.element -> unit

(** returns all global (see {Lemma_propositional.add_lemma}) clauses. *)
val get_lemmas: lemmas_propositional -> clause list

(** backtracks to the most recent valid choice point *)
val backtrack: lemmas_propositional -> unit
