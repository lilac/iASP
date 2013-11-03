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


(** selection of a split or assert literal
    
    manages the computation of candidates for {e Assert} and {e Split}
    after each change of the context
    with {!Selection_assert}, {!Selection_split}.

    manages the original problem clause set,
    plus derived lemmas.
*)


(** {6 Types} *)

type config = Config.config
type bound = Bound.bound
type statistic = Statistic.statistic
type literal = Term.literal
type clause = Term.clause
type choice_point = State.choice_point
type state = State.state
type selected = Selection_types.selected
type context = Context.context
type guiding_path = Jumping.guiding_path
type sorts = Sort_inference.sorts
type finite_domain = Finite_domain.finite_domain

(** the {e Selection} data structure *)
type selection



(** no further applicable candidate available -
    satisfiability of the problem detected *)
exception SATISFIABLE



(** {6 Functions} *)


(** {2 Creation} *)

(** [create ... input_clauses lemmas initial_interpretation guiding_path] creates a new [selection]
    over the given [input clauses] and [lemmas].

    if a [guiding_path] is given,
    only the search space specified by it is searched.
    that is, first the candidate literals are selected
    so that the [guiding_path] is recreated,
    then the search continues as usual.

    [lemmas] are lemmas kept over a restart.

    [initial_interpretation] is a set of literals
    which is asserted to the initial context.
*)
val create: config -> bound -> statistic -> state -> context -> finite_domain option ->
  clause list -> clause list -> literal list -> guiding_path -> selection

(** add a lemma to the current clause set.

    this does not compute any context unifiers,
    so {!Context_unifier.CLOSE} may not be raised.

    also ok to store local lemmas,
    i.e. lemmas learnt after an incomplete branch has been discovered.
*)
val add_lemma: selection -> clause -> unit


(** {2 Creation} *)

(** to be called when a new element is added to the context.
    Updates the set of assert and split candidate literals.
    May raise {!Context_unifier.CLOSE}. *)
val add: selection -> Context.element -> unit


(** selects a literal to be added to the context.
    Raises {!Selection.SATISFIABLE} if no applicable candidate literal exists. *)
val select: selection -> selected



(** {2 Misc} *)

(** get all stored (non-local) lemmas. *)
val get_lemmas: selection -> clause list

(** is the derivation still replaying the initial guiding path? *)
val in_replay: selection -> bool


(** {2 Backtracking} *)

(** [backtrack selection retracted_split explanation]
    removes candidates based on invalid states.

    takes the retracted left split and its explanation.

    {b DEPENDENCY}: {!State_backtrack} must have been executed before.
*)
val backtrack: selection -> choice_point -> literal list -> unit
