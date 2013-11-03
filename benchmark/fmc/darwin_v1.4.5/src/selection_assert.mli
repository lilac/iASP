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


(** selection of assert candidates

    stores all assert candidates,
    and selects one for application.
*)



(** {6 Types} *)

type config = Config.config
type bound = Bound.bound
type statistic = Statistic.statistic
type literal = Term.literal
type state = State.state
type raw_context_unifier = Context_unifier.raw_context_unifier
type problem_literals = Problem_literals.problem_literals
type selected = Selection_types.selected
type context = Context.context

type candidates


(** {6 Functions} *)


(** {2 Creation} *)

(** creates an empty candidate repository. *)
val create: config -> bound -> statistic -> state -> context -> problem_literals -> candidates



(** {2 Misc} *)

(** the next candidate to be chosen will lead to the computation
    of a closing context unifier ({!Selection_lookahead}).
    imperfect, might return false although the candidate leads to a Close.
*)
val has_closing_candidate: candidates -> bool

(** the number of candidates to chose from.
    not all of these may be actually eligible,
    some candidates may be subsumed by the context,
    and other candidates may be invalidated by backtracking,
    but not yet removed (done lazily). *)
val size: candidates -> int


(** {2 Candidates} *)

(** adds an assert candidate to the repository.
    returns true if a lookahead closing candidate is known
    ({!Selection_assert.has_closing_candidate}).   
*)
val add: candidates -> raw_context_unifier -> bool

(** selects (priority based) an assert literal from the repository,
    and blocks it from further selection (until backtracking).

    Does not choose candidates subsumed by the context.

    Returns None, if no further assert candidate exists. *)
val select: candidates -> selected option

(** registers all candidates
    which exceed the depth bound and are currently applicable {!Bound}.

    to be called only when [select] returns None,
    based on the semantics of {!Flags.opt_restart},
    when a branch has to be checked for being exhausted.
*)
val check_exceeding: candidates -> unit


(** [is_element_incomplete candidates context_element]
    returns true if applicable exceeding candidates
    computed by this [context_element] have been dropped
    when the restart mode {!Config.restart}
    is {!Flags.opt_restart}[.RS_Delayed].

    Then, when a branch has been found that is saturated
    wrt. to the candidates within the current depth bound,
    all candidates for incomplete context elements are recomputed
    and checked if they are satisfied in the branch,
    and registered in {!Bound} otherwise.
*)
val is_element_incomplete: candidates -> Context.element -> bool


(** {2 Backtracking} *)

(** returns the candidate store to the one associated to the current active state.

    {b DEPENDENCY}: {!State_backtrack} must have been executed before.
*)
val backtrack: candidates -> unit
