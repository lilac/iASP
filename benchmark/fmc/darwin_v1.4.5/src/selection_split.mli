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


(** selection of split candidates

    stores all split candidates (remainders),
    and selects one for application.
*)



(** {6 Types} *)

type config = Config.config
type choice_point = State.choice_point
type bound = Bound.bound
type statistic = Statistic.statistic
type literal = Term.literal
type state = State.state
type raw_context_unifier = Context_unifier.raw_context_unifier
type problem_literals = Problem_literals.problem_literals
type space_registry = Context_unifier_space.space_registry
type selected = Selection_types.selected
type context = Context.context

type candidates



(** {6 Functions} *)


(** {2 Creation} *)

(** creates an empty candidate repository. *)
val create: config -> bound -> statistic -> state -> context -> problem_literals ->
  space_registry -> candidates


(** {2 Misc} *)

(** the number of candidates to chose from.
    not all of these may be actually eligible,
    some candidates may not be applicable with the current context,
    and other candidates may be invalidated by backtracking
    but not yet removed (done lazily). *)
val size: candidates -> int


(** {2 Candidates} *)

(** adds a split candidate to the repository. *)
val add: candidates -> raw_context_unifier -> unit

(** selects (priority based) a split literal from the repository,
    and blocks it from further selection (until backtracking).

    Does not choose non-applicable candidates,
    i.e candidates subsumed by, contradictory with, or not productive
    with the current context.

    Returns None, if no further split candidate exists. *)
val select: candidates -> Jumping.guiding_step option -> selected option


(** selects a right split literal
    and blocks it from further selection (until backtracking).

    upon backtracking a left split with {!Selection_split.backtrack}
    the corresponding right split is remembered as long as it is implied.

    {b DEPENDENCY}: these have to be applied exhaustively after backtracking before any split or assert
    to ensure completeness/soundness.

    Returns None, if no further right split candidate exists.

    raise CLOSE, if right split is inconsistent with context.
 *)
val select_right_split: candidates -> selected option

(** registers all candidates
    which exceed the depth bound and are currently applicable {!Bound}.

    to be called only when [select] returns None,
    based on the semantics of {!Flags.opt_restart},
    when a branch has to be checked for being exhausted.
*)
val check_exceeding: candidates -> unit


(** see {!Selection_assert.is_element_incomplete}. *)
val is_element_incomplete: candidates -> Context.element -> bool



(** {2 Representation} *)

(** textual representation of (the remainder of) the context unifier as a clause. *)
val remainder_literals_to_string: config -> context -> raw_context_unifier -> string



(** {2 Backtracking} *)


(** [backtrack candidates retracted explanation]
    returns the candidate store to the one associated to the current active state.

    [retracted] is the choice point corresponding to the retracted left split,
    [explanation] are the choice points which implie this conflict.
    that is, [explanation] is the explanation of the corresponding right split,
    which is stored for further usage ({!Selection_split.select_right_split}).

    {b DEPENDENCY}: {!State_backtrack} must have been executed before.
*)
val backtrack: candidates -> choice_point -> literal list (*choice_point list*) -> unit
