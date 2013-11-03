(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2005, 2006
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


(** backtracking of module {!State}
*)

(** {6 Types} *)

type config = Config.config
type state = State.state
type context = Context.context
type literal = Term.literal
type clause = Term.clause
type choice_point = State.choice_point
type explanation = State.explanation
type raw_context_unifier = Context_unifier.raw_context_unifier




(** {6 Functions} *)



(** [backtrack state config closing_context_unifier (retract, explanation, lemma)]
    backtracks [state] based on the given [closing context unifier].
    the choice point to backtrack to is computed
    according to the chosen backtracking method ({!Flags.opt_backtracking}),
    all retracted choice points are invalidated and removed.

    returns
    - the retracted choice point in which which
    the closing context unifier was computed ([retract]).
    this might not be the most recent one when lemmas are used,
    or when a new right split not depending on the choice point it was added to,
    is responsible.
    - the [explanation] of the right split to do,
    i.e. the right split corresponding to the left split of [retracted].
    - and potentially a learned [lemma] ({!Lemma}).

    does also increment the value of each clause responsible
    for the closing context unifier ({!Context_unifier.context_unifier_space}).
    that is the closing clause,
    and each clause used to compute a context literal used in the closing context unifier,
    or (recursively) to compute a context literal computing one of these closing context literals,
    stoping at the split decisions.

    @raise Context_unifier.UNSATISFIABLE if no further backtracking is possible,
    i.e. no more choice point to backtrack to exists.
*)
val backtrack: state -> context -> config -> raw_context_unifier -> choice_point * literal list * clause option


(** [backtrack_incomplete state rectract]
    backtracks [state] to the choice point before [retract].

    this is not triggered by a closing context unifier,
    it is merely a jump back in the search space
    in the hope of finding a model more quickly in another search branch.
    this currently happens when
    - an incomplete exhaused branch is found ({!Bound}),
    - or random jumping is used ({!Jumping}).
*)
val backtrack_incomplete: state -> choice_point -> unit
