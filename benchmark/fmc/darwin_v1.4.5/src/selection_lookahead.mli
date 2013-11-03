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


(** Close lookahead for candidates
    
    Used to store assert and unit split candidates in a lookahead set.
    Other assert (and unit split) candidates can be checked
    for contradiction with the lookahead set.

    The intention is that if there exist two contradictory candidates,
    then this will be detected early and {e Close} can be applied.
    Otherwise - depending on the selection heuristic and bad luck --
    those contradictory candidates might not be chosen at all
    in a reasonable amount of time.
*)


(** {6 Types} *)

type literal = Term.literal
type raw_context_unifier = Context_unifier.raw_context_unifier
type selection_lookahead


(** {6 Functions} *)

(** [create limit database]
    creates a lookahead where at most [limit] candidates are stored in the index. *)
val create: int -> selection_lookahead

(** is the index full, i.e. [limit] candidates are stored? *)
val is_full: selection_lookahead -> bool

(** [add no_duplicates candidate_literal candidate_context_unifier]
    adds a [candidate_literal] with its context unifier to the lookahead set.

    stores each [candidate_literal] only once, if [no_duplicates] is given,
    duplicates are ignored (see {!Term_indexing}).
*)
val add: no_duplicates:bool -> selection_lookahead -> literal -> raw_context_unifier -> unit

(** [check literal]
    returns true, if the literal is contradictory with the lookahead set. *)
val check: selection_lookahead -> literal -> bool

  
(** removes invalid candidates.

    {b DEPENDENCY}: {!State_backtrack} must have been executed before.
*)
val backtrack: selection_lookahead -> unit
