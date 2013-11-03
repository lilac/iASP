(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2005
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


(** random jumps within the search space

    based on an extension of
    "A Random Jump Strategy for Combinatorial Search" by Hantao Zhang.

    based on the remaining time, or just every once in a while,
    a jump forward in the current search space is done,
    in order to increase the likelihood of finding a model,
    and not wasting all time in a deep fruitless branch.

    to keep completeness, the search space positions jumped from
    are recorded as {e guiding path}s.
    if the search space has been exhausted modulo jumped over parts,
    those are revisited by replaying the guiding paths in a non-redundant way.

    the downsides of this approach are
    - recomputation of some parts of the search space,
    though only unit propagation and no splitting is repeated
    - to remain complete, 
    backjumping can not be used for the part of the branch
    that remains after a jump
*)

(** {6 Types} *)


type config = Config.config
type statistic = Statistic.statistic
type literal = Term.literal
type state = State.state
type choice_point = State.choice_point
type bound = Bound.bound

(** a recorded step of a guiding path *)
type guiding_step =
  | Left of literal
      (** replay only the left split -
	  this split was 'above' the destination of a random jump.
	  i.e. it was not jumped over,
	  and it (and its corresponding right split) was left in the search space
	  after the jump. *)

  | Right of literal
      (** replay only the right split -
	  the left split has been explored before the random jump,
	  i.e. this was already a right split when the random jump happened. *)

  | Split of literal
      (** replay a full split decision -
	  this split has been jumped over when doing the random jump,
	  so it and its corresponding right split must still be explored. *)

(** replaying this guiding path in order recreates the search space remaining
    from the corresponding jump.
    each split decision of the original derivation branch is recorded
    in a guiding_step. *)
type guiding_path =
    guiding_step list


(** initiates jumps, and record guiding paths. *)
type jumping



(** {6 Functions} *)

(** {2 Creation} *)

val create: config -> statistic -> state -> bound -> jumping


(** {2 Jumping} *)

(** must be called upon each left split decision.
    decides if a jump should occur,
    and returns the choice point to retract in that case.
    the corresponding right split must be applied to the remaining branch
    in order to avoid recomputing the skipped branch again.    
*)
val jump: jumping -> choice_point option


(** {2 Replay} *)

(** is not guided path recorded?
    i.e. no jumped over search space is left to be explored. *)
val finished: jumping -> bool

(** reinitializes with [state], [bound], and the internal statistics.

    returns the oldest recorded guiding path,
    and removes it from [jumping].
    @raise Not_found if not guiding path is stored. *)
val replay: jumping -> state -> guiding_path



(** {2 Representation} *)

val guiding_path_to_string: guiding_path -> string
