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


(** derivation log *)



(** {6 Types} *)

type literal = Term.literal
type config = Config.config
type state = State.state
type choice_point = State.choice_point
type context = Context.context
type raw_context_unifier = Context_unifier.raw_context_unifier
type selected = Selection_types.selected


(** prints the current derivation. *)
class type log =
object
  (** [apply_assert selected]
      registers an {e Assert} application in the current choice point. *)
  method apply_assert: selected -> unit

  (** [apply_split_unit selected]
      registers a Unit {e Split} application in the current choice point. *)
  method apply_split_unit: selected -> unit

  (** [apply_split_left choice_point selected]
      registers a Left {e Split} creating a new [choice_point]. *)
  method apply_split_left: choice_point -> selected -> unit

  (** [apply_split_right selected]
      registers a Right {e Split} application in the current choice point. *)
  method apply_split_right: selected -> unit

  (** [close closing_context_unifier] registers a {e Close} application. *)
  method close: raw_context_unifier -> unit

  (** registers a branch as 'closed' because of being incomplete (see {!Bound}). *)
  method incomplete: unit

  (** registers a branch as 'closed' because of a jump (see {!Jumping}). *)
  method jump: unit

  (** backtracks to the currently active choice point. *)
  method backtrack: unit

  (** finalizes the derivation log.
      That is, depending on the log type,
      nothing might happen or the complete derivation might be printed. *)
  method finalize: unit
end


(** {6 Functions} *)

(**
   provides various ways to print the current derivation.
   Creates a log suite based on the config settings, see:
   - {!Config.print_derivation_online}
   - {!Config.print_derivation_context_unifier}
*)
val create: config -> state -> context -> log
