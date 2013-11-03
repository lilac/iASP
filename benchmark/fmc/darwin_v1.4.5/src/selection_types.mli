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


(** common types for selection modules *)


type literal = Term.literal
type choice_point = State.choice_point
type raw_context_unifier = Context_unifier.raw_context_unifier

(** a selected candidate *)
type selected = {
  candidate_type: State.literal_type;
  (** what type of candidate? *)

  literal: literal;
  (** the literal to add to the context. *)

  raw_context_unifier: raw_context_unifier;
  (** the context unifier from which this candidate was computed. *)
}
