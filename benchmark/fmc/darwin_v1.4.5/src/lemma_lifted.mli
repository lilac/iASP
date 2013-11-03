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


(** lifted lemma learning

    computes a lemma, i.e. a clause entailed by the problem clause set,
    from a closing context unifier.
    See {!Lemma} for details.
*)

(** {6 Types} *)

type state = State.state
type context = Context.context
type choice_point = State.choice_point
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst


(** {6 Functions} *)

(** [get_lemma uip state context closing_context_unifier closing_clause closing_clause_constrained context_literals retracted_choice_point]

    computes a lemma from the [closing_context_unifier] between
    the [closing_clause] and the used [context_literals] using lifted regression.
    
    stops the regression at the [retracted_choice_point],
    or earlier, when the uip is reached, if [uip] is given. *)
val get_lemma: uip:bool -> state -> context -> subst -> clause -> clause -> literal array -> choice_point -> clause option
