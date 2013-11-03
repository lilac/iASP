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


(** context unifier validity check

    Checks if an assert context unifier meets the {e Assert} rule
    preconditions,
    and checks if a context unifier obeys the deeping bound.
*)


(** {6 Types} *)

type config = Config.config
type bound = Bound.bound
type var = Var.var
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type context_unifier_space = Context_unifier.context_unifier_space
type context_partners = Context_unifier.context_partners
type raw_context_unifier = Context_unifier.raw_context_unifier



(** {6 Functions} *)

(** returns a [raw_context_unifier],
    assumes that subst has an empty remainder. *)
val check_close: config -> subst -> Finite_domain_constraints.solution option -> context_unifier_space ->
  context_partners -> raw_context_unifier option


(** [check_assert config bound context subst fd_constraints
    context_unifier_space context_partners is_element_incomplete]
    returns the [raw_context_unifier] representation of the assert literal,
    if [subst] is a valid 'assert context unifier'.
    I.e. exactly one context_partner in [context_partners]
    is {!Context_unifier.assert_partner}
    and considered to be the gap in the context unifier.
    Then, the assert rule criteria of the model evolution calculus must apply:
    - the remaining context unifier must have an empty remainder,
      this is assumed to be the case
    - and the assert literal must be parameter free,
      or the clause in context_unifier_space must obey {!Config.lemma_parametric_assert}.
      this is checked

    [is_element_incomplete] corresponds to the status of the context unifier wrt.
    {!Selection_assert.is_element_incomplete}.

    Returns also none,
    if the candidate does exceed the bound ({!Bound.bound.exceeds})
    and is not required later on as specified by [config].
    Then, the candidate is registered as dropped ({!Bound.bound.register}).
*)
val check_assert: config -> bound -> Context.context -> subst -> Finite_domain_constraints.solution option ->
  context_unifier_space -> context_partners -> bool -> raw_context_unifier option


(** [check_split config bound subst fd_constraints
    context_unifier_space context_partners is_element_incomplete]
    returns the [raw_context_unifier] representation
    of the context unifier [subst] between
    the input clause [context_unifier_space]
    and the context literals [context_partners].
    Does not check if the remainder is empty,
    this must be done separately with {!Context_unifier_check.check_close}.

    [is_element_incomplete] corresponds to the status of the context unifier wrt.
    {!Selection_assert.is_element_incomplete}.

    Returns also none,
    if the candidate does exceed the bound ({!Bound.bound.exceeds})
    and is not required later on as specified by [config].
    Then, the candidate is registered as dropped ({!Bound.bound.register}).
*)
val check_split: config -> bound -> subst -> Finite_domain_constraints.solution option ->
  context_unifier_space -> context_partners -> bool -> raw_context_unifier option
