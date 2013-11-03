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


(** search for context unifiers *)


(** {6 Types} *)


type config = Config.config
type bound = Bound.bound
type subst =  Subst.subst
type context = Context.context
type context_unifier_space = Context_unifier.context_unifier_space
type context_partner = Context_unifier.context_partner
type context_partners = Context_unifier.context_partners



(** {2 Search} *)


(** signature for context unifier search.
    the modules below are specialized instantiations for increased performance.
*)
module type Search = 
  sig
    (** [search_context_unifiers config bound context
	space fixed_index active_partner subst]
	searchs for all context unifiers of the clause [space]
	with the current context and the new context literal [active_partner],
	which is paired to the clause literal with ip_index = [fixed_index].
	
	found context unifiers are propagated
	via the process candidate callback functions of
	{!Context_unifier.context_unifier_space}.

	The search must already have been initialized by unifying
	[context_partner] with the clause literal at position [fixed_index]
	in [space.cus_input_partners],
	yielding the initial partial context unifier [subst].
	[partners] must be set to [context_partner] at position [fixed_index],

	returns true, if a (at least one) closing context unifier has been found.
    *)
    val search_context_unifiers:
      config ->
      bound ->
      context ->
      context_unifier_space ->
      int ->
      context_partner ->
      subst ->
      bool
  end


(** search for {e Close} context unifiers only. *)
module SearchClose: Search

(** search for {e Assert} context unifiers only. *)
module SearchAssert: Search

(** search for {e Split} context unifiers only. *)
module SearchSplit: Search

(** search for {e Assert} and {e Close} context unifiers only. *)
module SearchCloseAssert: Search

(** search for {e Assert}, {e Close}, and {e Split} context unifiers. *)
module SearchAll: Search
