(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005
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



(*** types ***)

type var = Var.var
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type raw_context_unifier = Context_unifier.raw_context_unifier






(* get the variables of each input literal producing a remainder literal. *)
let vars_of_remainder_input_literals
  (raw_context_unifier: raw_context_unifier)
  (remainder_states: bool array) :
  (Subst.var list) list =

  Array.fold_left
    (fun acc input_partner ->
       if remainder_states.(input_partner.Context_unifier.ip_index) then
	 input_partner.Context_unifier.ip_vars :: acc
       else
	 acc
    )
    []
    raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_input_partners


(* add the parameters of context literals not producing a remainder literal. *)
let add_pars_of_non_remainder_context_literals
  (raw_context_unifier: raw_context_unifier)
  (remainder_states: bool array)
  (old_pars: Subst.var list)
  : Subst.var list =

  Array.fold_left
    (fun acc input_partner ->
       let index =
	 input_partner.Context_unifier.ip_index
       in
	 if remainder_states.(index) then
	   acc
	 else
	   let context_partner =
	     raw_context_unifier.Context_unifier.rcu_context_partners.(index)
	   in
	   let context_offset =
	     Subst.context_literal_offset index
	   in
	     List.fold_left
	       (fun pars par ->
		  Subst.make_var par context_offset :: pars
	       )
	       acc
	       context_partner.Context_unifier.cp_element.Context.el_pars
    )
    old_pars
    raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_input_partners




(* reverse all var -> par bindings of the remaining remainder literals,
   except if this would also reverse bindings for protected variables. *)
let reverse_subst
  (subst: subst)
  (remainder_input_vars: Subst.var list list)
  (protected_vars: Subst.var list)
  (shared_vars: Subst.var list)
  : subst =

  List.fold_left
    (fun acc vars ->
       let to_reverse =
	 List.find_all
	   (fun var ->
	      not
		(List.exists
		   (fun var2 ->
		      Subst.var_equal var var2)
		   shared_vars
		)
	   )
	   vars
       in
	 Subst.reverse_bindings
	   acc
	   to_reverse
	   protected_vars
    )
    subst
    remainder_input_vars




(* all fresh parameters created up to now for this substitution are needed
   in order to create new unique fresh parameters. *)
let rec make_v_free
  (subst: subst)
  (vars: Subst.var list)
  (fresh_pars: Subst.var list)
  : subst * Subst.var list =

  match vars with
    | [] ->
	(subst, fresh_pars)

    | var :: tail ->
	(* only bind variables, not parameters *)
	if Var.is_universal var.Subst.sv_var then begin

	  match Subst.get subst var with
	    | None ->
		let fresh_par =
		  Var.create_parametric (List.length fresh_pars)
		in
		let fresh_term = 
		  Term.request_var fresh_par
		in

		let new_subst =
		  Subst.set' ~recompute:true ~p_preserving:true
		    subst var.Subst.sv_var var.Subst.sv_offset fresh_term Subst.fresh_par_offset
		in
		  make_v_free
		    new_subst
		    tail
		    ((Subst.make_var fresh_par Subst.fresh_par_offset) :: fresh_pars)

	    | Some _ ->
		(* already bound (to a fresh var?),
		   probably by make_v_free on another remainder literal *)
		(* :TODO: no, that's not the explanation, check...*)
		make_v_free subst tail fresh_pars
	end

	else
	  make_v_free subst tail fresh_pars



(* make all remainder literals containing at least one parameter
   completeley free of universal variables. *)
let rec parameterize_mixed_literals
  (subst: subst)
  (vars: Subst.var list list)
  (fresh_pars: Subst.var list)
  : subst =
  
  match vars with
    | [] ->
	(* all literals have been processed *)
	subst
	
    | head :: tail ->
	(* ignore a literal containing solely universal variables.  *)
	if
	  List.for_all
	    (fun var ->
	       Var.is_universal var.Subst.sv_var
	    )
	    head
	then begin
	  parameterize_mixed_literals subst tail fresh_pars
	end
	  
	(* completely parameterize a literal containing at least one parameter. *)
	else begin
	  let universal_vars =
	    List.find_all
	      (fun var ->
		 Var.is_universal var.Subst.sv_var
	      )
	      head
	  in
	    
	  let (new_subst, new_fresh_pars) =
	    make_v_free subst universal_vars fresh_pars
	  in
	    parameterize_mixed_literals new_subst tail new_fresh_pars
	end



      





(* transforms the context unifier subst to an admissible context unifier. *)
let make_admissible
  (config: Config.config)
  (raw_context_unifier: raw_context_unifier)
  (subst: subst)
  (remainder_states: bool array)
  : subst =

  (* get the variables for each input literal producing a remainder literal. *)
  let (remainder_input_vars: Subst.var list list) =
    vars_of_remainder_input_literals raw_context_unifier remainder_states
  in

  (* get the variables of the remainder literals.
     as the remainder literals are not build yet,
     get the variables in the terms bound to the input literals instead. *)
  let (remainder_literal_vars: Subst.var list list) =
    List.map
      (fun vars ->
	 Subst.get_bound_vars subst vars
      )
      remainder_input_vars
  in

  (* get the universal variables occurring in more than one remainder literal. *)
  let (shared_remainder_literal_vars: Subst.var list) =
    Tools.lists_shared Subst.var_equal remainder_literal_vars
  in
    
  (* map the shared variables to fresh parameters. *)
  let (subst, fresh_pars) =
    make_v_free
      subst
      shared_remainder_literal_vars
      []
  in

  (* fixed_vars are the variables which may not be reversed
     in an attempt to make the remainder literals more universal. *)
  let fixed_pars =
    add_pars_of_non_remainder_context_literals 
      raw_context_unifier
      remainder_states
      (
	fresh_pars
	@
	raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_shared_vars
        @
        shared_remainder_literal_vars
      )
  in

  (* reverse as many par -> var mappings as possible. *)
  let reversed_subst =
    reverse_subst
      subst
      remainder_input_vars
      fixed_pars
      raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_shared_vars
  in
    if Config.mixed_literals config then
      reversed_subst
    else
      (* make all mixed literals universal variable free. *)
      parameterize_mixed_literals
	reversed_subst
	remainder_literal_vars
	fresh_pars

