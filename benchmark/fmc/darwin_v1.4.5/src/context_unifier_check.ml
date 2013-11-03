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



(*** types ***)

type config = Config.config
type bound = Bound.bound
type var = Var.var
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type context_unifier_space = Context_unifier.context_unifier_space
type context_partners = Context_unifier.context_partners
type raw_context_unifier = Context_unifier.raw_context_unifier



(*** checks ***)



let check_close
  (_config: config)
  (_subst: subst)
  (constraints: Finite_domain_constraints.solution option)
  (context_unifier_space: context_unifier_space)
  (context_partners: context_partners)
  : raw_context_unifier option =

  (* precheck in context_unifier_check assures that
     check_close is only called with an empty remainder *)
  if true (*(Config.is_horn config) || not (Context_unifier.is_remainder' subst)*) then begin
    let closing_context_unifier =
      Context_unifier.create_context_unifier
	context_unifier_space
	(Array.copy context_partners)
	false
	constraints
    in
      Some closing_context_unifier
  end

  else
    None



let check_assert
  (config: config)
  (bound: bound)
  (context: Context.context)
  (subst: subst)
  (constraints: Finite_domain_constraints.solution option)
  (context_unifier_space: context_unifier_space)
  (context_partners: context_partners)
  (is_element_incomplete: bool)
  : raw_context_unifier option =
  
  (* is the assert literal parameter free? *)
  let is_assert_valid_at (i : int) : bool =
    let assert_vars =
      Subst.get_bound_vars
	subst
	context_unifier_space.Context_unifier.cus_input_partners.(i).Context_unifier.ip_vars
    in
      List.for_all 
	(fun var ->
	   Var.is_universal var.Subst.sv_var
	)
	assert_vars
  in
  
  (* has the unifier an empty remainder
     and leaves the assert literal parameter free?
     returns also the index of the assert literal. *)
  (* precheck in context_unifier_check assures that
     check_close is only called with an empty remainder,
     so check only that assert literal instance is universal. *)
  let rec is_valid_at (i: int) (assert_index: int option) : bool * int option =
    if i >= Array.length context_partners then begin
(*      not (Context_unifier.is_remainder' subst), assert_index*)
(*      true, assert_index*)
      failwith "Context_unifier_check.is_valid_at: no assert literal"
    end

    else begin
      let input_index =
	context_unifier_space.Context_unifier.cus_input_partners.(i).Context_unifier.ip_index
      in
      let context_partner =
	context_partners.(input_index)
      in
	(* no parameter in the assert literal? *)
	if context_partner.Context_unifier.cp_element == Context.assert_element then begin
	  (* the is_assert_valid_at check can be really expensive, so we want to avoid it if possible *)
	  if
	    (* parametric propagation on lemmas *)
	    (
	      Config.lemma_parametric_assert config > 0
	      &&
	      context_unifier_space.Context_unifier.cus_lemma_learned >= Config.lemma_parametric_assert config
	    )
	    ||
	    (* if all context literals are universal the instance must be universal *)
	    Context.is_universal context
	    ||
	    (* if all used context literals are universal the instance must be universal *)
	    (Tools.array_for_all (fun cp -> cp.Context_unifier.cp_element.Context.el_pars == []) context_partners)
	    ||
	    (* ok, we have to do the expensive (yes, it is) check *)
	    is_assert_valid_at i
	  then
	    (*is_valid_at (i + 1) (Some i)*)
	    true, Some i
	  else begin
	    false, assert_index
	  end
	end

	(* empty remainder? *)
	else begin
	    (*
	  if
	    Context_unifier.is_remainder 
	      subst
	      context_partner.Context_unifier.cp_element(*.Context.el_pars*)
	      input_index
	  then begin
	    false, assert_index
	  end
	
	  (* yes, so continue *)
	  else*)
	    is_valid_at (i + 1) assert_index
	end
    end
  in
(*    if (not (Config.is_horn config)) && Context_unifier.is_remainder' subst then
      None

    else*)
    match is_valid_at 0 None with
      | false, _ ->
	  None

      | true, None ->
	  failwith "Context_unifier_check.check_assert"

      | true, Some assert_index ->
	  let assert_clause_literal =
	    context_unifier_space.Context_unifier.cus_input_partners.(assert_index).Context_unifier.ip_literal
	  in
	    (* this should be filtered in context_unifier_searach *)
	    if
	      Const.debug
	      &&
	      (Config.neg_assert_candidates config = Flags.NAC_Ignore)
	      &&
	      (not assert_clause_literal.Term.sign)
	    then begin
	      failwith "Context_unifier_check.check_assert: neg_assert";
	    end;

	    (* - within bound -> keep
	       - exceeding bound and not delayed restarting -> register
	       - exceeding bound and delayed restarting -> keep
	       - exceeding bound and lookahead -> keep
	       - otherwise: drop
	    *)
	    let complexity =
	      bound#get_complexity_subst
		assert_clause_literal
		subst
		Subst.input_literal_offset
	    in
	    let keep, exceeding =
	      (* exceeding bound *)
	      if bound#exceeds complexity then begin
		(* exceeding bound, horn, positive, not delayed restarting -> register *)
		if Config.is_horn config && assert_clause_literal.Term.sign then begin
		  match Config.restart config with
		    | Flags.RS_Eager
		    | Flags.RS_Lazy ->
			ignore (bound#register complexity
				(Context_unifier.get_creation_choice_point context_partners)
				: bool);
		    | Flags.RS_Delayed ->
			()
		end;
		
		(* this context element is already incomplete,
		   so all its context unifiers will be recomputed anyway if needed. *)
		if is_element_incomplete then
		  false, true

		(* keep for Delayed *)
		else if Config.restart config = Flags.RS_Delayed then
		  true, true

		(* keep for exceeding lookeahead *)
		else if Config.lookahead_exceeding config then
		  true, true

		(* otherwise drop for the time being *)
		else
		  false, true
	      end

	      (* within bound -> keep *)
	      else
		true, false
	    in
	      if keep then
		(* this candidate is obeying the bound
		   or needed for the lookahead check *)
		let raw_context_unifier = 
		  Context_unifier.create_context_unifier
		    context_unifier_space
		    (Array.copy context_partners)
		    exceeding
		    constraints
		in
		  Some raw_context_unifier
	      else
		None




let check_split
  (config: config)
  (bound: bound)
  (subst: subst)
  (constraints: Finite_domain_constraints.solution option)
  (context_unifier_space: context_unifier_space)
  (context_partners: context_partners)
  (is_element_incomplete: bool)
  : raw_context_unifier option =

  let rec min_complexity_at (i: int) (acc: Bound.complexity option) : Bound.complexity option =
    (* all remainder literals exceed the bound? *)
    if i >= Array.length context_partners then begin
      acc
    end

    else
      let input_index =
	context_unifier_space.Context_unifier.cus_input_partners.(i).Context_unifier.ip_index
      in
      let context_partner =
	context_partners.(input_index)
      in
      let remainder =
	not context_partner.Context_unifier.cp_empty_remainder
	||
	Context_unifier.is_remainder 
	  subst
	  context_partner.Context_unifier.cp_element
	  input_index
      in
	if remainder then begin
	  (* a remainder literal, so check its bound complexity *)
	  let clause_literal =
	    context_unifier_space.Context_unifier.cus_input_partners.(i).Context_unifier.ip_literal
	  in
	  let complexity =
	    bound#get_complexity_subst
	      clause_literal
	      subst
	      Subst.input_literal_offset
	  in
	    if bound#exceeds complexity then begin
	      let new_acc =
		match acc with
		  | Some min_complexity when
		      bound#compare_complexity min_complexity complexity <= 0 ->
		      
		      acc
		    
		  | _ ->
		      Some complexity
	      in
		min_complexity_at (i + 1) new_acc
	    end

	    else begin
	      raise Exit
	    end
	end

	else
	  min_complexity_at (i + 1) acc
  in
    
    (* keep only split candidates within the deepening bound *)
    begin
      try
	match min_complexity_at 0 None with
	  | None ->
	      (* no remainder literal?!?! *)
	      print_endline (Subst.subst_to_string subst);
	      print_endline (Context_unifier.context_unifier_to_string
			       context_unifier_space context_partners);
	      failwith "Context_unifier_check.check_split.min_complexity_at I"

	  | Some complexity ->
	      begin
		match Config.restart config with
		  | Flags.RS_Eager
		  | Flags.RS_Lazy ->
		      (* register the minimum complexity for the potential next restart *)
		      if
			bound#register
			  complexity
			  (Context_unifier.get_creation_choice_point context_partners) 
		      then begin
			None
		      end
			  
		      else
			(* min complexity is within the bound?!? *)
			failwith "Context_unifier_check.check_split.min_complexity_at II"

		  | Flags.RS_Delayed ->
	              (* exceeding bound and delayed restarting *)
		      (* this context element is already incomplete,
			 so all its context unifiers will be recomputed anyway. *)
		      if is_element_incomplete then
			None

		      (* keep all split candidates *)
		      else
			let raw_context_unifier =
			  Context_unifier.create_context_unifier
			    context_unifier_space
			    (Array.copy context_partners)
			    true
			    constraints
			in
			  Some raw_context_unifier
	      end
		
      with
	| Exit ->
	    (* valid complexity found *)
	    let raw_context_unifier =
	      Context_unifier.create_context_unifier
		context_unifier_space
		(Array.copy context_partners)
		false
		constraints
	    in
	      Some raw_context_unifier
    end	    
	    

