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

type counter = Counter.counter
type config = Config.config
type bound = Bound.bound
type var =  Var.var
type literal =  Term.literal
type clause =  Term.clause
type subst = Subst.subst
type context = Context.context
type state = State.state
type context_unifier_space = Context_unifier.context_unifier_space
type context_partners = Context_unifier.context_partners
type context_partner = Context_unifier.context_partner
type input_partner = Context_unifier.input_partner
type raw_context_unifier = Context_unifier.raw_context_unifier
type candidate_type = Context_unifier.candidate_type



(* bundle the arguments propagate through all functions. *)
type assemble = {
  (* environment *)
  config: config;
  bound: bound;
  context: context;

  (* the clause to search for context unifiers *)
  context_unifier_space: context_unifier_space;

  (* shortcut to context_unifier_space.Context_unifier.cus_input_partners_ordering *)
  ordering: int array;

  (* the pairings of context literals to clause literals in the partial context unifier.
     the context literal at index i is paired
     with the clause literal with index ip_index = i.
     context unifiers are build from the lower to the higher indices,
     so depending on the current state of merging the partial context unifier
     only the first pairs may make sense, while the others are kind of uninitialized. *)
  context_partners: context_partners;

  (* the new context literal. *)
  active_partner: context_partner;

  (* the index of the clause literal (ip_index) paired with the new context literal.
     this position has only active_partner as its fixed partner,
     while all other clause literals range over all their partners,
     for determining all new context unifiers.
  *)
  fixed_index: int;

  (* has a closing context unifier been found during the current search?
     if so, from now on only other closing context unifiers are of interest,
     (for finding one which leads to the farest backtracking),
     so assert and split context unifiers are ignored.
  *)
  mutable close_found: bool;
}




(* the module structure serves just compile time optimization
   of differences of searching for assert or split context unifiers *)

module type Search = 
  sig
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

module type Candidate_type =
sig
  val t: candidate_type
end

module Close : Candidate_type =
struct
  let t = Context_unifier.Close
end

module Assert : Candidate_type =
struct
  let t = Context_unifier.Assert
end

module Split : Candidate_type =
struct
  let t = Context_unifier.Split
end

module CloseAssert : Candidate_type =
struct
  let t = Context_unifier.CloseAssert
end

module All : Candidate_type =
struct
  let t = Context_unifier.All
end






(* functor to build a search module *)
module Make (Candidate_type: Candidate_type) : Search =
struct

  (* explanation of the common labeled arguments:

   - assert_unifier:
     the partial context unifier is an assert unifier,
     i.e. one literal is not paired with a context literal,
     but with the pseudo context partner Context_unifier.assert_partner.
     therefore, it can not be extended towards a closing or a split
     context unifier.

   - non_empty_remainder:
     is an indication that the current partial context unifier
     does already have a non-empty remainder.
     this is used to stop the search early for close or assert context unifiers.
     it is only an imperfect prefilter, it may always be false,
     but if true, the remainder must be non-empty.

     see update_non_empty_remainder
  *)

let print_debug = false


  (*** process completed context unifiers ***)
  
  
  (* a context unifier is completed,
     now check if it's valid for Close, Assert, Split,
     and send it to the selection module via the given callback function. *)
  let process_context_unifier
      ~(assert_unifier: bool)
      ~(non_empty_remainder: bool)
      (assemble: assemble)
      (subst: subst)
      : unit =

    if print_debug then print_endline ("\n\nCompleted:");
    if print_debug then print_endline (Context_unifier.context_unifier_to_string assemble.context_unifier_space assemble.context_partners);
    if print_debug then print_endline (Subst.subst_to_string subst);
    (* an assert unifier? *)
    if assert_unifier then begin
      (* no closing candidate found yet *)
      if
	not Const.stable_derivation
	&&
	assemble.close_found
      then
	()

      else begin
	let is_element_incomplete =
	  assemble.context_unifier_space.Context_unifier.cus_is_element_incomplete_assert
	    assemble.active_partner.Context_unifier.cp_element
	in
	let solutions =
	  match assemble.context_unifier_space.Context_unifier.cus_constraints with
	     | None -> (subst, None) :: []
	     | Some constraints ->
		 if Finite_domain_constraints.is_p_preserving constraints subst then
		   Finite_domain_constraints.get_solutions
		     (Finite_domain_constraints.setup constraints subst)
		 else
		   []
	in
	  List.iter
	    (fun (subst, constraints) ->
	       if print_debug then print_endline ("Assert Solution: ");
	       if print_debug then print_endline (Subst.subst_to_string subst);
	       let candidate =
		 Context_unifier_check.check_assert
		   assemble.config
		   assemble.bound
		   assemble.context
		   subst
		   constraints
		   assemble.context_unifier_space
		   assemble.context_partners 
		   is_element_incomplete
	       in
		 match candidate with
		   | None ->
		       ()
			 
		   | Some candidate ->
		       (* propagate the candidate *)
		       if assemble.context_unifier_space.Context_unifier.cus_process_assert_candidate
			 candidate then begin
			   (* this assert candidate closes when applied
			      (detected via Selection_assert and lookahead) *)
			   assemble.close_found <- true;
			 end;
	    )
	    solutions
      end
    end

    (* a split or close unifier? *)
    else begin
      (* ugly *)
      let constraint_solution = ref (Some (subst, None)) in
      let search =
	match assemble.context_unifier_space.Context_unifier.cus_constraints with
	  | None -> None
	  | Some constraints ->
	      Some (Finite_domain_constraints.setup constraints subst)		
      in
      (* close? *)
      if
	(not non_empty_remainder)
	&&
	(
	  match Candidate_type.t with
	    | Context_unifier.Close
	    | Context_unifier.CloseAssert
	    | Context_unifier.All ->
		true
		  
	    | Context_unifier.Assert
	    | Context_unifier.Split ->
		false
	)
	&&
	  (* check that solving the finite domain constraints
	     yields a p-preserving solution, if any *)
	  (match assemble.context_unifier_space.Context_unifier.cus_constraints with
	     | None -> true
	     | Some constraints ->
		 Finite_domain_constraints.is_p_preserving constraints subst
	  )
	&&
	  begin
	    (* get a solution for the constraint part,
	       or declare that there is no solution *)
	    begin
	      match search with
		| None ->
		    ()
		| Some search ->
		    begin
		      match Finite_domain_constraints.exists_solution search with
			| None ->
			    constraint_solution := None
			| Some (subst, constraints) ->
			    if print_debug then print_endline ("Close Solution: ");
			    if print_debug then print_endline (Subst.subst_to_string subst);
			    constraint_solution := Some (subst, constraints);
		    end
	    end;
	  
	    match !constraint_solution with
	      | None ->
		  (* constraints unsolvable, but no split candidate *)
		  true

	      | Some (subst, constraints) ->
		  let candidate =
		    Context_unifier_check.check_close assemble.config
		      subst constraints
		      assemble.context_unifier_space assemble.context_partners
		  in
		    match candidate with
		      | None ->
			  (* constraints solvable, but a split candidate *)
			  false
			    
		      | Some candidate ->
			  (* propagate the candidate *)
			  assemble.context_unifier_space.Context_unifier.cus_process_close_candidate candidate;
			  assemble.close_found <- true;
			  true
	  end
      then
	(* yep, closing context unifier found *)
	()
	  
      (* no, so check for split. *)
      else if
	match Candidate_type.t with
	  | Context_unifier.Close
	  | Context_unifier.CloseAssert
	  | Context_unifier.Assert ->
	      false
		
	  | Context_unifier.Split
	  | Context_unifier.All ->
	      true
      then begin
	let is_element_incomplete =
	  assemble.context_unifier_space.Context_unifier.cus_is_element_incomplete_split
	    assemble.active_partner.Context_unifier.cp_element
	in
	let solutions =
	  match search with
	     | None -> (subst, None) :: []
	     | Some search ->
		 Finite_domain_constraints.get_solutions search
	in
	  List.iter
	    (fun (subst, constraints) ->
	       if print_debug then print_endline ("Split Solution: ");
	       if print_debug then print_endline (Subst.subst_to_string subst);
	       let candidate =
		 Context_unifier_check.check_split
		   assemble.config
		   assemble.bound
		   subst
		   constraints
		   assemble.context_unifier_space
		   assemble.context_partners
		   is_element_incomplete
	       in
		 begin
		   match candidate with
		     | None ->
			 (* dropped at depth bound *)
			 ()
			   
		     | Some candidate ->
			 (* propagate the candidate *)
			 assemble.context_unifier_space.Context_unifier.cus_process_split_candidate candidate;
		 end
	    )
	    solutions
      end

      else
	()
    end







  (*** search for context unifiers ***)


  (* to avoid extending partial context unifiers with non-empty remainders
     when only assert and close unifiers are targeted,
     the remainder state of every extending context literal is checked for.
     
     if it corresponds to a remainder literal,
     the context unifier has a non-empty remainder,
     and extending the partial context unifier makes no sense.

     previoiusly this was just based on testing the extension of the partial context unifier,
     not the whole remainder.
     Example: clause {p(x), q(x)} with context literals -p(u), -q(a)
     does yield non_empty_remainder = false, but has a non-empty remainder.

     now the whole remainder is checked.

     both pre-checks pay off.
  *)
  let update_non_empty_remainder (config: config) (non_empty_remainder: bool)
      (subst: subst) (context_partner: context_partner) (_input_partner: input_partner)
      (context_unifier_space: context_unifier_space)
      (context_partners_universal: bool)
      : bool =
    
    (* Horn never produces non-empty remainders *)
    not (Config.is_horn config)
    &&
    (* if all context literals are universal the remainder must be non-empty *)
    not (
      context_partners_universal
      &&
      (* for finite domain constraint vars, skolemized and non-remainder is equivalent *)
      not (Const.fd_constraint_solver && Const.ignore_skolem_literals
           && Config.finite_domain config
           && context_partner.Context_unifier.cp_element.Context.el_skolemized)
    )
    &&
      (
	non_empty_remainder
	||
	not context_partner.Context_unifier.cp_empty_remainder
	||
	(* check if whole subst is remainder-free *)
	Context_unifier.is_remainder' subst
	(* check if the new instantiated partial context unifier is still remainder-free *)
	(* (
	Context_unifier.is_remainder 
	  subst
	  context_partner.Context_unifier.cp_element
	  input_partner.Context_unifier.ip_index
	) *)
	||
	(match context_unifier_space.Context_unifier.cus_constraints with
	   | Some constraints ->
	       not (Finite_domain_constraints.is_p_preserving constraints subst)
	   | None ->
	       false
	)
      )



  (* is the potential context_partner compacted wrt. the new context partner? *)
  let is_compacted (context_partner: context_partner) (current: context_partner) : bool =
    context_partner.Context_unifier.cp_element.Context.el_compacted >= 0
    &&
    context_partner.Context_unifier.cp_element.Context.el_compacted < current.Context_unifier.cp_element.Context.el_id




  (* build all possible unifiers.
     
     the clause may have been virtually reordered (assemble.ordering),
     so the literal currently at position i may have ip_index <> i.
     
     the literals are considered from first to last.
     
     - assert_unifier: see above
     
     - non_empty_remainder: see above
     
     - current_index:
       try to pair the clause literal currently at this position in the ordering,
       i.e. ip_index = ip_input_literal_ordering.(current_index),
       with a context literal at this step.
       invariant: all clause literals at a lower position in assemble.
       ordering have already been paired.

     - subst: the partial context unifier

     - context_partners_universal: all context literals used in the
       partial context unifier are parameter free

     - skolemized: at some of the used context literals contain skolem constants
  *)
  let rec assemble_context_unifier
      ~(assert_unifier: bool)
      ~(non_empty_remainder: bool)
      (assemble: assemble)
      (current_index: int)
      (partial_context_unifier: subst)
      (context_partners_universal: bool)
      (skolemized: bool)
      : unit =

    (* the context unifier is complete. *)
    if current_index >= Array.length assemble.context_unifier_space.Context_unifier.cus_input_partners then begin
      process_context_unifier
	~assert_unifier:assert_unifier
	~non_empty_remainder:non_empty_remainder
	assemble
	partial_context_unifier
    end

    (* ignore the fixed literal -
       it has been paired in the initialization with the new context literal. *)
    else if assemble.ordering.(current_index) = assemble.fixed_index then begin
      assemble_context_unifier
	~assert_unifier:assert_unifier
	~non_empty_remainder:non_empty_remainder
	assemble
	(current_index + 1)
	partial_context_unifier
	context_partners_universal
	skolemized
    end

    else begin
      (* try to extend from here to an assert context unifier. *)
      assemble_assert
	~assert_unifier:assert_unifier
	~non_empty_remainder:non_empty_remainder
	assemble
	current_index
	partial_context_unifier
	context_partners_universal
	skolemized;
    
      (* and also try to extend from here to a split/close context unifier. *)
      assemble_split
	~assert_unifier:assert_unifier
	~non_empty_remainder:non_empty_remainder
	assemble
	current_index
	partial_context_unifier
	context_partners_universal
	skolemized
    end


and assemble_assert
    ~(assert_unifier: bool)
    ~(non_empty_remainder: bool)
    (assemble: assemble)
    (current_index: int)
    (partial_context_unifier: subst)
    (context_partners_universal: bool)
    (skolemized: bool)
    : unit =
  
  if
    (* don't build assert unifiers for the close and split only search *)
    (match Candidate_type.t with
       | Context_unifier.Assert
       | Context_unifier.CloseAssert
       | Context_unifier.All ->
	   true

       | Context_unifier.Close
       | Context_unifier.Split ->
	   false
    )
    &&
    (* this is already an assert unifier *)
    (not assert_unifier)
    &&
    (* no point in trying to build an assert unifier with a non empty remainder *)
    (not non_empty_remainder)
    &&
    (* ignore negative assert literals if wished. *)
    (not (
      (Config.neg_assert_candidates assemble.config = Flags.NAC_Ignore)
      &&
      (not (assemble.context_unifier_space.Context_unifier.cus_input_partners.(assemble.ordering.(current_index)).Context_unifier.ip_literal.Term.sign))
    )
    )
    &&
    (* no closing candidate found yet *)
    ( Const.stable_derivation
      ||
      not assemble.close_found
    )
    &&
      (* ignore asserting equalities in finite domain mode,
	 can only propagate positive equalities,
	 and these can never unify with anything if they are skolemized,
	 and are redundant or lead to a closing context unifier otherwise *)
      not
      (Config.finite_domain assemble.config
       &&
       let symbol = Term.top_symbol_literal assemble.context_unifier_space.Context_unifier.cus_input_partners.(assemble.ordering.(current_index)).Context_unifier.ip_literal in
	 Symbol.equal symbol Symbol.equality || Symbol.equal symbol Symbol.diff
      )
  then begin
    (* mark this literal as the assert candidate *)
    assemble.context_partners.(assemble.ordering.(current_index)) <-  Context_unifier.assert_partner;

    if print_debug then print_endline ("\nASSERT : " ^ Term.literal_to_string assemble.context_unifier_space.Context_unifier.cus_input_partners.(assemble.ordering.(current_index)).Context_unifier.ip_literal);

    assemble_context_unifier
      ~assert_unifier:true
      ~non_empty_remainder:non_empty_remainder
      assemble
      (current_index + 1)
      partial_context_unifier
      context_partners_universal
      skolemized
  end


(* checks if the next literal to choose is resolved.
   raise Not_found otherwise *)
and assemble_resolved_split
    ~(assert_unifier: bool)
    ~(non_empty_remainder: bool)
    (assemble: assemble)
    (current_index: int)
    (partial_context_unifier: subst)
    (context_partners_universal: bool)
    (skolemized: bool)
    : unit =
 
  let input_partner =
    assemble.context_unifier_space.Context_unifier.cus_input_partners.(assemble.ordering.(current_index))
  in

  (* is this input literal resolved wrt. this context literal? *)
  let resolving_partner : context_partner =
    match input_partner.Context_unifier.ip_resolved with
      | Some resolving_partner ->
	  (* we recompute context unifiers after the context partner pairing was done,
	     therefore

	     a) we can only use older resolving context literals than the active one,

	     b) and the same context literal could resolve several clause literals

	       assume that both resolves are done before candidate computation is done,
	       thus not computing the now resolved candidates.
	     
	       Example:
	       Clause: { L1, L2, L3 }
	       Context Literals : K1, K2, K3, R, where R is resolving, applied in this order
	     
	       Pairs:
	         - L1: K1, R
	         - L2: K2
	         - L3: K3, R

	         i) Combinations without Resolve:
	         K1 K2 K3
	         K1 K2 R
	         R  K2 K3
	         R  K2 R

	         ii) Combinations with Resolve first:
	         R  K2 R

	         And that's actually all what is needed from the point of view of the calculus.
	     
	         Now, in contrast to the calculus in the implementation
	         the shorter clause { L1 } is not actually created,
	         this effect is just simulated by fixing the resolving partners.

	         The remainder of L1 R  (L3 R) is at least as general and short
	         as the unifier of L1 K1 (L3 K3), so that is ok.
	         But now the productivity check is also applied to the resolving pairs.
	         And it is bound to fail if the context is not compacted,
	         as R is very likely a generalization of K1 resp. K3.,
	         There, the productivity check is not applied to resolving pairs (Selection_split).
	  *)	      
	  if
	    (* unfortunately, for a stable derivation we need to always compute all candidates,
	       as the original step-wise computation might (in the above example)
	       - compute some candidates with the resolving pair L1 R
	       - resolve with L1 with R
	       - compute some candidates with the resolving pair L3 R	         
	       - resolve with L3 with R
	       or exactly the other way round, as the order is unspecified (Problem_literals).
	       thus, if R can resolve L1 before it resolves L3,
	       the candidate R K2 K3 is computed in this computation order, but K1 K2 R is not,
	       while a different computation order could have the opposite effect.
	       this doesn't matter except for recreating the exact same (stable) derivation,
	       and thus always providing the exact same context unifiers.
	    *)
	    (
	      Const.stable_derivation
	      &&
	      resolving_partner.Context_unifier.cp_element.Context.el_id
	      <
	      assemble.active_partner.Context_unifier.cp_element.Context.el_id
	    )
	    ||

	    (* resolve can be applied in parallel for one context literal on one clause.
	       this might recompute an already existing candidate.*)
	    (
	      not Const.stable_derivation
	      &&
	      resolving_partner.Context_unifier.cp_element.Context.el_id
	      <=
	      assemble.active_partner.Context_unifier.cp_element.Context.el_id
	    )
	  then
	    resolving_partner
	  else
	    raise Not_found

      | _ ->
	  raise Not_found
  in
    if print_debug then print_endline ("\nRESOLVED : " ^ Term.literal_to_string input_partner.Context_unifier.ip_literal);
    if print_debug then print_endline ("BY       : " ^ Term.literal_to_string resolving_partner.Context_unifier.cp_element.Context.el_literal);
    assemble.context_partners.(input_partner.Context_unifier.ip_index) <- resolving_partner;
    
    (* a resolving substitution does not change the rest of the clause,
       and does not produce a remainder literal.
       thus, as this substitution is only used to check
       for remainder literals and their bound complexity,
       and is recomputed and made admissible when the actual candidate is built,
       the resolving substitution can be ignored. *)
    assemble_context_unifier
      ~assert_unifier:assert_unifier
      ~non_empty_remainder:non_empty_remainder
      assemble
      (current_index + 1)
      partial_context_unifier
      context_partners_universal
      skolemized (* resolving literal can't contain skolem constants,
	 as it subsumes a clause literal *)
  
(* try all context literals of the selected queue 
   in order to extend the current context unifier *)
and assemble_split
    ~(assert_unifier: bool)
    ~(non_empty_remainder: bool)
    (assemble: assemble)
    (current_index: int)
    (partial_context_unifier: subst)
    (context_partners_universal: bool)
    (skolemized: bool)
    : unit =

  (* is the current clause literal resolved? *)
  try
    assemble_resolved_split
      ~assert_unifier:assert_unifier
      ~non_empty_remainder:non_empty_remainder
      assemble
      current_index
      partial_context_unifier
      context_partners_universal
      skolemized
  with
    | Not_found ->
	(* no, so find a context partner *)
	let input_partner =
	  assemble.context_unifier_space.Context_unifier.cus_input_partners.(assemble.ordering.(current_index))
	in
	  Stack.iter_stop
	    (fun (context_partner: context_partner) ->
	       if print_debug then print_endline ("\nTRY WITH : " ^ Term.literal_to_string context_partner.Context_unifier.cp_element.Context.el_literal);
	       
	       (* ignore compacted context literals *)
	       if is_compacted context_partner assemble.active_partner then
		 ()

	       (* a non empty remainder is not interesting for assert and close *)
	       else if
		 not context_partner.Context_unifier.cp_empty_remainder
		 &&
		 (
		   (
		     match Candidate_type.t with
		       | Context_unifier.Close
		       | Context_unifier.Assert
		       | Context_unifier.CloseAssert ->
			   true
			     
		       | Context_unifier.Split
		       | Context_unifier.All ->
			   false
		   )
		   ||
		   (* partial assert context unifier to be extended *)
		   assert_unifier
		   ||
		   (* find only close *)
		   assemble.close_found
		 )
	       then
		 ()

	       else begin
		 (* try to extend the unifier *)
		 try
		   let extended_partial_context_unifier =
		     Context_unifier.extend_partial_context_unifier
		       ~recompute:false ~p_preserving:false
		       partial_context_unifier input_partner context_partner
		   in
		   let context_partners_universal =
		     context_partners_universal
		     && 
		     context_partner.Context_unifier.cp_element.Context.el_pars == []
		   in
		   let new_non_empty_remainder =
		     update_non_empty_remainder
		       assemble.config
		       non_empty_remainder
		       extended_partial_context_unifier
		       context_partner
		       input_partner
		       assemble.context_unifier_space
		       context_partners_universal
		   in

		   let new_skolemized =
		     skolemized
		     ||
		     context_partner.Context_unifier.cp_element.Context.el_skolemized
		   in
		     (* a non empty remainder is not interesting for assert and close *)
		     if
		       new_non_empty_remainder
		       &&
		       (
			 (
			   match Candidate_type.t with
			     | Context_unifier.Close
			     | Context_unifier.Assert
			     | Context_unifier.CloseAssert ->
				 true
				   
			     | Context_unifier.Split
			     | Context_unifier.All ->
				 false
			 )
			 ||
			 (* partial assert context unifier to be extended *)
			 assert_unifier
			 ||
			 (* find only close *)
			 assemble.close_found
		       )
		     then
		       ()

		     (* don't compute split candidates from skolem context literals *)
		     else if Const.ignore_skolem_literals
		       && new_skolemized && new_non_empty_remainder then
			 ()

		     (* already violating some constraint *)
		     else if
		       partial_context_unifier != extended_partial_context_unifier
		       &&
		       match assemble.context_unifier_space.Context_unifier.cus_constraints with
			 | Some constraints ->
			     Finite_domain_constraints.is_unsatisfiable_filter constraints extended_partial_context_unifier
			 | None ->
			     false
		     then begin
		       ()
		     end
		       (*
		     (* already violating some constraint *)
		     else if
		       partial_context_unifier != extended_partial_context_unifier
		       &&
		       match assemble.context_unifier_space.Context_unifier.cus_constraints with
			 | Some constraints ->
			     let search =
			       Finite_domain_constraints.setup constraints extended_partial_context_unifier
			     in begin
			       match Finite_domain_constraints.exists_solution search with
				 | Some _ ->
				     false
				 | None ->
				     true
			       end
			 | None ->
			     false
		     then begin
		       Statistic.inc_global_debug ();
		       ()
		     end*)

		     (* finally, continue with the extended partial context unifier. *)
		     else begin
		       if print_debug then print_endline ("\nLITERAL : " ^ Term.literal_to_string input_partner.Context_unifier.ip_literal);
		       if print_debug then print_endline ("BEFORE  : " ^ Subst.subst_to_string partial_context_unifier);
		       if print_debug then print_endline ("EXTENDED: " ^ Subst.subst_to_string extended_partial_context_unifier);
		       assemble.context_partners.(input_partner.Context_unifier.ip_index) <- context_partner;
		       
		       assemble_context_unifier
			 ~assert_unifier:assert_unifier
			 ~non_empty_remainder:new_non_empty_remainder
			 assemble
			 (current_index + 1)
			 extended_partial_context_unifier
			 context_partners_universal
			 new_skolemized
		     end;
		 with
		   | Unification.UNIFICATION_FAIL  ->
		       (* context literal unusable to extend the partial context unifier. *)
		       ()
	       end;
	    )

	    (* stop if the context partner is more recent than the active context literal *)
	    (fun (context_partner: context_partner) ->
	       (* when decoupling the search for specific context unifiers
		  we have to assure that only context literals older
		  than the currently processed context literal are used. *)
	       match Candidate_type.t with
		 | Context_unifier.Close
		 | Context_unifier.CloseAssert
		 | Context_unifier.All ->
		     false
		       
		 | Context_unifier.Assert
		 | Context_unifier.Split ->
		     (* only consider older context literals *)
		     (
		       context_partner.Context_unifier.cp_element.Context.el_id
		       >
		       assemble.active_partner.Context_unifier.cp_element.Context.el_id
		     )
		     ||
		     (* or the same context literal occuring earlier in the clause.
			this is just an arbitrary ordering so that if a context literal
		        pairs with two literals of the same clause the corresponding
			context unifiers is computed only once,
			and not twice, one time from the perspective of each match.
			
			this order is stable, as, this code is only executed
			when context unifiers of existing context partner pairings are recomputed,
			and then no reordering is done.
		     *)
		       (
			 (
			   context_partner.Context_unifier.cp_element.Context.el_id
			   =
			   assemble.active_partner.Context_unifier.cp_element.Context.el_id
			 )
			 &&
			 assemble.ordering.(current_index) > assemble.fixed_index
		       )
	    )
	    input_partner.Context_unifier.ip_context_partners



(* searchs all context unifiers and adds the new candidates to candidates.
   one input_partner may be exempted from the search by setting fixed_index to its index
   and initializing partial_context_unifier and subst accordingly *)

(*   one input literal (fixed_index) is already paired with the new context literal.*)

let search_context_unifiers
  (config: config)
  (bound: bound)
  (context: context)
  (context_unifier_space: context_unifier_space)
  (fixed_index: int)
  (active_partner: context_partner)
  (subst: subst)
  : bool =

  (* if the first unifier is not p-preserving,
     there can't be any close or assert candidate *)
  if (
    match Candidate_type.t with
      | Context_unifier.Close
      | Context_unifier.CloseAssert
      | Context_unifier.Assert ->
	  true

      | Context_unifier.All
      | Context_unifier.Split ->
	  false
  )
    &&
    not active_partner.Context_unifier.cp_empty_remainder
  then begin
    false
  end

  (* constraints known to be unsolvable in the first place *)
  else if
    match context_unifier_space.Context_unifier.cus_constraints with
      | Some constraints -> Finite_domain_constraints.is_unsatisfiable constraints
      | None -> false
  then begin
    false
  end

  else begin
    try
    if print_debug then print_endline ("search_context_unifiers\n");
    if print_debug then print_endline (Term.clause_to_string context_unifier_space.Context_unifier.cus_clause);
    if print_debug then print_endline (Term.literal_to_string active_partner.Context_unifier.cp_element.Context.el_literal);

    let subst, non_empty_remainder =
      match context_unifier_space.Context_unifier.cus_constraints with
	| Some constraints ->
	    begin
	      let subst =
	    try
(*	      begin
		if List.length (Finite_domain_constraints.base_subst constraints) > 0 then begin
		  print_newline ();
		  print_endline ("search_context_unifiers\n");
		  print_endline (Term.clause_to_string (Finite_domain_constraints.get_clause constraints));
		  print_endline (Term.literal_to_string active_partner.Context_unifier.cp_element.Context.el_literal);
		  print_endline (Subst.subst_to_string (Finite_domain_constraints.base_subst constraints));
		end
	      end;*)

	      Unification.unify_substs ~recompute:false
		subst
		(Finite_domain_constraints.base_subst constraints)

	    with
	      | Unification.UNIFICATION_FAIL ->
		  raise Exit
	      in
		let non_empty_remainder =
		  not active_partner.Context_unifier.cp_empty_remainder
		  ||
		  Context_unifier.is_remainder' subst
		  ||
		  not (Finite_domain_constraints.is_p_preserving constraints subst)
		in
		  (* use skolem literals only for close and assert *)
		  if
		    Const.ignore_skolem_literals
		    &&
		    non_empty_remainder
		    &&
		    active_partner.Context_unifier.cp_element.Context.el_skolemized
		  then begin
		    (*print_endline ("NO SOL 3");*)
		    raise Exit;
		  end;

		  (* see if there is a possible solution with this context literal *)
	      let search =
		Finite_domain_constraints.setup constraints subst
	      in
		match Finite_domain_constraints.exists_solution search with
		  | Some _ ->
		      subst, non_empty_remainder
		  | None ->
		      (*print_endline ("NO SOL 1");*)
		      raise Exit
	    end
	| None ->
	    subst, (not active_partner.Context_unifier.cp_empty_remainder)
	    
    in

  (* create the inital partial context unifier scheme with the new context partner *)
  let context_partners =
    Array.make
      (Array.length context_unifier_space.Context_unifier.cus_input_partners)
      Context_unifier.null_partner
  in
    context_partners.(fixed_index) <- active_partner;
    
  let assemble = {
    config = config;
    bound = bound;
    context = context;
    context_unifier_space = context_unifier_space;
    ordering = context_unifier_space.Context_unifier.cus_input_partners_ordering;
    context_partners = context_partners;
    fixed_index = fixed_index;
    active_partner = active_partner;
    close_found = false;
  }
  in
    assemble_context_unifier
      ~assert_unifier:false
      ~non_empty_remainder:non_empty_remainder
      assemble
      0
      subst
      (active_partner.Context_unifier.cp_element.Context.el_pars == [])      
      active_partner.Context_unifier.cp_element.Context.el_skolemized
    ;

    assemble.close_found
    with
      | Exit ->
	  false
  end
end


module SearchClose = Make (Close)
module SearchAssert = Make (Assert)
module SearchSplit = Make (Split)
module SearchCloseAssert = Make (CloseAssert)
module SearchAll = Make (All)

