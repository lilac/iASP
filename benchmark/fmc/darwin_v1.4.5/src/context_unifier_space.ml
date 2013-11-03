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
type statistic = Statistic.statistic
type bound = Bound.bound
type var =  Var.var
type literal =  Term.literal
type clause =  Term.clause
type subst =  Subst.subst
type state = State.state
type context_unifier_space = Context_unifier.context_unifier_space
type context_partners = Context_unifier.context_partners
type context_partner = Context.element
type input_partner = Context_unifier.input_partner
type raw_context_unifier = Context_unifier.raw_context_unifier
type problem_literals = Problem_literals.problem_literals
type candidate_type = Context_unifier.candidate_type
type finite_domain = Finite_domain.finite_domain

type space_registry = {
  config: config;
  bound: bound;
  counter: counter;
}

exception UNSATISFIABLE = Context_unifier.UNSATISFIABLE



let get_id space_registry =
  try
    Counter.next space_registry.counter
  with
    | Counter.OVERFLOW ->
	raise (Const.NO_SOLUTION "Context_unifier_space.create_space: context unifier space id overflow")


let create (config: config) (bound: bound) : space_registry =
  {
    config = config;
    bound = bound;
    counter = Counter.create ()
  }


(*** creation ***)


(* convert the clause to input_partners *)
let create_input_partners (problem_literals: problem_literals) (clause: clause) : input_partner array =
  Array.mapi
    (fun index literal ->
       Problem_literals.register_literal problem_literals literal index
    )
    (Array.of_list clause)


(* create the initial ordering *)
let create_ordering (input_partners: input_partner array) : int array =
  let ordering =
    Array.mapi
      (fun i _ -> i)
      input_partners
  in
    Array.sort
      (fun x y ->
	 compare
	   (Stack.size input_partners.(x).Context_unifier.ip_context_partners)
	   (Stack.size input_partners.(y).Context_unifier.ip_context_partners)
      ) 
      ordering;

    ordering




(* create a context unifier space structure *)
let create_space' ~(register: bool) (space_registry: space_registry)
  (problem_literals: problem_literals)
  (process_close_candidate: raw_context_unifier -> unit)
  (process_assert_candidate: raw_context_unifier -> bool)
  (process_split_candidate: raw_context_unifier -> unit)
  (is_element_incomplete_assert: Context.element -> bool)
  (is_element_incomplete_split: Context.element -> bool)
  (clause: clause)
  (is_lemma: bool)
  (finite_domain: finite_domain option)
  : context_unifier_space =

  (* should we treat constraints specially? *)
  let clause, constraints =
    if not Const.fd_constraint_solver then
      clause, None

    else
    match finite_domain with
      | None -> clause, None
      | Some finite_domain ->
	  let constraints =
	    Finite_domain_constraints.create finite_domain clause
	  in
	    match Finite_domain_constraints.get_constrained constraints with
	      | [] ->
		  (* unconstrained clause *)
		  clause, None
	      | _ ->		  
		  Finite_domain_constraints.get_unconstrained constraints, Some constraints
  in

  let input_partners =
    create_input_partners problem_literals clause
  in
  let ordering =
    create_ordering input_partners
  in

  let input_vars =
    Array.map
      (fun partner -> partner.Context_unifier.ip_vars)
      input_partners
  in

  let vars =
    Array.fold_left
      (fun acc vars ->
	 Tools.lists_merge Subst.var_equal acc vars
      )
      []
      input_vars
  in

  let shared_subst_vars =
    Tools.lists_shared
      Subst.var_equal
      (Array.to_list input_vars)
  in
  let local_subst_vars =
    Array.fold_left
      (fun acc vars ->
	 List.fold_left
	   (fun acc var ->
	      if List.exists (fun var' -> Subst.var_equal var var') shared_subst_vars then
		acc
	      else
		var :: acc;		
	   )
	   acc
	   vars
      )
      []
      input_vars
  in

  let id =
    get_id space_registry
  in

  let context_unifier_space = 
    Context_unifier.create_space
      id clause vars shared_subst_vars local_subst_vars input_partners ordering
      process_close_candidate process_assert_candidate process_split_candidate
      is_element_incomplete_assert is_element_incomplete_split is_lemma
      constraints
  in
    (* register the clause for its literals *)
    if register then begin
      Array.iter
	(fun input_partner ->
	  Problem_literals.register_input_partner
	    problem_literals input_partner context_unifier_space
	)
	input_partners;
    end;

    (* might make sense for lifted lemmas factorized to a tautology *)
    Problem_literals.check_space_subsumed problem_literals context_unifier_space;

    (* if all clause literals are unifiable with -v, we have an initial candidate.
       no split on lemmas or unit clause, so ignore these.
    *)
    if
      is_lemma
      ||
      (* a pure equality clause might have no unconstrained part,
	 so check for clause size = 0 as well. *)
      List.length clause <= 1
    then
      ()

    else begin
      try
	let context_partners =
	  Array.map
	    (fun input_partner ->
	       if Const.debug then begin
		 if Stack.size input_partner.Context_unifier.ip_context_partners > 1 then
		   failwith "Context_unifier_space.create_space"
	       end;

	       let context_partner =
		 Stack.top input_partner.Context_unifier.ip_context_partners
	       in
		 if
		   Context.element_equal Context.plus_v_element context_partner.Context_unifier.cp_element
		   ||
		   Context.element_equal Context.minus_v_element context_partner.Context_unifier.cp_element
		 then
		   context_partner
		 else
		   failwith "Context_unifier_space: -v candidate";
	    )
	    input_partners
	in
	let subst =
	  Subst.empty
(*	  let candidate' = 
	    Context_unifier.create_context_unifier
	      context_unifier_space context_partners false None
	  in
	    Context_unifier.recompute_unifier ~recompute:false candidate'*)
	in
	  (* what happens if we have finite domain constraints?

	     say, we have:
	     p(x), x = y.

	     what do we get?
	     well, with axioms we get -v, -v,
	     but then we get the axioms 1 != 2 and 2 != 2,
	     so -v, -v is not productive anymore,
	     and we also get the instances -v, 1 !=2 and -v 2 != 1.

	     So, if we handle constrains internally we have to list
	     all these solutions here.
	     This also takes care of clauses consisting of constraints only,
	     i.e. pure equality.
	  *)
	let solutions =
	  match constraints with
	     | None -> (subst, None) :: []
	     | Some constraints ->
		 Finite_domain_constraints.get_solutions
		   (Finite_domain_constraints.setup constraints subst)
	in
	  List.iter
	    (fun (subst, constraints) ->
	       match Context_unifier_check.check_split space_registry.config space_registry.bound
		 subst constraints context_unifier_space context_partners false with
		   | None ->
		       (* dropped at depth bound *)
		       ()
			 
		   | Some candidate ->
		       context_unifier_space.Context_unifier.cus_process_split_candidate candidate;
	    )
	    solutions
      with
	| Not_found ->
	    ()
    end;
    
    context_unifier_space







(* creates a context_unifier_space from a clause and returns the initial candidates,
   i.e. a split context unifier computed by using -v *)    
let create_space ?(register: bool = true) (space_registry: space_registry)
  (problem_literals: problem_literals)
  (clause: clause)
  (process_close_candidate: raw_context_unifier -> unit)
  (process_assert_candidate: raw_context_unifier -> bool)
  (process_split_candidate: raw_context_unifier -> unit)
  (is_element_incomplete_assert: Context.element -> bool)
  (is_element_incomplete_split: Context.element -> bool)
  (is_lemma: bool)
  (finite_domain: finite_domain option)
  : context_unifier_space =
  match clause with
    | [] ->
	(* empty clause, i.e. unsatisfiable clause set,
	   should be caught when the clause is added to the system. *)
	failwith "Context_unifier_space.create"

    | _ ->
	create_space' ~register:register space_registry problem_literals
	  process_close_candidate process_assert_candidate process_split_candidate
	  is_element_incomplete_assert is_element_incomplete_split
	  clause is_lemma finite_domain








(*** backtracking ***)


(* assure that the input_partners are kept in increasing order
   of the number of associated context_partners *)
let backtrack (context_unifier_space: context_unifier_space) : unit =

  (* try insertion sort *)
  let input_partners =
    context_unifier_space.Context_unifier.cus_input_partners
  in
  let ordering =
    context_unifier_space.Context_unifier.cus_input_partners_ordering
  in
    (* try to insert all input_partners with insertion sort *)
    for i = 1 to Array.length input_partners - 1 do

      let input_partner_to_move =
	input_partners.(ordering.(i))
      in
	match input_partner_to_move.Context_unifier.ip_resolved with
	  | Some _ ->
	      (* just ignore this resolved literal *)
	      ()
		
	  | None ->
	      let rec insert_at j =
		if j = i then
		  (* biggest entry up to now, just keep in place *)
		  ()
		
		
		(* ignore other resolved literals *)
		else if
		  match input_partners.(ordering.(j)).Context_unifier.ip_resolved with
		    | Some _ -> true
		    | None -> false
		then
		  insert_at (j + 1)


		(* insert after queues with at least same length.
		   this way reordering / swapping of equal length queues is hopefully minimized. *)
		else if
		  (Stack.size input_partner_to_move.Context_unifier.ip_context_partners)
		  >=
		  (Stack.size input_partners.(ordering.(j)).Context_unifier.ip_context_partners)
		then
		  insert_at (j + 1)

		(* bigger than all other up to now, insert here and shift the others *)
		else begin
		  let swap = ordering.(i)
		  in
		    for k = i downto j + 1 do
		      ordering.(k) <- ordering.(k - 1);
		    done;
		    
		    ordering.(j) <- swap;
		end
	      in
		insert_at 0
    done;

    (* check that the order is preserved *)
    if Const.debug then begin
      for i = 1 to Array.length input_partners - 1 do
	if 
	  (
	    (Stack.size input_partners.(ordering.(i)).Context_unifier.ip_context_partners)
	    >=
	    (Stack.size input_partners.(ordering.(i - 1)).Context_unifier.ip_context_partners)
	  )
	  ||
	    (match input_partners.(ordering.(i)).Context_unifier.ip_resolved with
	       | Some _ -> true
	       | None -> false
	    )
	  ||
	    (match input_partners.(ordering.(i - 1)).Context_unifier.ip_resolved with
	       | Some _ -> true
	       | None -> false
	    )
	    
	then
	  ()
	    
	else begin
	  for i = 0 to Array.length input_partners - 1 do
	    print_int
	      (Stack.size input_partners.(ordering.(i)).Context_unifier.ip_context_partners(*_length*));
	    print_newline ();
	  done;
	  failwith "Context_unifier_space.backtrack: order not preserved"
	end
      done
    end 
