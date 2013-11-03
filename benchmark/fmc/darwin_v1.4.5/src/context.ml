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
type statistic = Statistic.statistic
type counter = Counter.counter
type symbol = Symbol.symbol
type var = Var.var
type term = Term.term
type clause = Term.clause
type literal = Term.literal
type subst =  Subst.subst
type choice_point = State.choice_point
type state = State.state
type problem = Problem.problem
type bound = Bound.bound
type finite_domain = Finite_domain.finite_domain
type 'data stack = 'data Stack.stack


type element = {
  el_id: int;
  el_literal: literal;
  el_choice_point: choice_point;
  el_pars: var list;
  el_skolemized: bool;
  el_generation: int;
  el_is_fd_incomplete: bool;
  mutable el_compacted: int;
}


(*** representation ***)

let element_to_string (element: element) : string =
  "choice_point:    " ^ State.choice_point_to_string element.el_choice_point ^ "\n"
  ^ "literal: " ^ Term.literal_to_string element.el_literal ^ "\n"
  ^ "compacted: " ^ (if element.el_compacted = -1 then "No" else "Yes") ^ "\n"


(* context elements are stored in a term index *)
class index_data =
object
  method is_equal (first: element) (second: element) : bool =
    first == second
      
  method to_string (element: element) : string =
    element_to_string element
end
type predicate_index = element Term_indexing.predicate_index

(* a term index for each sign/predicate combination *)
type index = element Term_indexing.index


(* the context is (redundantly) stored in
- a list/stack for easy backtracking
- an index for fast term checks *)
type context = {
  (* environment *)
  statistic: statistic;
  config: config;
  state: state;

  (* the id counter for the context literals/elements.
     only positive, and local to this context,
     in contrast to unique_element_id_counter. *)
  id_counter: counter;

  (* the max. size of the context over the whole derivation.
     just for statistical purposes. *)
  mutable max_size: int;

  (* the context elements stored in a list / as a stack.
     ordered by increasing element id. *)
  mutable context: element stack;

  (* term indexing over the context. an index for each sign/predicate pair *)
  index: index;
  index_skolem: index;

  (* None --> context is universal.
     Some cp -> first cp in which the context become parametric. *)
  mutable universal: choice_point option;

  (* None --> context contains no skolem literals.
     Some cp -> first cp in which the context has a skolem literal.
     
     only updated when Const.ignore_skolem_literals is true
  *)
  mutable skolem_free: choice_point option;
}




(*** globals ***)

(* negative ids reserved for special global elements *)

let unique_element_id_counter =
  ref (-1)


let get_unique_element_id () : int =
  unique_element_id_counter := !unique_element_id_counter - 1;
  !unique_element_id_counter


(*** constants ***)

let null_element : element = {
  el_id = get_unique_element_id ();
  el_literal = Term.null_literal;
  el_pars = [];
  el_skolemized = false;
  el_generation = 0;
  el_choice_point = State.invalid_choice_point;
  el_is_fd_incomplete = false;
  el_compacted = -1;
}

let assert_element = {
  el_id = get_unique_element_id ();
  el_literal = Term.assert_literal;
  el_choice_point = State.valid_choice_point;
  el_pars = [];
  el_skolemized = false;
  el_generation = 0;
  el_is_fd_incomplete = false;
  el_compacted = -1;
}

let plus_v_element = {
  el_id = get_unique_element_id ();
  el_literal = Term.plus_v;
  el_choice_point = State.valid_choice_point;
  el_pars = Term.vars_of_literal Term.plus_v;
  el_skolemized = false;
  el_generation = 0;
  el_is_fd_incomplete = false;
  el_compacted = -1;
}

let minus_v_element = {
  el_id = get_unique_element_id ();
  el_literal = Term.minus_v;
  el_choice_point = State.valid_choice_point;
  el_pars = Term.vars_of_literal Term.minus_v;
  el_skolemized = false;
  el_generation = 0;
  el_is_fd_incomplete = false;
  el_compacted = -1;
}




(*** representation ***)

let print_context (context: context) (out: out_channel) : unit =
  Print.output_line out (Term.literal_to_string (Config.default_v context.config));

  let elements =
    Stack.fold (fun acc x -> x :: acc) [] context.context
  in
  let elements =
    List.sort
      (fun x y ->
	Term.compare_literals x.el_literal y.el_literal
      )
      elements
  in
  List.iter
    (fun element ->
      Print.output_line out (Term.literal_to_string element.el_literal);
    )
    elements




let update_max_size (context: context) : unit =
  context.max_size <- Tools.max_int context.max_size (Stack.size context.context)


let print_max_size (context: context) : unit =
  Print.print_statistic "Maximum Context Size" context.max_size




(*** iteration ***)

let iter (func: element -> unit) (context: context) : unit =
  Stack.iter func context.context

let fold (func: 'a -> element -> 'a) (acc: 'a) (context: context) : 'a =
  Stack.fold func acc context.context

let iter_compacted (func: element -> unit) (context: context) : unit =
  Stack.iter
    (fun element ->
       if element.el_compacted != -1 then
	 func element
    )
    context.context

let fold_compacted (func: 'a -> element -> 'a) (acc: 'a) (context: context) : 'a =
  Stack.fold
    (fun acc element ->
       if element.el_compacted != -1 then
	 func acc element
       else
	 acc
    )
    acc
    context.context





(*** access ***)

let element_equal (first: element) (second: element) : bool =
  first == second






let element_for_literal context literal =
  match (context.index#find literal)#find literal.Term.atom with
   | None ->
       (* compacted *)
       let found = ref None in
	 begin
	 try
	 Stack.iter
	   (fun element ->
	      if Term.literal_equal element.el_literal literal then begin
		found := Some element;
		raise Exit
	      end
	   )
	   context.context;
	   !found
	 with
	   | Exit ->
	       !found
	 end

   | element ->
       element


let most_recent_element (context: context) =
  Stack.top context.context

let is_more_recent x y =
  compare x.el_id y.el_id > 0


let parameters_of_literal (literal: literal) : var list =
  List.find_all
    Var.is_parametric
    (Term.vars_of_literal literal)


(* compact the context with the given (new) literal *)
let compact (context: context) (index: predicate_index) (new_element: element) : unit =
  if not (Config.compact context.config) then begin
    ()
  end

  else begin
    (* find all compacted elements *)
    let compacted_elements =
      if Config.term_indexing context.config then
	index#find_all_instances ~p_preserving:true new_element.el_literal.Term.atom
      else
	Stack.fold
	  (fun acc element ->
	     try
	       ignore (Unification.match_literals ~recompute:false ~p_preserving:true
			 new_element.el_literal 0
			 element.el_literal 1);
	       element :: acc
	     with
	       | Unification.UNIFICATION_FAIL  ->
		   acc
	  )
	  []
	  context.context
    in
      (* mark all uncompacted elements compacted *)
      List.iter
	(fun compacted_element ->
	   (* already compacted by another literal? *)
	   if compacted_element.el_compacted < 0 then begin
	     (* no, so compact *)
	     Statistic.inc_compact context.statistic;
	     compacted_element.el_compacted <- new_element.el_id;
	     let index =
	       context.index#find compacted_element.el_literal
	     in
	       if not(
		 index#remove
		   compacted_element.el_literal.Term.atom
		   (Some compacted_element)
	       )
	       then
		 failwith "Context.compact: remove from index"  

	   end
	)
	compacted_elements
  end


let add (context: context) (literal: literal) (generation: int) (is_fd_incomplete: bool)
    : element =

  (* check if literal already contained *)
  if Const.debug then begin
    iter
      (fun element ->
	 if Term.literal_equal literal element.el_literal then begin
	    print_endline ("in context:\n" ^ element_to_string element);
	    print_endline ("add:        " ^ Term.literal_to_string literal);
	    failwith ("Context.add: already contained");
	 end
      )
      context;
(* this doesn't check compacted elements
    match (context.index#find literal)#find literal.Term.atom with
      | None ->
	  ()
	    
      | Some element ->
	  print_endline ("in context: " ^ Term.literal_to_string element.el_literal);
	  print_endline ("add:        " ^ Term.literal_to_string literal);
	  failwith ("Context.add: already contained");*)
  end;
  (* check if context gets inconsistent *)
  if Const.debug then begin
    iter
      (fun element ->
	 if literal.Term.sign <> element.el_literal.Term.sign then begin
	   try
	     ignore (
	       Unification.unify_terms ~recompute:false ~p_preserving:true
		 literal.Term.atom 0
		 element.el_literal.Term.atom 1
		 : subst);
	     print_endline ("in context:\n" ^ element_to_string element);
	     print_endline ("add:        " ^ Term.literal_to_string literal);
	     failwith ("Context.add: inconsistent: " ^ Term.literal_to_string literal);
	   with
	     | Unification.UNIFICATION_FAIL -> ();
       end
      )
      context;
(* this doesn't check compacted elements
    let index =
      context.index#find (Term.request_negated_literal literal)
    in
      match index#find_unifiable ~p_preserving:true literal.Term.atom with
	| None ->
	    ()

	| Some element ->
	    print_endline ("in context: " ^ Term.literal_to_string element.el_literal);
	    print_endline ("add:        " ^ Term.literal_to_string literal);
	    failwith ("Context.add: inconsistent: " ^ Term.literal_to_string literal);*)
  end;

  let pars =
    parameters_of_literal literal
  in
  begin
    match context.universal, pars with
      | None, _ :: _ ->
	  (* first non-universal literal *)
	  context.universal <- Some (State.active_choice_point context.state)

      | _ ->
	  ()
  end;

  let id =
    try
      Counter.next context.id_counter
    with
      | Counter.OVERFLOW ->
	  raise (Const.NO_SOLUTION "Context.add: context id overflow")
  in
  let new_context_element = {
    el_id = id;
    el_literal = literal;
    el_choice_point = State.active_choice_point context.state;
    el_pars = pars;
    el_skolemized = not (Term.is_skolem_free_literal literal);
    el_generation = generation;
    el_is_fd_incomplete = is_fd_incomplete;
    el_compacted = -1;
  }
  in
    (* put skolem literal also into special index *)
    if Const.ignore_skolem_literals && new_context_element.el_skolemized then begin
      Statistic.inc_debug context.statistic;

      begin
	match context.skolem_free with
	  | None -> context.skolem_free <- Some (new_context_element.el_choice_point)
	  | _ -> ()
      end;

      let index =
	context.index_skolem#find new_context_element.el_literal
      in
	index#add new_context_element.el_literal.Term.atom new_context_element
    end;

    let index =
      context.index#find new_context_element.el_literal
    in
    
    compact context index new_context_element;
    
    (* add new literal to context and index *)
    Stack.push context.context new_context_element;
    update_max_size context;

    if Config.term_indexing context.config then begin
      index#add new_context_element.el_literal.Term.atom new_context_element
    end;
	
    (* all index entries correspond to the term they are stored under *)
    if Const.debug then begin
      context.index#iter
	(fun _ index ->
	   index#iter
	     (fun term element ->
		if not (Term.term_equal term element.el_literal.Term.atom) then begin
		  print_endline ("\n\n\nFAIL=====\n");
		  print_endline (index#to_string);
		  print_endline ("ADD: " ^ Term.literal_to_string new_context_element.el_literal);
		  print_endline ("Term             : " ^ Term.term_to_string term);
		  print_endline ("But node contains: " ^ element_to_string element);
		  failwith "Context.add"
		end
	     )
	)
    end;
    
    new_context_element



let is_universal context =
  context.universal == None




(*** creation ***)

(* the context does not explicitely contain -v/+v,
   this information has to be retrieved from config *)
let create (config: config)
    (statistic: Statistic.statistic) (state: state) : context =
  let context =
  let index =
    if Config.is_horn config then
      Discrimination_tree.create_index false (new index_data)
    else
      Discrimination_tree.create_index true (new index_data)
(*      Substitution_tree.create_index (new index_data)*)
  in
  let index_skolem =
    if Config.is_horn config then
      Discrimination_tree.create_index false (new index_data)
    else
      Discrimination_tree.create_index true (new index_data)
(*      Substitution_tree.create_index (new index_data)*)
  in
    {
      config = config;
      statistic = statistic;
      state = state;
      id_counter = Counter.create_with 0;
      max_size = 0;
      context = Stack.create null_element;
      index = index;
      index_skolem = index_skolem;
      universal = None;
      skolem_free = None;
    }
  in
(*  ignore (
    add context Term.true_literal 0 (Config.finite_domain config)
    : element);*)
  context



let from_file (file_name: string)
  (config: config) (statistic: statistic) (state: state) : context =
  let context =
    create config statistic state
  in

  let in_channel =
    open_in file_name
  in

  let rec read_literals () =
    try
      let line =
	input_line in_channel
      in
      let literal =
	Read_darwin.to_literal line
      in
	ignore (add context literal 0 false: element);
	read_literals ()
    with
      | End_of_file ->
	  ()
  in
    read_literals ();
    close_in in_channel;
    context








(*** checks ***)


let check_contradictory (context: context) (literal: literal) : element option =
  let index =
    context.index#find (Term.request_negated_literal ~insert_db:false literal)
  in
(*  let element =*)
    index#find_unifiable ~p_preserving:true literal.Term.atom
(*  in
    if Const.ignore_skolem_literals then
      match element with
	| Some _ -> element
	| None ->
	    let index =
	      context.index_skolem#find (Term.request_negated_literal ~insert_db:false literal)
	    in
	      index#find_unifiable ~p_preserving:true literal.Term.atom
    else
      element*)

let check_skolem_contradictory (context: context) (literal: literal) : bool =
  if not Const.ignore_skolem_literals then failwith "Context.check_skolem_contradictory";

  (* a cheap but in some cases actually pretty helpful pre-check *)
  context.skolem_free != None
(*  not (context.index_skolem#is_empty)*)
  &&
  let index =
    context.index_skolem#find (Term.request_negated_literal ~insert_db:false literal)
  in
    match
      index#find_unifiable ~p_preserving:true literal.Term.atom
    with
      | Some _ -> true
      | None -> false

let check_subsumed (context: context) (literal: literal) : element option =
  let index =
    context.index#find literal
  in
(*  let element =*)
    index#find_generalization ~p_preserving:true literal.Term.atom
(*  in
    if Const.ignore_skolem_literals then
      match element with
	| Some _ -> element
	| None ->
	    let index =
	      context.index_skolem#find literal
	    in
	      index#find_generalization ~p_preserving:true literal.Term.atom
    else
      element*)


let check_productive (context: context) (producer: literal) (produced: literal) : element option =
  (* if Const.ignore_skolem_literals is true,
     we need to ignore skolem literals in the productivity check.
     Now, as we don't split on skolem literals,
     and the input literal is skolem free,
     this means that neither producer nor produced contain skolem constants.

     this also means that no skolem literal can be between them,
     i.e. strongly shielding produced from producer.

     actually, it means that no skolem literal can be a generalization of produced,
     so if produced is skolem free, no skolem literal can be used
     to make it non-productive.

     thus, we don't have to do anything special in this case.
  *)
  if Const.debug && Const.ignore_skolem_literals &&
    (not (Term.is_skolem_free_literal producer) || not (Term.is_skolem_free_literal producer)) then
    failwith "Context.check_productive: not skolem free";

  let index_producer =
    context.index#find producer
  in
  let index_produced =
    context.index#find produced
  in
      

  (* check second part of productivity *)
  let do_check_productive () =
      (* if the producer is v then any literal more general than produced is shielding *)
      if Term.literal_equal producer (Config.default_v context.config) then begin
	index_produced#find_generalization ~p_preserving:false produced.Term.atom
      end
	
      (* for not mixed literals a simple shielding check is sufficient. *)
      else if not (Config.mixed_literals context.config) then begin
	index_produced#find_shielding producer.Term.atom produced.Term.atom
      end
	
      (* for mixed literals a shielding literal must also not be shielded itself. *)
      else begin
	let iterator =
	  index_produced#get_shielding_iterator producer.Term.atom produced.Term.atom
	in
	let rec loop iterator =
	  if iterator#is_empty then
	    (* not shielded *)
	    None
		  
	  else
	    (* is the shielding literal itself shielded? *)
	    let shielding_element =
	      iterator#next
	    in
	    let shield_for_shielding_element =
	      index_producer#find_shielding
		shielding_element.el_literal.Term.atom
		produced.Term.atom
	    in
	      match shield_for_shielding_element with
		| None ->
		    Some shielding_element
		      
		| Some _ ->
		    loop iterator
	in
	  loop iterator
      end
  in
    
  (* check first part of productivity, i.e. if producer is a most specific generalization.  *)
    if not (Config.mixed_literals context.config) then begin
      match index_producer#find_strongly_shielding producer.Term.atom produced.Term.atom with
	| None ->
	    do_check_productive ()
	      
	| Some _ as conflict ->
	    conflict
    end
    else begin
      do_check_productive ()
    end


let check_productive (context: context) (producer: literal) (produced: literal) : element option =

  if not (Config.productivity context.config) then begin
    None
  end

  (* must be productive if the context is not contradictory. *)
  else if Term.are_terms_variants producer.Term.atom produced.Term.atom then begin
    None
  end

  (* if the producer is universal (and not compacted) it must produce the instance
     if the context is not contradictory. *)
  else if not (Term_attributes.is_term_parametric producer.Term.atom) then begin
    None
  end

  (* do the check *)
  else begin
    let check =
      check_productive context producer produced
    in
      begin
	match check with
	  | None ->
	      ()
		
          | Some _ ->
	      Statistic.inc_filtered_by_productivity context.statistic;
      end;
      check
  end

let find_all_unifiable (context: context) (literal: literal) : element list =
  let index =
    context.index#find literal
  in
    index#find_all_unifiable ~p_preserving:false literal.Term.atom


let find_all_subsuming (context: context) (literal: literal) : element list =
  let index =
    context.index#find literal
  in
    index#find_all_generalizations ~p_preserving:true literal.Term.atom







let rec is_true (context: context) (literal: literal) : bool =
  (* contradictory? *)
  match check_contradictory context literal with
    | Some _ ->
	false

    | None ->
	(* p-instance of a context literal? *)
	begin
	  match check_subsumed context literal with
	    | Some _ -> true

	    | None ->
		(* produced? *)
		let generalizations =
		  (context.index#find literal)#find_all_generalizations
		    ~p_preserving:false literal.Term.atom
		in
		  List.exists
		    (fun generalization ->
		      match check_productive context
			generalization.el_literal
			(Term.request_negated_literal ~insert_db:false literal)
		      with
			| None ->
			    (* a positive produced literal is true in the interpretation,
			       but for a negative one we have to check that the positive one
			       is not produced as well. *)
			    if literal.Term.sign then
			      true
			    else
			      not (is_true context (Term.request_negated_literal ~insert_db:false literal))
			| Some _ -> false
		    )
		    generalizations
	end




(*** Backtracking ***)

(* removes all context literal added in invalid choice points,
   and resets the element counter *)
let backtrack_elements (context: context) : unit =

  (* remove an element from the index *)
  let remove_from_index (element: element) : unit =
    if Config.term_indexing context.config then begin
      if Const.ignore_skolem_literals && element.el_skolemized then begin
	let index =
	  context.index_skolem#find element.el_literal
	in
	let removed : bool =
	  index#remove
	    element.el_literal.Term.atom
	    (Some element)
	in
	  if not removed then begin
	    failwith "Context.backtrack index III"
	  end
      end;

      let index =
	context.index#find element.el_literal
      in
      let removed : bool =
	index#remove
	  element.el_literal.Term.atom
	  (Some element)
      in
	if Const.debug then begin
	  context.index#iter
	    (fun _ index ->
	       index#iter
		 (fun term element ->
		    if not (Term.term_equal term element.el_literal.Term.atom) then begin
		      print_endline ("\n\n\nFAIL=====\n");
		      print_endline (index#to_string);
		      print_endline ("REMOVE: " ^ Term.literal_to_string element.el_literal);
		      print_endline ("Term             : " ^ Term.term_to_string term);
		      print_endline ("But node contains: " ^ element_to_string element);
		      failwith "Context.backtrack"
		    end
		 )
	      )
	end;
	
	if not removed && element.el_compacted == -1 then begin
	  failwith "Context.backtrack index II"
	end
    end
  in
    
  (* all invalidated choice points are the at the top of the context.

     the id counter could be reset here,
     but that has bad interactions with the candidate queue.
     As a total ordering over all candidates uses the id of each used context element,
     and invalid candidates are only lazily removed,
     it can happen that different candidates have the same position in the ordering.
     that is, if an old invalid and a new valid candidate use context literals
     with the same id.

     the same happens with the more complicates scheme of set compression
     for candidates, basically whenever a total ordering over candidates is needed.

     Furthermore, if invalid candidates come earlier in the ordering they
     are also earlier removed.
  *)
  let rec backtrack_elements' () =
    if Stack.is_empty context.context then
      (*	  Counter.set context.id_counter 0;*)
      ()

    else
      let element =
	Stack.top context.context
      in
	if State.is_choice_point_invalid element.el_choice_point then begin
	  remove_from_index element;
	  Stack.remove_top context.context;
	  backtrack_elements' ();
	end
  in
    backtrack_elements' ()


(* uncompact all context literal compacted in invalid choice points *)
let backtrack_compact (context: context) : unit =
  if not (Config.compact context.config) then begin
    ()
  end

  else begin
    try
      let max_context_id =
	(most_recent_element context).el_id
      in
	Stack.iter
	  (fun context_element ->
	     if Const.debug && State.is_choice_point_invalid context_element.el_choice_point then begin
	       failwith (
		 "Context.backtrack_compact: invalid context literal "
		 ^ Term.literal_to_string context_element.el_literal
		 ^ " in the context."
	       );
	     end
	       
	     else if context_element.el_compacted > max_context_id then begin
	       context_element.el_compacted <- -1;
	       let index =
		 context.index#find context_element.el_literal
	       in
		 index#add context_element.el_literal.Term.atom context_element
	     end
	  )
	  context.context
	  
    with
      | Not_found ->
	  (* empty context *)
	  ()
  end

    
let backtrack (context: context) : unit =
  (* first remove all invalid literals *)
  backtrack_elements context;
  (* now undoe the compacting of now removed literals *)
  backtrack_compact context;

  begin
    match context.universal with
      | Some cp when State.is_choice_point_invalid cp ->
	  context.universal <- None
      | _ -> ()
  end;

  begin
    match context.skolem_free with
      | Some cp when State.is_choice_point_invalid cp ->
	  context.skolem_free <- None
      | _ -> ()
  end














(*** DIG ***)




(* in the dig representation a model is represented by positive atoms
   with a set of negative exception atoms.
   For -v, this is quite convenient.
   For +v, this implies that per default the most general version
   of a predicate (called schema term here) is true,
   if not overridden by the context.
   That is, for the predicate p of arity 3 per default p(u, v, w) is true,
   unless a variant of -p(u, v, w) is in the context.
   For a schema term the discrimination into universal and parametric variables
   is of no importance.
*)


(* an implicit generalization. *)
type ig = {
  (* the positive term *)
  term: term;
  (* the negative exceptions *)
  exceptions: term list;
}

(* a disjunction of implicit generalization. *)
type dig = ig list



let sort_digs (dig: dig) : dig =
  List.sort
    (fun x y ->
      Symbol.compare_name (Term.top_symbol_term x.term) (Term.top_symbol_term y.term)
    )
    dig



(* is term subsumed by any term in terms? *)
let is_exception_subsumed (term: term) (terms: term list) : bool =
  List.exists
    (fun old ->
      Unification.is_term_generalization ~p_preserving:false old term
    )
    terms

(* return the terms not subsumed by term. *)  
let subsume_exceptions (term: term) (terms: term list) : term list =
  List.find_all
    (fun old ->
      not (Unification.is_term_generalization ~p_preserving:false term old)
    )
    terms


(* is term subsumed by any ig in dig? *)
let is_subsumed_by_dig (term: term) (dig : dig) : bool =
  List.exists
    (fun ig ->
      (* is the dig term more general? *)
      Unification.is_term_generalization ~p_preserving:false ig.term term
      &&

      (* and none of its exceptions overlapping? *)
      List.for_all
	(fun exception_term ->
	  (* either exception is an instance of the dig term -
	     then the subsuming dig term produces the same instances
	     as the subsumed dig term with this exception *)
	  Unification.is_term_generalization ~p_preserving:false
	    term exception_term
	  ||
	  (* or exception and dig term do not overlap -
	     then the subsuming dig term produces the all instances
	     of the subsumed dig term *)
	  not (Unification.are_terms_unifiable ~p_preserving:false
	    term exception_term)
	)
	ig.exceptions
    )
    dig
  

(* return the igs not subsumed by the ig *)  
let subsume_with_ig (ig: ig) (dig: dig) : dig =
  let subsume =
    [ig]
  in
    List.find_all
      (fun ig' ->
	 not (is_subsumed_by_dig ig'.term subsume)
      )
      dig




(* get the dig representation of all predicates
   which are contained in the context as positive terms.
   for these, the needed schema terms are added as well.

   as compacted literals are not in the index,
   these are ignored automatically.
   literals outside the intended signature or are filtered explicitly
   using the predicate restrict_signature.
*)


(* in finite domain mode replace reltions by equations,
   e.g.
   r_2(z, x, y, 1)
   --> with the 2-ary function symbols f and g:
   f(x, y) = 1
   g(x, y) = 1
*)
let copy_index (context: context) (finite_domain: finite_domain option) : term Term_indexing.index =
  let new_index =
    Discrimination_tree.create_term_index false
  in
    match finite_domain with
      | None ->
	  (* just copy the old index *)
	  context.index#iter
	    (fun literal index ->
	      index#iter
		(fun term _data ->
		  if Term.is_input_term term then
		    (new_index#find literal)#add term term
		)
	    );
	  new_index
	  
    | Some finite_domain ->
	(* copy and replace relations by equations *)
	context.index#iter
	  (fun literal index ->
	    index#iter
	      (fun term _data ->
		(* contains special symbol, skolem, splitting, ... - ignore *)
		if not (Term.is_fd_term term) then begin
		  ()
		end
		  
		(* replace relation by all function instances *)
		else if Term.is_fd_relation literal then begin
		  let equations =
		    Finite_domain.relation_to_equations finite_domain term
		  in
		    List.iter
		      (fun equation ->
			(* if the relation symbol was based on a skolem function,
			   as introduced by term definitions (Const.fd_use_term_definitions),
			   then the function is not over the input signature. *)
			if Term.is_fd_term equation then begin
			  let literal =
			    Term.request_literal literal.Term.sign equation
			  in
			    (new_index#find literal)#add equation equation
			end
		      )
		      equations
		end
		
		(* replace diff by equality *)
		else if Symbol.equal Symbol.diff (Term.top_symbol_term term) then begin
		  match term with
		    | Term.Func func ->
			let equation =
			  Term.request_func (Symbol.equality, func.Term.subterms)
			in
			let literal =
			  Term.request_literal (not literal.Term.sign) equation
			in
			  (new_index#find literal)#add equation equation

		    | _ ->
			failwith "Context.copy_index: diff"
		end

		(* normal predicate, just keep *)
		else if Symbol.is_input (Term.top_symbol_literal literal) then begin
		  (new_index#find literal)#add term term
		end
		  
		(* ? *)
		else begin
		  failwith "Context.copy_index"
		end
	      )
	  );
	new_index


(* assure that all positive schema terms are added for +v *)
let add_schemas (index: term Term_indexing.index) (dig: dig) (problem: problem) : dig =
  begin
    let predicates =
      if problem#containsEquality then
	Symbol.equality :: problem#getPredicateSymbols
      else
	problem#getPredicateSymbols
    in
      List.fold_left
	(fun acc predicate ->
	   (* predicate already processed when creating the digs
	      from the context? *)
	   if
	     List.exists
	       (fun ig ->
		  match ig.term with
		    | Term.Const symbol
		    | Term.Func { Term.symbol = symbol } (*(symbol, _, _)*) ->
			Symbol.equal predicate symbol
			  
		    | _ ->
			false
	       )
	       dig
	   then
	     acc

	   else begin
	     let schema_term =
	       Term.create_schema_term predicate
	     in
	     let exception_index =
	       index#find
		 (Term.request_literal false schema_term)
	     in
	       (* schema already included in negative terms? *)
	       if
		 try
		   exception_index#iter
		     (fun term _ ->
			if Term.is_schema_term term then raise Exit
		     );
		   false
		 with
		   | Exit ->
		       true
	       then
		 acc
		   
	       (* add the schema term *)
	       else
		 (* get all exceptions of the term which are instances *)
		 let instance_exceptions =
		   exception_index#find_all_instances ~p_preserving:false schema_term
		 in
		 
		 (* remove subsumed exceptions. *)
		 let pruned_exceptions =
		   List.fold_left
		     (fun acc current ->
			if is_exception_subsumed current acc then
			  acc
			    
			else
			  current :: subsume_exceptions current acc
		     )
		     []
		     instance_exceptions
		 in
		   
		 (* create the ig *)
		 let ig = {
		   term = schema_term;
		   exceptions = List.sort Term.compare_terms pruned_exceptions;
		 }
		 in
		   ig :: acc
	   end
	)
	dig
	predicates
  end
    



let get_dig (context: context) (problem: problem) (finite_domain: finite_domain option) : dig =
  (* for finite domain copy context and replace relations by functions *)
  let index =
    copy_index context finite_domain
  in

  (* assemble the dig of this predicate by adding the exceptions to the terms. *)
    (* terms: positive term instances *)
    (* exception index: index over negative term instances *)
  let assemble_dig terms exception_index =
    List.fold_left
      (fun acc term ->
	(* in finite domain mode remove all equalities/disequalities
	   not over function symbols.
	   they are not needed anyway,
	   and otherwise e.g. x = 1 might subsume all functions *)
	if begin
	  match finite_domain with
	    | None -> false
	    | Some _ ->
		begin
		  match term with
		    | Term.Func func when Symbol.equal Symbol.equality func.Term.symbol ->
			begin
			  match func.Term.subterms.(0) with
			    | Term.Var _ ->
				true
			    | _ -> false
			    (*| Term.Const symbol ->
				not (Symbol.is_fd_symbol symbol)*)
			end
		    | _ -> false
		end
	  end
	then
	  acc

	(* redundant, ignore this term. *)
	else if is_subsumed_by_dig term acc then begin
	  acc
	end
	  
	else begin
	  (* get all exceptions of the term which are instances *)
	  let instance_exceptions =
	    exception_index#find_all_instances ~p_preserving:false term
	  in

	  (* get all exceptions of the term which can be p-instantiated to instances. *)
	  let exceptions =
	    (* get all p-unifiables... *)
	    let candidate_exceptions =
	      exception_index#find_all_unifiable ~p_preserving:false term
	    in
	      
	      (* but keep only the p-instances *)
	      List.fold_left
		(fun exceptions candidate_exception ->
		  let unifier =
		    Unification.unify_terms ~recompute:false
		      term 0
		      candidate_exception 1
		  in
		  let instance =
		    Subst.apply_to_term unifier ~insert_db:false term 0
		  in
		    if
		      (* instance must be p-preserving *)
		      Subst.is_p_renaming unifier 1
			
		      &&
			
			(* instance can not be a variant of term,
			   this would mean that term is a p-preserving instance of its exception,
			   which is a contradictory context.
			   thus, this checks for proper instantiation.
			*)
			Unification.is_term_instance instance term
		    then
		      instance :: exceptions
		    else
		      exceptions
		)
		instance_exceptions
		candidate_exceptions
	  in
	    
	  (* remove subsumed exceptions. *)
	  let pruned_exceptions =
	    List.fold_left
	      (fun acc current ->
		if is_exception_subsumed current acc then
		  acc
		    
		else
		  current :: subsume_exceptions current acc
	      )
	      []
	      exceptions
	  in
	    
	  (* create the dig *)
	  let ig = {
	      term = term;
	      exceptions = List.sort Term.compare_terms pruned_exceptions;
	  }
	  in
	    (* remove subsumed digs *)
	    ig :: subsume_with_ig ig acc
	  end
      )
      []
      terms
  in
    





  let dig =
  (* get all igs *)
  index#fold
    (fun acc literal index' ->
       (* ignore negative literals *)
       if not literal.Term.sign then begin
	 acc
       end

       (* ignore connection literals *)
       else if Term.is_connection_literal literal then begin
	 acc
       end

       (* get all negative elements for this predicate. *)
       else begin
	 let exception_index =
	   index#find (Term.request_negated_literal ~insert_db:false literal)
	 in
	 (* all positive instances of the term *)
	 let terms =
	   index'#fold
	     (fun acc term _ ->
	       term :: acc
	     )
	     []
	 in
	   
	 (* check if schema term has to be added *)
	 let terms =
	   (* assure that the positive schema literal is added for +v *)
	   if Config.plus_v context.config then begin
	     (* schema already included in positive terms? *)
	     if List.exists Term.is_schema_term terms then
	       terms
		 
	     (* schema already included in negative terms? *)
	     else if
	       try
		 exception_index#iter
		   (fun term _ ->
		     if Term.is_schema_term term then raise Exit
		   );
		 false
	       with
		 | Exit ->
		     true
	     then
	       terms

	     (* nope, so explicitly add the schema term *)
	     else begin
	       Term.create_schema_term_from_term literal.Term.atom :: terms
	     end
	   end

	   (* nothing to do for -v *)
	   else begin
	     terms
	   end
	 in

	 (* add this predicate's dig to all digs *)
	 let dig =
	   assemble_dig terms exception_index
	 in
	   dig @ acc
	 end
	 
    )
    []
  in
    if Config.plus_v context.config then
      add_schemas index dig problem
    else
      dig




let print_DIG (context: context) (out: out_channel) (problem: problem) : unit =
  let dig =
    get_dig context problem None
  in
  let dig =
    sort_digs dig
  in
    (* finally print the digs.
       parameters have no business here,
       so use the universal form of the literals *)
    List.iter
      (fun dig ->
	 match dig.exceptions with
	   | [] ->
	       Print.output_line out
		 (Term.term_to_string
		    (Term.request_pure_term ~universal:true dig.term))
		 
	   | _ ->
	       Print.output_line out
		 (Term.term_to_string
		    (Term.request_pure_term ~universal:true dig.term)
		  ^ " -- exceptions: ");
	       List.iter
		 (fun term ->
		    Print.output_line out
		      ("  " ^ Term.term_to_string
			 (Term.request_pure_term ~universal:true term));
		 )
		 dig.exceptions;
      )
      dig







(*** tptp model ***)

  



(* print the finite domain for the given size *)
let tptp_print_finite_domain (out: out_channel) (bound: bound) =
  let domain_size =
    bound#current_bound
  in

  (* the domain ranges over these elements *)
  let rec create_enumeration i =
    if i > domain_size then
      []
	
    else
      let e_i = Term.to_tptp_term (Finite_domain.get_domain_element i) in
	("X = " ^ Term.term_to_string e_i) :: create_enumeration (i + 1)
  in
  let enumeration =
    create_enumeration 1
  in

  (* the domain elements have to be mutual disjoint *)
  let rec create_distinct i j =
    if i > domain_size then
      []

    else if j > domain_size then
      create_distinct (i + 1) (i + 2)

    else
      let e_i = Term.to_tptp_term (Finite_domain.get_domain_element i) in
      let e_j = Term.to_tptp_term (Finite_domain.get_domain_element j) in
      let axiom = Term.term_to_string e_i ^ " != " ^ Term.term_to_string e_j in
	axiom :: create_distinct i (j + 1)
  in
  let distinct =
    create_distinct 1 2
  in
    Print.output_line out (
        "fof(interpretation_domain, fi_domain,\n"
        (*"fof(interpretation_domain, axiom,\n"*)
      ^ "    ! [X] : ( "
      ^ String.concat " | " enumeration
      ^ " )\n    ).");
    Print.output_line out "";

    if domain_size > 1 then begin
      Print.output_line out (
        "fof(interpretation_domain_distinct, fi_domain,\n"
          (*"fof(interpretation_domain, axiom,\n"*)
	^ "    ( "
	^ String.concat " & " distinct
	^ " )\n    ).");
      Print.output_line out "";
    end




(* get the dig representation *)
let tptp_get_dig (context: context) (problem: problem)
    (finite_domain: finite_domain option) : (symbol * dig) list =
  let dig =
    get_dig context problem finite_domain
  in
  let dig =
    sort_digs dig
  in

  (* group and sort by symbol *)
  let partition =
    Symbol.SymbolTable.create 1024
  in
    List.iter
      (fun dig ->
	let symbol =
	  Term.top_symbol_term dig.term
	in
	let old =
	  try
	    Symbol.SymbolTable.find partition symbol
	  with
	    | Not_found ->
		[]
	in
	  Symbol.SymbolTable.replace partition symbol (dig :: old)
      )
      dig;
    
  let predicates =
    Symbol.SymbolTable.fold
      (fun symbol digs acc -> (symbol, digs) :: acc)
      partition
      []
  in
    List.sort
      (fun (x, _) (y, _) -> Symbol.compare_name x y)
      predicates


(* need to find:
   
   - mapping: generalization -> instance
     p(x, y, z) -> p(a, x', x')
     is
     x -> a, y -> x', z -> x'
   
   - apply reverse mapping to instance,
     using only the first of any bindings to the same variable:
     p(a, y, y)
   
     this ensures the instance is normalized the same way as the generalization
   
   - final mapping: is now x -> a, z -> y

   returns normalized instance and mapping from generalization to it
*)
type mapping = (var * term) list

let map_to_instance (generalization: term) (instance: term) : term * mapping =
  let instantiation =
    Unification.match_terms ~recompute:true
      generalization 0
      instance 1
  in
  let reversed =
    Subst.fold
      (fun acc binding ->
	match binding.Subst.sb_term.Subst.st_term with
	  | Term.Var var ->
	      (Term.request_var var, Term.request_var binding.Subst.sb_var.Subst.sv_var)
	      ::
	      acc

	  | _ ->
	      acc
      )
      []
      instantiation
  in
  let instance' =
    Term.replace_terms_in_term instance reversed
  in
  let instantiation' =
    Unification.match_terms ~recompute:true
      generalization 0
      instance' 1
  in
  let mapping =
    Subst.fold
      (fun acc binding ->
	match binding.Subst.sb_term.Subst.st_term with
	  | Term.Var var when
	      Var.equal var binding.Subst.sb_var.Subst.sv_var ->
	      (* ignore renamings *)
	      acc

	  | _ ->
	      (binding.Subst.sb_var.Subst.sv_var, binding.Subst.sb_term.Subst.st_term)
	      ::
	      acc
	
      )
      []
      instantiation'
  in
    instance', mapping



(* the formula describing the true values of a predicate in a model.

   for example for the
   ig:     p(x, a) \ p(a, a)

   the formula:
   ![x, y]: p(x, y) <=> ((y = a) & -(x = a))

   with:
   pd_schema: p(x, y)
   ig: p(x, a) to p(x, y) yields: [y = a]
       p(x, a) to p(a, a) yields: [x = a]
       so the only pd_igs element is: [ [y = a], [ [x = a] ] ]
 *)
type predicate_definition = {
  (* the schema term p(x, y, z) *)
  pd_schema: term;

  (* the generalizations with exceptions:
     - the instantiation from the schema term to the generalization
     - the instantiations from the generalization to its exceptions
  *)
  pd_igs: (mapping * mapping list) list;

  (* all instances are false *)
  pd_false: bool;
}




(* represent p(x, y) <=> y = 1 as p(x, 1) *)
let simplify_definition (definition: predicate_definition) =
  match definition.pd_igs with
    | (((var, const) :: []), []) :: [] ->
	(* instantiate exactly one variable of the schema term,
	   and no exceptions *)
	let result_var =
	  match definition.pd_schema with
	    | Term.Func func ->
		let result_term =
		  func.Term.subterms.(Array.length func.Term.subterms - 1)
		in begin
		    match result_term with
		      | Term.Var var -> var
		      | _ -> failwith "Context.simplify_definition 1"
		  end
	    | _ -> failwith "Context.simplify_definition 2"
	in
	  (* and the instantiated variable is the result variable *)
	  if Var.equal var result_var then begin
	    let schema_term =
	      Term.replace_vars_in_term
		definition.pd_schema
		(fun var' term ->
		  if Var.equal var var' then
		    const
		  else
		    term
		)
	    in
	      (* recreate the instance and replace the schema with it *)
	      { definition with
		pd_schema = schema_term;
		pd_igs = [];
	      }
	  end

	  else
	    definition
	      
    | _ ->
	definition



(* create the definition for each predicate: *)
let create_predicate_definitions (dig : (symbol * ig list) list) : predicate_definition list =
  List.fold_left
    (fun acc (symbol, dig) ->
      (* create the definition for this dig:
	 schema: p(x, y)
	 ig:     p(x, a) \ p(a, a)
	 --->
	 ![x, y]: p(x, y) <=>
	 ((y = a) & -(x = a))
      *)
      let schema_term =
	(* this was not in the spirit of the tptp representation,
	   which must also specifiy all false terms.
	match dig with
	  | ig :: [] ->
	      (* only one instance, so make instance to schema term -
		 more compact representation *)
	      Term.request_universal_term ig.term
	  | _ ->*)
	      Term.create_schema_term symbol
      in
      let definitions =
	List.fold_left
	  (fun acc ig ->
	    let ig_term, mapping =
	      map_to_instance schema_term ig.term
	    in
	    let exceptions =
	      List.fold_left
		(fun acc exception_term ->
		  let _exception_term', mapping' =
		    map_to_instance ig_term exception_term
		  in
		    mapping' :: acc
		)
		[]
		ig.exceptions
	    in
	      (mapping, exceptions) :: acc
	  )
	  []
	  dig
      in
      let definition =
	{
	  pd_schema = schema_term;
	  pd_igs = definitions;
	  pd_false = false;
	}
      in
	definition :: acc
      )
    []
    dig




(* create the definition for each function:
   
   as this is for finite domain mode,
   we know from the totality axioms that each function symbol occurs positive
   in the context.
*)
let create_function_definitions (problem: problem) (dig : (symbol * ig list) list)
    : predicate_definition list =
  try
    let _, igs =
      List.find
	(fun (symbol, _) ->
	  Symbol.equal Symbol.equality symbol
	)
	dig
    in

    (* create for each function *)
    let function_table =
      Symbol.SymbolTable.create 1024
    in
      (* register each term to the function symbol it is an instance of *)
      List.iter
	(fun ig ->
	  match ig.term with
	    | Term.Func func ->
		begin
		match func.Term.subterms.(0) with
		  | Term.Const symbol
		  | Term.Func { Term.symbol = symbol } ->
		      let old =
			try
			  Symbol.SymbolTable.find function_table symbol
			with
			  | Not_found ->
			      []
		      in
			Symbol.SymbolTable.replace function_table symbol (ig :: old)


		  | Term.Var _ ->
		      (* something like (x = x) *)
		      ()
		end
	    | _ ->
		failwith ("Context.create_function_definitions: " ^ Term.term_to_string ig.term)
	)
	igs;

      (* symbol removed during simplification?
	 then interpret it as '1' at each argument. *)
      let create_function_definition known symbol =
	let simplified =
	  not (List.exists (Symbol.equal symbol) known)
	in
	  if simplified then begin
	    assert (not (Symbol.SymbolTable.mem function_table symbol));
	    let schema_term =
	      let function_term = Term.create_schema_term symbol in
	      let result_term   = Finite_domain.get_domain_element 1 in
		Term.request_func (Symbol.equality, [| function_term; result_term |])
	    in
	    let ig = {
	      term = schema_term;
	      exceptions = [];
	    }
	    in
	    let old =
	      try
		Symbol.SymbolTable.find function_table symbol
	      with
		| Not_found ->
		    []
	    in
	      Symbol.SymbolTable.replace function_table symbol (ig :: old)
	  end
	  else begin
	    assert (Symbol.SymbolTable.mem function_table symbol);
	  end
      in
	List.iter (create_function_definition problem#getConstantSymbols) problem#getAllConstantSymbols;
	List.iter (create_function_definition problem#getFunctionSymbols) problem#getAllFunctionSymbols;

    let function_definitions : predicate_definition list =
      (* create the definitions *)
      Symbol.SymbolTable.fold
	(fun symbol dig acc ->
	  (* create the definition for this dig:
	     schema: f(x) = y
	     ig:     p(x, a) \ p(a, a)
	     --->
	     ![x, y]: p(x, y) <=>
	     ((y = a) & -(x = a))
	  *)
	  let schema_term =
	    (* this was not in the spirit of the tptp representation,
	       which must also specifiy all false terms.
	    match dig with
	      | ig :: [] ->
		  (* only one instance, so make instance to schema term -
		     more compact representation *)
		  Term.request_universal_term ig.term

	      | _ ->*)
		  let function_term =
		    Term.create_schema_term symbol
		  in
		  let result_var =
		    Var.create_universal (Symbol.arity symbol)
		  in
		  let result_term =
		    Term.request_var result_var
		  in
		    Term.request_func (Symbol.equality, [| function_term; result_term |])
	  in
	  let definitions =
	    List.fold_left
	      (fun acc ig ->
		let ig_term, mapping =
		  map_to_instance schema_term ig.term
		in
		let exceptions =
		  List.fold_left
		    (fun acc exception_term ->
		      let _exception_term', mapping' =
			map_to_instance ig_term exception_term
		      in
			mapping' :: acc
		    )
		    []
		    ig.exceptions
		in

		(* empty dig, as the exceptions makes all instances false *)
		let is_empty_dig =
		  (* check didn't pay off, applied almost never *)
		  false
		    (*
		  let vars =
		    Term.vars_of_term ig_term
		  in
		    (* there is a variable *)
		    List.exists
		      (fun var ->
			(* such that for all domain elements *)
			List.for_all
			  (fun domain_element ->
			    (* there is an exception *)
			    List.exists
			      (fun exception_mapping' ->
				match exception_mapping' with
				  | (var', term) :: [] ->
				      (* which makes exactly that domain element false *)
				      Var.equal var var'
				      &&
			              Term.term_equal domain_element term 

				  | _ ->
				      false
			      )
			      exceptions
			  )
			  domain_elements
		      )
		      vars*)
		in

		  (* exception = schema term, then remove definition.
		     can happen for:
		     r_3(u, v, w)
		     -r_3(f, v, w)
		     where r_3(u, v, w) which is instantiated to r_3(f, v, w)
		     by Finite_domain.relation_to_equations in copy_index *)
		  if List.exists (fun x -> x == []) exceptions then
		    acc
		  else if is_empty_dig then
		    acc
		  else
		    (mapping, exceptions) :: acc
	      )
	      []
	      dig
	  in
	  let definition =
	    {
	      pd_schema = schema_term;
	      pd_igs = definitions;
	      pd_false = false;
	    }
	  in
	    (simplify_definition definition) :: acc
	)
	function_table
	[]
    in
      
      (* sort functions by name *)
    let function_symbol_of_definition definition =
      match definition.pd_schema with
	| Term.Func func -> Term.top_symbol_term func.Term.subterms.(0)
	| _ -> failwith "Context.function_symbol_of_definition"
    in

      List.sort
	(fun x y ->
	  Symbol.compare_name
	    (function_symbol_of_definition x)
	    (function_symbol_of_definition y)
	)
	function_definitions;

      
    with
      | Not_found ->
	  (* no equality and no functions in the input, I hope. *)
	  []






let tptp_print_definitions
    (* print these definitions *)
    (definitions: predicate_definition list)
    (* axiom type, e.g. "fi_predicates" *)
    (label: string)
    (out: out_channel)
    : unit =

  Print.output_line out
    ("fof(" ^ label ^ ", (");

  let mapping_to_string mapping : string =
    "( " ^ String.concat " & "
      (List.map
	(fun (var, term) ->
	  let left =
	    Term.tptp_replace_var var
	  and right =
	    term
	  in
	  let term =
	    Term.request_func (Symbol.equality, [| left ; right |])
	  in
	    Term.term_to_string (Term.to_tptp_term term)
	)
	mapping)
      ^ " )"
  in

  (* print the predicates *)
  let definitions' =
    List.fold_left
      (fun acc definition ->
	  (* for each generalization *)
	  let definitions' =
	    List.fold_left
	      (fun acc (mapping, mappings') ->
		match mapping, mappings' with
		  | [], [] ->
		      (* all instances are true without exception *)
		      acc
			
		  | [], _ ->
		      (* generalization is the schema term itself, but there are exceptions *)
		      ("( ~" ^ String.concat " & ~" (List.map mapping_to_string mappings') ^ " )") :: acc
			
		  | _, [] ->
		      (* instance of schema term without exceptions *)
		      (mapping_to_string mapping) :: acc
			  
		  | _ ->
		      (* instance of schema term with exceptions *)
		      ( "( " ^ mapping_to_string mapping
		      ^
		      " & ~" ^ String.concat " & ~" (List.map mapping_to_string mappings')
		      ^ " )")
		      ::
		      acc
	      )
	      []
	      definition.pd_igs
	  in
	  let vars =
	    List.map Term.tptp_replace_var (List.rev (Term.vars_of_term definition.pd_schema))
	  in
	  let var_prefix =
	    match vars with
	      | [] ->
		  "    ( "
	      | _ ->
		  "    ( ! [" ^ String.concat ", " (List.map Term.term_to_string vars) ^ "] : "
	  in
	  let definition' =
	    if definition.pd_false then
	      var_prefix ^ " ( "
		^ Term.term_to_string (Term.to_tptp_term definition.pd_schema)
	      ^ " <=> $false ) )"
	    else
	    match definitions' with
	      | [] ->
		  (* no exceptions *)
		  var_prefix ^ Term.term_to_string (Term.to_tptp_term definition.pd_schema) ^ " )"
		    
	      | _ ->
		  var_prefix  ^ " ( "
		  ^ Term.term_to_string (Term.to_tptp_term definition.pd_schema) ^ " <=> (\n        "
		  ^ String.concat "\n        |\n        " definitions'
		  ^ "\n    ) ) )"
	  in
	    definition' :: acc
      )
      []
      definitions
  in
    Print.output_line out
      (String.concat "\n    &\n" (List.rev definitions') ^ "\n    ) ).")



let print_tptp_model (context: context) (out: out_channel) (problem: problem)
    (finite_domain: finite_domain option) (bound: bound) : unit =

  begin
    match finite_domain with
      | Some _ -> tptp_print_finite_domain out bound;
      | None -> ()
  end;
  (* get the dig *)
  let dig : (symbol * ig list) list =
    tptp_get_dig context problem finite_domain
  in

  (* create the definition for each predicate *)
  let predicate_definitions : predicate_definition list =
    create_predicate_definitions dig
  in

  (* add predicates of which all instances are false *)
  let predicate_definitions =
    List.fold_left
      (fun acc symbol ->
	if not (Symbol.is_input symbol) then
	  acc
	    
	(* already defined? *)
	else if
	  List.exists
	    (fun definition ->
	      Symbol.equal symbol (Term.top_symbol_term definition.pd_schema)
	    )
	    predicate_definitions
	then
	  acc

	(* define all instances as false *)
	else begin
	  let definition = {
	    pd_schema = Term.create_schema_term symbol;
	    pd_igs = [];
	    pd_false = true;
	  }
	  in
	    definition :: acc
	end
      )
      predicate_definitions
      problem#getPredicateSymbols
  in
  let predicate_definitions =
    List.sort
      (fun x y ->
	Symbol.compare_name
	  (Term.top_symbol_term x.pd_schema)
	  (Term.top_symbol_term y.pd_schema)
      )
      predicate_definitions
  in

  (* filter equality in finite domain mode *)
  let predicate_definitions =
    if finite_domain != None then
      List.find_all
	(fun definition ->
	  not (Symbol.equal Symbol.equality (Term.top_symbol_term definition.pd_schema))
	)
	predicate_definitions
    else
      predicate_definitions
  in

  (* print the definitions *)
  begin
    match finite_domain with
      | None ->
	  tptp_print_definitions predicate_definitions
	    "interpretation, i_predicates"
	    (*"interpretation, axiom"*)
	    out;

      | Some _ ->
	  let function_definitions =
	    create_function_definitions problem dig
	  in

	  (* first functions *)
	  if List.length function_definitions > 0 then
	    tptp_print_definitions function_definitions
	      "interpretation_terms, fi_functors" out;
	      (*"interpretation_terms, axiom" out;*)

	  Print.output_line out "";
	  
	  (* then predicates *)
	  if List.length predicate_definitions > 0 then begin
	    tptp_print_definitions predicate_definitions
	      "interpretation_atoms, fi_predicates" out;
	      (*"interpretation_atoms, axiom" out;*)
	  end

	  (* input better contains no predicates ... *)
	  else begin
	      (* there is a tptp problem (SYN916-1) which contains only true.
		 so the check below would crash in that case,
		 although everything is fine. *)
	      (*
	    if problem#containsEquality && List.length problem#getPredicateSymbols = 0 then
	      Print.output_line out "% input contains no predicates except for equality."
	    else
	      failwith "Context.print_tptp_model: print finite domain predicates: input contains no predicates."
	      *)
	  end
  end















(*** finite model ***)

let print_multiplication_tables (context: context) (out: out_channel) (problem: problem) (bound: bound) : unit =
  (* print the sorts and domain elements *)
  let domain_size =
    bound#current_bound
  in

  (* find all posible result combinations for a constant/function *)  
  let rec find_result symbol subterms domain_element : int =
    if domain_element > domain_size then begin
      failwith ("print_multiplication_tables: " ^ Symbol.to_string symbol);
    end
	    
    else begin
      subterms.(Symbol.arity symbol + 1) <- Finite_domain.get_domain_element domain_element;
      let relation_literal =
	Term.request_literal true
	  (Term.request_func (Symbol.get_fd_relation (Symbol.arity symbol + 2), subterms))
      in
	if is_true context relation_literal then
	  domain_element
	else
	  find_result symbol subterms (domain_element + 1)
    end
  in

  (* build all argument vectors for a function/predicate *)
  let rec enumerate_arguments symbol subterms print position simplified =
    (* done *)
    if position > Symbol.arity symbol then begin
      (* check results *)
      let result =
	if simplified then
	  1
	else
	  find_result symbol subterms 1
      in
	print subterms result
      end
      
    else begin
      (* use each domain element and extend to full argument vector *)
      for i = 1 to domain_size do
	subterms.(position) <- Finite_domain.get_domain_element i;
	enumerate_arguments symbol subterms print (position + 1) simplified;
      done;
    end
  in
    
  (* build all argument vectors for a predicate *)
  let rec enumerate_arguments' symbol subterms print position =
    (* done *)
    if position > Symbol.arity symbol then begin
      (* check results *)
      let relation_literal =
	Term.request_literal true
	  (Term.request_func (symbol, subterms))
      in
	if is_true context relation_literal then
	  print relation_literal
	else
	  ()
    end
      
    (* use each domain element and extend to full argument vector *)
    else begin
      for i = 1 to domain_size do
	subterms.(position - 1) <- Finite_domain.get_domain_element i;
	enumerate_arguments' symbol subterms print (position + 1);
      done;
    end
  in

  (* print the multiplication tables *)
  Print.output_line out ("Domain size: " ^ string_of_int domain_size);
  Print.output_line out "";

  (* print the constants *)
  Print.output_line out ("Constants: ");
  List.iter
    (fun symbol ->
      let constant =
	Term.request_const (Symbol.create_fd_symbol symbol)
      in
	(* constant removed during simplification?
	   then interpret it as '1'. *)
      let simplified =
	not (List.exists (Symbol.equal symbol) problem#getConstantSymbols)
      in
      let print _subterms result =
	Print.output_line out
	  (Term.term_to_string constant ^ " = " ^ Term.term_to_string (Finite_domain.get_domain_element result));
      in
	enumerate_arguments symbol [| constant; constant |] print 1 simplified
    )
    (List.sort Symbol.compare_name (List.find_all Symbol.is_input problem#getAllConstantSymbols));
  Print.output_line out "";
    

  (* print the functions *)
  Print.output_line out ("Functions: ");
  List.iter
    (fun (arity, symbols) ->
      List.iter
	(fun symbol ->
	  let symbol_term =
	    Term.request_const (Symbol.create_fd_symbol symbol)
	  in
	    (* symbol removed during simplification?
	       then interpret it as '1' at each argument. *)
	  let simplified =
	    not (List.exists (Symbol.equal symbol) problem#getFunctionSymbols)
	  in
	  let subterms =
	    Array.create (arity + 2) symbol_term
	  in
	  let print subterms result =
	    let subterms' =
	      Array.sub subterms 1 (Symbol.arity symbol);
	    in
	    let function_term =
	      Term.request_func (symbol, subterms')
	    in
	      Print.output_line out
		(Term.term_to_string function_term ^ " = "
		 ^ Term.term_to_string (Finite_domain.get_domain_element result));
	  in
	    enumerate_arguments symbol subterms print 1 simplified;
	    Print.output_line out "";
	)
	(List.sort Symbol.compare_name (List.find_all Symbol.is_input symbols))
    )
    problem#getAllFunctionArities;
  Print.output_line out "";

  (* print the predicates
     just ignore any predicate symbols removed during simplification,
     thus effectively making all instances false *)
  Print.output_line out ("Predicates: ");
  List.iter
    (fun (arity, symbols) ->
      List.iter
	(fun symbol ->
	  Print.output_line out (Symbol.to_string symbol ^ ":");
	  let subterms =
	    Array.create arity (Term.request_var (Var.create_parametric 0))
	  in
	  let print literal =
	    Print.output_line out (Term.term_to_string literal.Term.atom)
	  in
	    enumerate_arguments' symbol subterms print 1;
	    Print.output_line out "";
	)
	(List.sort Symbol.compare_name (List.find_all Symbol.is_input symbols))
    )
    problem#getPredicateArities


let contains context element =
  element == assert_element || element == plus_v_element || element == minus_v_element ||
  let literal = element.el_literal in
  match (context.index#find literal)#find literal.Term.atom with
    | None ->
	false
    | Some element' ->
	element_equal element element'
