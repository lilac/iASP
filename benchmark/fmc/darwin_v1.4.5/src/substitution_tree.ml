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


(* a basic implementation of a substitution tree.

   ...and despites this the most complicated module...

   PARAMETERS:
   The only modifications is the usage of parameters.
   thus there are two kinds of indicators: universal and parametric.
   non-indicator variables, i.e. variables used to abstract over subterms,
   are always universal.

   INSERTION:
   Only very basic attempts are undertaken to keep the tree shallow:
   - if the term is an instance of a tree node,
     the search descends into this branch.
     The first such node found is used,
     even if some of its siblings are also valid.
     The only slight attempt to use a node which keeps the tree flat
     is to keep sibling nodes ordered by increasing depth of their branches
     and thus to choose the shallowest of the possible branches.

   - if the term is not an instance of any of the child nodes of a node,
     it is inserted under this node.
     If a non-empty mscg with a child exists it is used as a generalization
     (if several exists, the one introducing less bindings is chosen),
     otherwise the term is inserted as a new child.

   NORMALIZATION:
   Prior to insertion a term has to be normalized.
   I.e. all is variables are replaced by indicator variables.
   To allow easy merging into the tree
   a term is normalized using indicator variables
   not already contained in the tree.
   To do this easily the tree contains a counter for indicator ids.

   OFFSETS:
   The tree has one offset (tree_offset) for all contained variables and terms.
   A query term / insertion term gets a different offset (query_offset)
   for the unification operations.
   For an insertion term the query_offset is replace by tree_offset
   when the place to insert the term has been found.

   
*)



(*** types ***)


type var = Var.var
type term = Term.term
type literal = Term.literal
type subst = Subst.subst
type counter = Counter.counter
type binding = Subst.binding
    
(* a leaf - i.e. representing a term with its attached data *)
type 'data leaf = {
  sl_subst: subst;
  (* subst from parent node to this leaf *)

  sl_term: term;
  (* the key *)
  
  sl_data: 'data;
  (* attached data *)
  
  mutable sl_indicators: Var.var list;
  (* the indicator variables which are contained
     in the substitution composed from the root up to and including this node *)
}
    
(* a generalization over leaves *)
and 'data inner_node = {
  sn_subst: subst;
  (* subst from parent node to this node *)
  
  sn_open_vars: Subst.var list;
  (* the variables which are open in the substitution composed from the root
     up to and including this node. I.e. these variables have to be bound
     in a path down to a leaf. *)
  
  sn_closed_vars: Subst.var list;
  (* variables which are bound in the path up to and including the node. *)
  
  mutable sn_indicators: Var.var list;
  (* the indicator variables which are contained
     in the substitution composed from the root up to and including this node *)
  
  mutable sn_children: 'data node list;
  (* child nodes *)
    
  mutable sn_depth: int;
  (* maximum of the depth of the subtrees under the children *)
}
  
(* a node *)    
and 'data node =
  | Node of 'data inner_node
  | Leaf of 'data leaf


exception ITERATOR_EMPTY = Term_indexing.ITERATOR_EMPTY

(* an element matching the request has been found in the index.
   unfortunately, just returning it with (FOUND data)
   is not possible due to the polymorphic exception.
*)
exception FOUND

type 'data data = 'data Term_indexing.data
type 'data iterator = 'data Term_indexing.iterator



(*** offset consts ***)
    
let tree_offset = 0
		    
let query_offset = 1



(*** the substitution tree ***)
class ['data] predicate_index (__data: 'data data) =

object (self)

  (**** values ***)

  (* object providing functions on the data type *)
  val _data = __data

    
  (* index size *)
  val mutable _size = 0;

  (* counter for the creation of fresh indicator variables *)
  val _counter = Counter.create ()

  (* the root of the substitution tree *)
  val mutable _root = None;



  (*** exeption FOUND ***)
  (* ugly: this combo simulates raise (FOUND 'data) *)

  val mutable _found : 'data option = None

  method private set_found data =
    _found <- Some data
      
  method private get_found =
    match _found with
      | None ->
	  failwith "Substitution_tree.get_found"
	    
      | Some _ ->
	  let data =
	    _found
	  in
	    _found <- None;
	    data






      
  (*** access ***)
      

  method clear : unit =
    _root <- None;
    Counter.set _counter 0

  method size : int =
    _size






  (*** node accesss ***)
      
  method private get_node_subst (node: 'data node) : subst =
    match node with
      | Leaf leaf ->
	  leaf.sl_subst
	    
      | Node inner_node ->
	  inner_node.sn_subst
	    
  method private get_node_indicators (node: 'data node) : Var.var list =
    match node with
      | Leaf leaf ->
	  leaf.sl_indicators
	    
      | Node inner_node ->
	  inner_node.sn_indicators


  (*** tree root access ***)
	    
  method private get_root_var : Subst.var =
    match _root with
      | None ->
	  failwith "Substitution_tree.get_root_var: empty tree"
	    
      | Some node ->
	  begin
	    let subst =
	      self#get_node_subst node
	    in
	      if Const.debug then begin
		if Subst.length subst != 1 then
		  failwith "Substitution_tree.get_root_var:"
	      end;

	      (Subst.first subst).Subst.sb_var
	  end
	    
  method private get_root_term : term =
    match _root with
      | None ->
	  failwith "Substitution_tree.get_root_term: empty tree"
	    
      | Some node ->
	  begin
	    let subst =
	      self#get_node_subst node
	    in
	      if Const.debug then begin
		if Subst.length subst != 1 then
		  failwith "Substitution_tree.get_root_term:"
	      end;

	      (Subst.first subst).Subst.sb_term.Subst.st_term
	  end




  (*** term normalization ***)
	    
  (* replace indicator vars by non-indicator vars *)
  method private clone_term_without_indicator_vars (term: term) =
    
    let rec do_clone_term_without_indicator_vars (term: term) : term =
      
      match term with
	| Term.Var var ->
	    Term.request_var (Var.clone_as_non_indicator var)
	      
	| Term.Const _ -> 
	    term
	      
	| Term.Func (symbol, terms, _) ->
	    Term.request_func (symbol, Array.map do_clone_term_without_indicator_vars terms)
    in
      do_clone_term_without_indicator_vars term
	
	
  (* replace variables with fresh indicator variables not contained in the tree
     
     a term to insert
     - may not contain any variables already used in the tree
     - and must contain only indicator variables
  *)
  method private clone_term_renumbered (term: term) =

    let rec do_clone_term_renumbered (var_mapping: (var * var) list) (term: term) :
	term * ((var * var) list) =
      
      match term with
	| Term.Var var ->
	    let new_var, new_var_mapping =
		  let rec assoc list =
		    match list with
		      | [] ->
			  let new_var =
			    (* the next id can be deduced from the number of remapped vars *)
			    Var.clone_renumbered (Var.clone_as_indicator var) (Counter.next _counter)
			  in
			    new_var, (var, new_var) :: var_mapping
			  
		      | (var', bound_var) :: tail ->
			  if Var.equal var var' then
			    bound_var, var_mapping
			  else
			    assoc tail
		  in
		    assoc var_mapping
(*
	      try 
		List.assoc var var_mapping, var_mapping
	      with
		| Not_found ->
		    let new_var =
		      (* the next id can be deduced from the number of remapped vars *)
		      Var.clone_renumbered (Var.clone_as_indicator var) (Counter.next _counter)
		    in
		      new_var, (var, new_var) :: var_mapping
*)
	    in
	      Term.request_var new_var, new_var_mapping
		
	| Term.Const _ ->
	    term, var_mapping
	      
	| Term.Func (symbol, terms, _) ->
	    let new_terms =
	      Array.copy terms
	    in
	      
	    let rec do_at (i : int) mapping =
	      if i >= Array.length new_terms then
		mapping
	      else
		let new_term, altered_var_mapping =
		  do_clone_term_renumbered mapping terms.(i)
		in
		  new_terms.(i) <- new_term;
		  do_at (i + 1) altered_var_mapping
	    in
	      
	    (* side effect on new_terms *)
	    let new_var_mapping =
	      do_at 0 var_mapping
	    in
	      Term.request_func (symbol, new_terms), new_var_mapping
    in
    let renumbered_term, _ =
      do_clone_term_renumbered [] term
    in
      renumbered_term







  (*** representation  ***)



  method private leaf_to_string
    (root_term: term)
    (subst_to_parent_node: subst)
    (depth: int)
    (leaf: 'data leaf)
    : string =

    let subst_to_current_node =
      try
	Subst.to_immutable (
	Unification.unify_substs (Subst.to_mutable subst_to_parent_node) (Subst.to_mutable leaf.sl_subst)
	)
      with
	| Unification.UNIFICATION_FAIL ->
	    print_endline ("subst_to_parent: " ^ Subst.subst_to_string subst_to_parent_node);
	    print_endline ("leaf_subst: " ^ Subst.subst_to_string leaf.sl_subst);
	    failwith "leaf_to_string"
    in
      
    let depth_prefix =
      String.make (2 * depth) ' ' ^ string_of_int depth ^ ": "
    in
      
    let indicator_vars =
      "Indicator_vars: [ " ^ String.concat ", " (List.map Var.to_string leaf.sl_indicators) ^ " ]"
    in

    let term =
      Subst.apply_to_term
	(Subst.to_mutable subst_to_current_node)
	root_term
	tree_offset
    in
    let term_representation =
      Term.term_to_string term
    in
      
    let data_representation =
      _data#to_string leaf.sl_data
    in
      
      "\n" ^ depth_prefix ^ term_representation ^ ": " ^ data_representation ^ "\n"
      ^ depth_prefix ^ Subst.subst_to_string leaf.sl_subst ^ "\n"
      ^ indicator_vars ^ "\n"
	

  method private node_to_string
    (root_term: term)
    (subst_to_parent_node: subst)
    (depth: int)
    (node: 'data inner_node)
    (leaf_count: int ref)
    (inner_count: int ref)
    : string =
    
    inner_count := !inner_count + 1;

    let subst_to_current_node =
      try
	Subst.to_immutable (
	Unification.unify_substs (Subst.to_mutable subst_to_parent_node) (Subst.to_mutable node.sn_subst)
	)
      with
	| Unification.UNIFICATION_FAIL ->
	    print_endline ("subst_to_parent: " ^ Subst.subst_to_string subst_to_parent_node);
	    print_endline ("node_subst: " ^ Subst.subst_to_string node.sn_subst);
	    failwith "node_to_string"
    in
      
    let depth_prefix =
      String.make (2 * depth) ' ' ^ string_of_int depth ^ ": "
    in
      
    let open_vars =
      "Open_vars: [ " ^ String.concat ", " (List.map Subst.var_to_string node.sn_open_vars) ^ " ]"
    in
    let indicator_vars =
      "Indicator_vars: [ " ^ String.concat ", " (List.map Var.to_string node.sn_indicators) ^ " ]"
    in

      
    let term =
      Subst.apply_to_term
	(Subst.to_mutable subst_to_current_node)
	root_term
	tree_offset
    in
    let term_representation =
      Term.term_to_string term
    in
      
    let children_representation =
      String.concat ""
	(List.map
	   (fun child_node ->
	      match child_node with
		| Leaf leaf ->
		    leaf_count := !leaf_count + 1;
		    self#leaf_to_string
		      root_term
		      subst_to_current_node
		      (depth + 1)
		      leaf

		| Node node ->
		    self#node_to_string
		      root_term
		      subst_to_current_node
		      (depth + 1)
		      node
		      leaf_count
		      inner_count
	   )
	   node.sn_children)
    in

      "\n" ^ depth_prefix
	(*      ^ "(" ^ string_of_int node.sn_depth ^ ")"*)
      ^ term_representation ^ "\n"
      ^ depth_prefix ^ Subst.subst_to_string node.sn_subst ^ "\n"
      ^ open_vars ^ "\n"
      ^ indicator_vars ^ "\n"
      ^ "\n" ^ children_representation



  method to_string: string =
    let leaf_count = ref 0
    in
    let inner_count = ref 0
    in
      match _root with
	| None ->
	    "empty tree"

	| Some (Leaf leaf) ->
	    self#leaf_to_string
	      (self#get_root_term)
	      (Subst.create ())
	      0
	      leaf

	| Some (Node node) ->
	    let rep =
	      self#node_to_string
		(self#get_root_term)
		(Subst.create ())
		0
		node
		leaf_count
		inner_count
	    in
	      "LEAFS: " ^ string_of_int !leaf_count
	      ^ "  NODES: " ^ string_of_int !inner_count
	      ^ rep















  (*** mscg ***)

  (* computation of the most specific generalization
     of two terms/substitutions, e.g.:
     f(a, X, g(a)) and f(a, Y, g(Y)) -> f(a, X, g(Z)); [Z -> a]; [Z -> X]

     I.e. the substitutions applied to the mscg term produce variants
     of the original terms, not the terms themselves.
     
     these functions are specific for the usage in a substitution tree.
     they expect some prerequirements:
     - the substitutions only contain one offset: tree_offset
     - the left term/substitution is alread part of the tree, i.e. a leaf or node,
     the right term/substitution represents the term to insert into the tree.
  *)

  method private mscg_of_terms
    (* build the mscg of these terms *)
    (_tree_term: term) (_query_term: term)

    (* when called by mscg_of_substs these contain generalizations
       of previous calls to mscg_of_terms for other terms
       of the tree and the query *)
    (_tree_subst: subst) (_query_subst: subst)

    (* the open vars of the parent node. *)
    (_open_vars: Subst.var list)
    (_closed_vars: Subst.var list)
    : term * subst * subst =

    (* create a new variable to generalize over the two incompatible terms *)
    let do_generalize (tree_term: term) (query_term: term) (tree_subst: subst) (query_subst: subst)
	: term * subst * subst =

      let var =
	Var.create_universal (Counter.next _counter)
      in
	Term.request_var var,
	Subst.set_ tree_subst var tree_offset tree_term tree_offset,
	Subst.set_ query_subst var tree_offset query_term tree_offset
    in


    (* generalize over two incompatible terms *)
    let generalize (tree_term: term) (query_term: term) (tree_subst: subst) (query_subst: subst)
	: term * subst * subst =

      (* try to reuse previous generalizations over the same terms.
	 only non-open vars may be reused.
	 otherwise open vars might be lost due tue variable reusage and never closed. *)

      (* if this is the top term this binding will be pruned
	 by mscg_of_subst to the open var on top of the two top terms.
	 thuse an open var is involved and reusage is not possible. *)
      if Term.term_equal _tree_term tree_term then begin
	do_generalize tree_term query_term tree_subst query_subst
      end

      (* try to find a previous generalization over the same two terms *)
      else begin
	try
	  let reuse_binding =
	    Subst.find
	      (fun binding ->
		 (* var is bound to tree_term ... *)
		 Term.term_equal tree_term binding.Subst.sb_term.Subst.st_term
		 &&
		   (* and var is not an open var ... *)
		   (List.for_all
		      (fun open_var ->
			 not (Var.equal binding.Subst.sb_var.Subst.sv_var open_var.Subst.sv_var)
		      )
		      _open_vars
		   )
		 &&
		   (* and var is bound to query_term ... *)
		   (Subst.exists
		      (fun binding2 ->
			 Var.equal binding.Subst.sb_var.Subst.sv_var binding2.Subst.sb_var.Subst.sv_var
			 &&
			   Term.term_equal query_term binding2.Subst.sb_term.Subst.st_term
		      )
		      query_subst
		   )
	      )
	      tree_subst
	  in
	    Term.request_var reuse_binding.Subst.sb_var.Subst.sv_var, tree_subst, query_subst
	with
	  | Not_found ->
	      do_generalize tree_term query_term tree_subst query_subst
      end
    in


    let rec do_mscg_of_terms (tree_term: term) (query_term: term) (tree_subst: subst) (query_subst: subst)
	: term * subst * subst =
      (* try to use var as the generalization,
	 i.e. avoid doing anything for the left side *)
      match tree_term, query_term with
	| Term.Var var, _ when
	    (* indicator may never be bound in an mscg *)
	    not (Var.is_indicator var)
	    &&
	      (* reusage of open vars is handled in mscg_of_subst
		 as the open var might also be bound in the query subst. *)
	      List.for_all
	      (fun open_var -> not (Var.equal var open_var.Subst.sv_var))
	      _open_vars

	    &&
	      List.for_all
	      (fun closed_var -> not (Var.equal var closed_var.Subst.sv_var))
	      _closed_vars

	    ->

	    begin
	      match Subst.get_ query_subst var tree_offset with
		| None ->
		    tree_term,
		    tree_subst,
		    Subst.set_ query_subst var tree_offset query_term tree_offset
		      
		(* already bound to the same term?
		   so the old binding can be reused by simply reusing the variable *)
		| Some bound_term when
		    Term.term_equal query_term bound_term.Subst.st_term ->
		    tree_term, tree_subst, query_subst
		      
		| _ ->
		    generalize tree_term query_term tree_subst query_subst
	    end


	| Term.Const symbol1, Term.Const symbol2 when
	    Symbol.equal symbol1 symbol2 ->

	    tree_term, tree_subst, query_subst


	| Term.Func (symbol1, terms1, _), Term.Func (symbol2, terms2, _) when
	    Symbol.equal symbol1 symbol2 ->

	    let new_terms =
	      Array.copy terms1
	    in
	      
	    let rec map_terms (index: int) (subst1: subst) (subst2: subst) : subst * subst =
	      if index >= Array.length terms1 then
		subst1, subst2

	      else
		let mscg, subst1, subst2 =
		  do_mscg_of_terms terms1.(index) terms2.(index) subst1 subst2
		in
		  new_terms.(index) <- mscg;
		  map_terms (index + 1) subst1 subst2
	    in
	      
	    let subst1, subst2 =
	      map_terms 0 tree_subst query_subst
	    in
	      
	      Term.request_func (symbol1, new_terms), subst1, subst2;

	      
	| _ ->
	    generalize tree_term query_term tree_subst query_subst
	      
    in
      do_mscg_of_terms _tree_term _query_term _tree_subst _query_subst





  method private mscg_of_substs
    (* build the mscg of these substitutions *)
    (_tree_subst: subst) (_query_subst: subst)

    (* the open vars of the parent node. *)
    (_open_vars_parent: Subst.var list)
    (* the open vars of the parent node. *)
    (_closed_vars_parent: Subst.var list)

    (* the open vars of the tree node. *)
    (_open_vars_tree: Subst.var list)

    (* the indicator vars used in the parent substitutions *)
    (_parent_indicators: Var.var list)

    (* the indicator vars used in the query substitution *)
    (_query_indicators: Var.var list)
    : subst * subst * subst =

    (* get the binding of open_var in _query_subst *)
    let get_query_only_bound_var (open_var: Subst.var) : binding =
      try
	Subst.find
	  (fun binding ->
	     Subst.var_equal open_var binding.Subst.sb_var
	  )
	  _query_subst
      with
	| Not_found ->
	    print_endline ("TREE: " ^ self#to_string);
	    print_endline ("OPEN_VAR: " ^ Subst.var_to_string open_var);
	    print_endline ("TREE_SUBST: " ^ Subst.subst_to_string _tree_subst);
	    print_endline ("QUERY_SUBST: " ^ Subst.subst_to_string _query_subst);
	    failwith "get_query_only_bound_var"
    in

    (* get the bindinds of open_var in _term_subst and _query_subst *)
    let get_binding_pair (open_var: Subst.var) : binding * binding =
      try
	Subst.find
	  (fun binding ->
	     Subst.var_equal open_var binding.Subst.sb_var
	  )
	  _tree_subst,
	  
	  Subst.find
	    (fun binding ->
	       Subst.var_equal open_var binding.Subst.sb_var
	    )
	    _query_subst
      with
	| Not_found ->
	    print_endline ("TREE: " ^ self#to_string);
	    print_endline ("OPEN_VAR: " ^ Subst.var_to_string open_var);
	    print_endline ("TREE_SUBST: " ^ Subst.subst_to_string _tree_subst);
	    print_endline ("QUERY_SUBST: " ^ Subst.subst_to_string _query_subst);
	    failwith "get_binding_pair"
    in




      
    (* the mscg of two terms was mscg_term, which is mscg_var.
       thus the generalization basically failed, as the only generalization is a variable.
       so try to reuse variables from equal previous generalizations *)
    let reuse_previous_generalizations
	(open_var: Subst.var)
	(mscg_term: term) (mscg_var: Var.var)
	(mscg_subst: subst) (new_mscg_tree: subst) (new_mscg_query: subst)
	(* previous to the generalization to mscg_term *)
	(old_mscg_tree: subst) (old_mscg_query: subst)
	: subst * subst * subst =


      (* does a generalization with a variable (x)
	 over the same terms (t1, t2) already exist?
	 if so, reuse it:
	 replace open_var -> mscg_var; mscg_var -> t1; mscg_var -> t2
	 with open_var -> x if x -> t1, x -> t2 does already exist.

	 raises Not_found on failure *)
      let bind_to_previous_generalization
	  (tree_binding: binding) (query_binding: binding) : subst * subst * subst =

	(* find such a reusable generalization *)
	let reuse_binding =
	  Subst.find
	    (fun binding1 ->
	       Subst.term_equal tree_binding.Subst.sb_term binding1.Subst.sb_term
	       &&
		 not (Var.equal mscg_var binding1.Subst.sb_var.Subst.sv_var)
	       &&
		 (
		   Subst.exists
		     (fun binding2 ->
			Subst.var_equal binding1.Subst.sb_var binding2.Subst.sb_var
			&&
			  Subst.term_equal query_binding.Subst.sb_term binding2.Subst.sb_term
		     )
		 )
		 new_mscg_query
	    )
	    new_mscg_tree
	in

	(* so bind open_var to the previously used generalization var
	   and drop the new generalizations *)
	let new_mscg_term =
	  Term.request_var reuse_binding.Subst.sb_var.Subst.sv_var
	in
	let mscg_binding = {
	  Subst.sb_var = open_var;
	  Subst.sb_term = Subst.make_term new_mscg_term tree_offset;
	}
	in
	  (Subst.append mscg_binding mscg_subst), old_mscg_tree, old_mscg_query
      in



      (* the mscg is only a variable, thus we have:
	 open_var -> mscg_var; mscg_var -> t1; mscg_var -> t2
	 and can get rid of mscg_var with
	 ; open_var -> t1; open_var -> t2 *)
      let prune_mscg_var
	  (tree_binding: binding) (query_binding: binding) : subst * subst * subst =
	
	let prune_subst (subst: subst) : subst =
	  Subst.map
	    (fun binding ->
	       if Var.equal mscg_var binding.Subst.sb_var.Subst.sv_var then
		 { binding with
		     Subst.sb_var = {
		       binding.Subst.sb_var with
			 Subst.sv_var = open_var.Subst.sv_var
		     }
		 }
	       else
		 binding
	    )
	    subst
	in
	  (*	print_endline ("\nPRUNE_MSCG_VAR: " ^ Subst.var_to_string open_var);
		print_endline ("\nopen_var: " ^ Subst.var_to_string open_var);
		print_endline ("\nmscg_var: " ^ Var.to_string mscg_var);*)
	  mscg_subst, prune_subst new_mscg_tree, prune_subst new_mscg_query
      in



      (* reusage is only done if mscg_var is bound in mscg_tree and mscg_query

	 raises Not_found on failure *)
      let do_reuse_previous_generalizations () : subst * subst * subst =
	(* is mscg_var bound in new_mscg_tree? if not, fail *)
	let tree_binding =
	  Subst.find
	    (fun binding ->
	       Var.equal mscg_var binding.Subst.sb_var.Subst.sv_var
	    )
	    new_mscg_tree
	in
	  try
	    let query_binding =
	      Subst.find
		(fun binding ->
		   Var.equal mscg_var binding.Subst.sb_var.Subst.sv_var
		)
		new_mscg_query
	    in
	      begin
		try
		  bind_to_previous_generalization tree_binding query_binding
		with
		  | Not_found ->
		      prune_mscg_var tree_binding query_binding
	      end		       
	  with
	    | Not_found ->
		print_endline ("MSCG_VAR: " ^ Var.to_string mscg_var);
		print_endline ("TREE_MSCG: " ^ Subst.subst_to_string new_mscg_tree);
		print_endline ("QUERY_MSCG: " ^ Subst.subst_to_string new_mscg_query);
		failwith "Substitution_tree.do_reuse_previous_generalizations"
      in


	try
	  do_reuse_previous_generalizations ()
	with
	  | Not_found ->
	      (* no, so mscg_var was a variable from tree_subst reused to generalize.
		 just keep it *)
	      let mscg_binding = {
		Subst.sb_var = open_var;
		Subst.sb_term = Subst.make_term mscg_term tree_offset;
	      }
	      in
		Subst.append mscg_binding mscg_subst,
		new_mscg_tree,
		new_mscg_query
    in


    (* compute the mscg for the two substs
       and do all variable reusing/optimization except for indicator variables *)
    let compute_mscg () : subst * subst * subst =
      List.fold_left
	(fun (mscg_subst, mscg_tree, mscg_query) open_var ->
	   (* if this open_var remains an open var for the tree_subst
	      is is only bound in the query_subst
	      (and must be bound there as it is a leaf)
	      and the binding can just be propagated *)
	   if List.exists (Subst.var_equal open_var) _open_vars_tree then begin
	     mscg_subst, mscg_tree, Subst.append (get_query_only_bound_var open_var) mscg_query
	   end

	   else begin
	     let first_binding, second_binding =
	       get_binding_pair open_var
	     in
	     let mscg_term, new_mscg_tree, new_mscg_query =
	       self#mscg_of_terms
		 first_binding.Subst.sb_term.Subst.st_term
		 second_binding.Subst.sb_term.Subst.st_term
		 mscg_tree
		 mscg_query
		 _open_vars_parent
		 _closed_vars_parent
	     in
	       begin
		 match mscg_term with
		   | Term.Var mscg_var ->
 		       (* generalization basically failed, as the only generalization is a variable.
			  so try to reuse variables from equal previous generalizations *)
		       reuse_previous_generalizations
			 open_var
			 mscg_term
			 mscg_var
			 mscg_subst
			 new_mscg_tree
			 new_mscg_query
			 mscg_tree
			 mscg_query

		   | _ ->
		       (* some generalization has been done, add it to the mscg *)
		       let mscg_binding = {
			 Subst.sb_var = open_var;
			 Subst.sb_term = Subst.make_term mscg_term tree_offset;
		       }
		       in
			 Subst.append mscg_binding mscg_subst,
			 new_mscg_tree,
			 new_mscg_query
	       end
	   end
	)
	(Subst.create (), Subst.create (), Subst.create ())
	_open_vars_parent
    in


    (* replace the query indicator variable by the tree indicator variable.
       this permanently changes the indicator variables
       contained in the query leaf *)
    let use_tree_indicator
	(tree_indicator_var: Var.var)
	(query_indicator_var: Var.var)
	(tree_binding: binding) (query_binding: binding)
	(mscg_subst: subst) (mscg_tree: subst) (mscg_query: subst)
	: subst * subst * subst =

      (* if the variable used to generalize the indicator variables is
	 a open var of the parent node it must be kept *)
      let new_mscg =
	if List.exists (Subst.var_equal tree_binding.Subst.sb_var) _open_vars_parent then
	  (* open var, so bind it to the tree indicator *)
	  let mscg_binding = {
	    Subst.sb_var = tree_binding.Subst.sb_var;
	    Subst.sb_term = Subst.make_term (Term.request_var tree_indicator_var) tree_offset;
	  }
	  in
	    Subst.append mscg_binding mscg_subst
	else
	  (* new generalization var, replace it with the tree indicator var *)
	  Subst.replace_in_bound_terms
	    mscg_subst
	    (Term.request_var tree_binding.Subst.sb_var.Subst.sv_var)
	    (Term.request_var tree_indicator_var)
      in

      (* remove the lifted bindings *)
      let pruned_mscg_tree =
	Subst.find_all
	  (fun binding -> tree_binding != binding)
	  mscg_tree
      in
      let pruned_mscg_query =
	Subst.find_all
	  (fun binding -> query_binding != binding)
	  mscg_query
      in
	
      (* replace the query indicator with the tree indicator used in the mscg *)
      let mapped_mscg_query =
	if Var.equal query_indicator_var tree_indicator_var then
	  pruned_mscg_query
	else
	  Subst.replace_in_bound_terms
	    pruned_mscg_query
	    (Term.request_var query_indicator_var)
	    (Term.request_var tree_indicator_var)
      in
	new_mscg, pruned_mscg_tree, mapped_mscg_query
    in



    let is_liftable
	(tree_indicator: Var.var) (query_indicator: Var.var)
	(common_indicators: Var.var list)
	: bool =
      Var.equal tree_indicator query_indicator
      ||
	(
	  Var.is_indicator query_indicator
	  &&
	    (Var.is_universal tree_indicator = Var.is_universal query_indicator)
	  &&
	    (* an indicator which is part of the mscg or parent node can not be touched.
	       otherwise indicators which are equal in the tree subst
	       but different in the query subst
	       might be replaced by the same indicator,
	       thus changing the query subst *)
	    (not
	       (List.exists
		  (fun common ->
		     (* f(x, x) and f(x, y) *)
		     Var.equal tree_indicator common
		     ||
		       (* f(x, y) and f(x, x) considering var replacement *)
		       Var.equal query_indicator common
		  )
		  common_indicators)
	    )
	  &&
	    (* similiar as above.
	       but an indicator which is only in the query,
	       neither in the mscg or parent node,
	       may be replaced by another indicator -
	       is this replacing indicator is not already part of the query subst *)
	    (not
	       (List.exists
		  (fun blocked ->
		     (* f(x, x) and f(x, y) *)
		     Var.equal tree_indicator blocked
		       (*					    ||
		       (* f(x, y) and f(x, x) considering var replacement *)
								    Var.equal right_indicator blocked*)
		  )
		  _query_indicators)
	    )
	)
    in

    (* try to reuse indicator variables from the tree_subst
       by lifting them to the mscg and replacing the corresponding
       indicator variables in query_subst. *)
    let lift_indicators
	(_mscg_subst: subst) (_mscg_tree: subst) (_mscg_query: subst)
	: subst * subst * subst =

      let (mscg_subst, mscg_tree, mscg_query, _) =
	Subst.fold
	  (fun (mscg_subst, mscg_tree, mscg_query, common_indicators as acc) tree_binding ->
	     match tree_binding.Subst.sb_term.Subst.st_term with
	       | Term.Var tree_indicator when
		   Var.is_indicator tree_indicator ->
		   
		   begin
		     try
		       let query_binding =
			 Subst.find
			   (fun binding ->
			      Subst.var_equal tree_binding.Subst.sb_var binding.Subst.sb_var
			   )
			   mscg_query
		       in
			 begin
			   match query_binding.Subst.sb_term.Subst.st_term with
			     | Term.Var query_indicator ->
				 if is_liftable tree_indicator query_indicator common_indicators then
				   let new_mscg, new_tree_mscg, new_query_mscg =
				     use_tree_indicator
				       tree_indicator query_indicator
				       tree_binding query_binding
				       mscg_subst mscg_tree mscg_query
				   in
				     (* tree_indicator is lifted to the mscg
					and thus now also used in both the tree and the query subst *)
				     new_mscg, new_tree_mscg, new_query_mscg, tree_indicator :: common_indicators
				 else
				   acc
				     
			     | _ ->
				 acc
			 end

		     with
		       | Not_found ->
			   (* generalization variables of the tree must also be in the query *)
			   failwith "lift_indicators";
		   end
		     
	       | _ ->
		   acc
	  )
	  (_mscg_subst, _mscg_tree, _mscg_query, _parent_indicators)
	  _mscg_tree
      in
	mscg_subst, mscg_tree, mscg_query
    in


    let mscg_subst, mscg_tree, mscg_query =
      compute_mscg ()
    in
      if
	Const.debug
	&&
	  (Subst.exists
	     (fun binding ->
		List.exists
		  (fun var ->
		     not (Var.is_indicator var)
		  )
		  (Term.vars_of_term binding.Subst.sb_term.Subst.st_term)
	     )
	     mscg_query)
      then begin
	print_endline ("TREE_SUBST: " ^ Subst.subst_to_string _tree_subst);
	print_endline ("QUERY_SUBST: " ^ Subst.subst_to_string _query_subst);
	print_endline ("OPEN_VARS_PARENT: " ^ String.concat ", " (List.map Subst.var_to_string _open_vars_parent));
	print_endline ("OPEN_VARS_TREE: " ^ String.concat ", " (List.map Subst.var_to_string _open_vars_tree));
	print_endline ("INDICATORS_PARENT: " ^ String.concat ", " (List.map Var.to_string _parent_indicators));
	print_endline ("INDICATORS_QUERY: " ^ String.concat ", " (List.map Var.to_string _query_indicators));
	print_endline ("MSCG: " ^ Subst.subst_to_string mscg_subst);
	print_endline ("TREE_MSCG: " ^ Subst.subst_to_string mscg_tree);
	print_endline ("QUERY_MSCG: " ^ Subst.subst_to_string mscg_query);
	failwith "non-indicator in leaf mscg"
      end;
      let mscg_subst, mscg_tree, mscg_query =
	lift_indicators mscg_subst mscg_tree mscg_query
      in
	if
	  Const.debug
	  &&
	    (Subst.exists
	       (fun binding ->
		  List.exists
		    (fun var ->
		       not (Var.is_indicator var)
		    )
		    (Term.vars_of_term binding.Subst.sb_term.Subst.st_term)
	       )
	       mscg_query)
	then begin
	  print_endline ("TREE_SUBST: " ^ Subst.subst_to_string _tree_subst);
	  print_endline ("QUERY_SUBST: " ^ Subst.subst_to_string _query_subst);
	  print_endline ("OPEN_VARS_PARENT: " ^ String.concat ", " (List.map Subst.var_to_string _open_vars_parent));
	  print_endline ("OPEN_VARS_TREE: " ^ String.concat ", " (List.map Subst.var_to_string _open_vars_tree));
	  print_endline ("INDICATORS_PARENT: " ^ String.concat ", " (List.map Var.to_string _parent_indicators));
	  print_endline ("INDICATORS_QUERY: " ^ String.concat ", " (List.map Var.to_string _query_indicators));
	  print_endline ("MSCG: " ^ Subst.subst_to_string mscg_subst);
	  print_endline ("TREE_MSCG: " ^ Subst.subst_to_string mscg_tree);
	  print_endline ("QUERY_MSCG: " ^ Subst.subst_to_string mscg_query);
	  failwith "non-indicator in lifted mscg"
	end;
	mscg_subst, mscg_tree, mscg_query








  (*** add ***)

  (* sort nodes by increasing depth *)
  method private sort_nodes (nodes: 'data node list) : 'data node list =
    List.sort
      (fun first second ->
	 match first, second with
	   | Leaf _, Leaf _ ->
	       0
	       
	   | Leaf _, Node _ ->
	       -1
	       
	   | Node _, Leaf _ ->
	       1
	       
	   | Node first_node, Node second_node ->
	       compare first_node.sn_depth second_node.sn_depth
      )
      nodes


  (* remove indicator renamings *x -> *y by
     dropping *x -> *y
     and replacing all occurences of *y by *x in the substitution

     the substitution must only contain one offset - i.e. tree_offset *)
  method private prune_query_indicators (subst: subst) : subst =

    let bindings_to_prune, bindings_to_keep =
      Subst.partition
	(fun binding ->
	   Var.is_indicator binding.Subst.sb_var.Subst.sv_var
	   &&
	   (
	     match binding.Subst.sb_term.Subst.st_term with
	       | Term.Var var ->
		   Var.is_universal binding.Subst.sb_var.Subst.sv_var
		   =
	           Var.is_universal var
		
	       | _ ->
		   false
	   )
	)
	subst
    in
      Subst.fold
	(fun acc binding ->
	   let old_term =
	     binding.Subst.sb_term.Subst.st_term
	   in
	   let new_term =
	     Term.request_var binding.Subst.sb_var.Subst.sv_var
	   in
	     Subst.replace_in_bound_terms acc old_term new_term
	)
	bindings_to_keep
	bindings_to_prune


  (* returns the indicator variables contained in the terms bound in subst *)
  method private get_bound_indicators (subst: subst) (acc: Var.var list) : Var.var list =
    Subst.fold
      (fun acc binding ->
	 let term_vars =
	   List.find_all
	     Var.is_indicator
	     (Term.vars_of_term binding.Subst.sb_term.Subst.st_term)
	 in
	   Tools.lists_merge Var.equal acc term_vars
      )
      acc
      subst



  (* finds a child node more general than the query.
     returns the node with its substitution *)
  method private find_subsuming_child (children: 'data node list) (subst_to_parent: subst) :
    ('data inner_node * subst) option =
    match children with
      | [] ->
	  None

      | Leaf _ :: tail ->
	  (* a leaf can not be (truely) more general, as it contains no open vars.
	     but it might be a variant. *)
	  self#find_subsuming_child tail subst_to_parent

      | Node child_node :: tail ->
	  begin
	    try
	      let subst_to_child =
		Unification.match_substs
		  ~p_preserving:true ~i_preserving:true
		  tree_offset (Subst.to_mutable subst_to_parent) (Subst.to_mutable child_node.sn_subst)
	      in
		Some (child_node, Subst.to_immutable subst_to_child)
	    with
	      | Unification.UNIFICATION_FAIL ->
		  self#find_subsuming_child tail subst_to_parent
	  end


  (* finds the child node with the 'best' mscg with respect to the query.
     returns the node with its mscg, tree, and query substitution.
     
     assumes that all substs contain only tree_offset as an offset *)
  method private find_mscg_child
    (_parent: 'data inner_node)
    (_parent_to_query_subst: subst)
    : ('data node * subst * subst * subst) option =

    let query_indicators =
      self#get_bound_indicators _parent_to_query_subst []
    in

    let rec do_find_mscg_child
      (children: 'data node list) (acc: ('data node * subst * subst * subst) option)
      : ('data node * subst * subst * subst) option =

      match children with
	| [] ->
	    acc
	    
	| child_node :: tail ->
	    begin
	      let open_vars_tree =
		match child_node with
		  | Leaf _ ->
		      []
		  | Node node ->
		      node.sn_open_vars
	      in
	      let mscg_subst, mscg_tree, mscg_query =
		self#mscg_of_substs
		  (self#get_node_subst child_node)
		  _parent_to_query_subst
		  _parent.sn_open_vars
		  _parent.sn_closed_vars
		  open_vars_tree
		  _parent.sn_indicators
		  query_indicators
	      in
		(* ignore empty mscgs *)
		if Subst.is_empty mscg_subst then begin
		  do_find_mscg_child tail acc
		end

		(* choose the best mscg *)
		else
		  begin
			match acc with
			  | Some (_, old_mscg_subst, old_mscg_tree, old_mscg_query) when
			      (* the new mscg is not better than the old one *)
			      (Subst.length old_mscg_tree + Subst.length old_mscg_query)
			      <=
			      (Subst.length mscg_tree + Subst.length mscg_query)
			      ->
			      do_find_mscg_child tail acc

			  | _ ->
			      let new_acc =
				Some (child_node, mscg_subst, mscg_tree, mscg_query)
			      in
				do_find_mscg_child tail new_acc
		      end
	    end
    in
      do_find_mscg_child _parent.sn_children None







  (* the term to insert is subsumed by node, so
     - find a subsuming child and descend
     - create an mscg with a child
     - or add as a new leaf *)
  method private add_node
    (node: 'data inner_node)
    (* substitution from root to node, including the subsumed query*)
    (subst_to_node: subst)
    (term: term) (data: 'data) : unit =

    (* try to find a subsuming child *)
    let subsuming_child =
      self#find_subsuming_child node.sn_children subst_to_node
    in
      begin
	match subsuming_child with
	  | Some (child_node, subst_to_child) ->
	      (* found *)
	      let old_child_depth =
		child_node.sn_depth
	      in
		(* descend *)
		self#add_node child_node subst_to_child term data;

		(* reorder children if necessary *)
		if child_node.sn_depth > old_child_depth then begin
		  node.sn_children <- self#sort_nodes node.sn_children;

		  if child_node.sn_depth >= node.sn_depth then
		    node.sn_depth <- child_node.sn_depth + 1;
		end;
		  
	  | None ->
	      (* failed, so try to find a child with a non-empty mscg *)
	      begin
		(* normalize the substitution: *)
		(* only use tree_offset *)
		let subst_to_node =
		  Subst.replace_offset subst_to_node query_offset tree_offset
		in

		(* and only keep the bindings of open variables of the parent node *)
		let subst_to_node =
		  self#prune_query_indicators subst_to_node
		in


		(* extract the query subst from the combination
		   of the node and the query subst *)
		let node_to_query_subst =
		  Subst.find_all
		    (fun binding ->
		       List.exists
		       (fun open_var ->
			  Subst.var_equal binding.Subst.sb_var open_var
		       )
		       node.sn_open_vars
		    )
		    subst_to_node
		in

		let mscg_child =
		  self#find_mscg_child
		    node
		    node_to_query_subst
		in
		  match mscg_child with
		    | None ->
			let child_indicators =
			  self#get_bound_indicators node_to_query_subst node.sn_indicators
			in

			let new_child_node = {
			  sl_subst = node_to_query_subst;
			  sl_term = term;
			  sl_data = data;
			  sl_indicators = child_indicators;
			}
			in
			  node.sn_children <- (Leaf new_child_node) :: node.sn_children;
			  
		    | Some (child_node, mscg_subst, left_subst, right_subst) ->
			let mscg_open_vars =
			  List.find_all
			    (fun open_var ->
			       not (Var.is_indicator open_var.Subst.sv_var)
			    )
			    (Subst.get_bound_vars mscg_subst node.sn_open_vars)
			in                      

			let mscg_indicators =
			  self#get_bound_indicators mscg_subst node.sn_indicators
			in
			let left_indicators =
			  self#get_bound_indicators right_subst mscg_indicators
			in
			  
			let left_node = Leaf {
			  sl_subst = right_subst;
			  sl_term = term;
			  sl_data = data;
			  sl_indicators = left_indicators;
			}
			in
			let right_node, right_node_depth =
			  match child_node with
			    | Node inner_node ->
				Node {
				  inner_node with
				  sn_subst = left_subst;
				},
				inner_node.sn_depth

			    | Leaf leaf ->
				Leaf {
				  leaf with
				  sl_subst = left_subst;
				},
				0
			in
			let mscg_closed_vars =
			  Subst.fold
			    (fun acc binding ->
			       binding.Subst.sb_var :: acc
			    )			    
			    node.sn_closed_vars
			    mscg_subst
			in
			let mscg_node = {
			  sn_subst = mscg_subst;
			  sn_open_vars = mscg_open_vars;
			  sn_closed_vars = mscg_closed_vars;
			  sn_indicators = mscg_indicators;
			  sn_children = [left_node; right_node];
			  sn_depth = right_node_depth + 1;
			}
			in

			(* most often there are only two children,
			   so these operation are not going to be expensive *)
			let new_children =
			  List.map
			    (fun child ->
			       if child == child_node then
				 Node mscg_node
			       else
				 child
			    )
			    node.sn_children
			in
			  node.sn_children <- self#sort_nodes new_children;
			  if mscg_node.sn_depth >= node.sn_depth then
			    node.sn_depth <- mscg_node.sn_depth + 1
	      end
      end



  (* add term (with attached data) to the tree *)
  method add ?(no_duplicates: bool = false) (term: term) (data: 'data) : unit =
    if no_duplicates then begin
      failwith "Substitution_tree.add"
    end;

    _size <- _size + 1;

    match _root with
      | None ->
	  let root_var =
	    Var.create_universal (Counter.next _counter)
	  in
	    
	  let renumbered_term =
	    self#clone_term_renumbered term
	  in
	    
	  let subst =
	    Subst.set_ (Subst.create ()) root_var tree_offset renumbered_term tree_offset
	  in
	    
	  let indicators =
	    List.find_all
	      Var.is_indicator
	      (Term.vars_of_term renumbered_term)
	  in

	  let root_node = Leaf {
	    sl_subst = subst;
	    sl_term = term;
	    sl_data = data;
	    sl_indicators = indicators;
	  }
	  in
	    _root <- Some root_node
	    
      | Some (Leaf leaf) ->
	  let renumbered_term =
	    self#clone_term_renumbered term
	  in

	  let query_subst =
	    Subst.set (Subst.create ())
	      self#get_root_var
	      (Subst.make_term renumbered_term tree_offset)
	  in

	  let query_indicators =
	    List.find_all
	      Var.is_indicator 
	      (Term.vars_of_term renumbered_term)
	  in

	  let mscg_subst, mscg_tree, mscg_query =
	    self#mscg_of_substs
	      leaf.sl_subst
	      query_subst
	      [self#get_root_var]
	      []
	      []
	      []
	      query_indicators
	  in

	  let mscg_open_vars =
	    List.find_all
	      (fun open_var ->
		 not (Var.is_indicator open_var.Subst.sv_var)
	      )
	      (Subst.get_bound_vars mscg_subst [self#get_root_var])
	  in

	  let mscg_indicators =
	    self#get_bound_indicators mscg_subst []
	  in
	  let left_indicators =
	    self#get_bound_indicators mscg_query mscg_indicators
	  in

	  let left_node = Leaf {
	    sl_subst = mscg_query;
	    sl_term = term;
	    sl_data = data;
	    sl_indicators = left_indicators;
	  }
	  in
	  let right_node = Leaf {
	    leaf with
	    sl_subst = mscg_tree;
	  }
	  in
	  let mscg_closed_vars =
	    Subst.map'
	      (fun binding ->
		 binding.Subst.sb_var
	      )
	      mscg_subst
	  in
	  let mscg_node = Node {
	    sn_subst = mscg_subst;
	    sn_open_vars = mscg_open_vars;
	    sn_closed_vars = mscg_closed_vars;
	    sn_children = [left_node; right_node];
	    sn_depth = 1;
	    sn_indicators = mscg_indicators;
	  }
	  in
	    _root <- Some mscg_node
	    
      | Some (Node node) ->
	  begin
	    let renumbered_term =
	      self#clone_term_renumbered term
	    in
	      try
		(* instance of root? *)
		let subst_to_root =
		  Unification.match_terms
		    ~p_preserving:true ~i_preserving:true
		    self#get_root_term tree_offset
		    renumbered_term query_offset
		in
		  self#add_node node (Subst.to_immutable subst_to_root) term data

	      with
		| Unification.UNIFICATION_FAIL ->
		    (* no, so create a new root *)

		    let query_subst =
		      Subst.set
			(Subst.create ())
			self#get_root_var
			(Subst.make_term renumbered_term tree_offset)
		    in
		    let query_indicators =
		      List.find_all
			Var.is_indicator 
			(Term.vars_of_term renumbered_term)
		    in

		    let mscg_subst, mscg_tree, mscg_query =
		      self#mscg_of_substs
			node.sn_subst
			query_subst
			[self#get_root_var]
			[]
			[]
			[]
			query_indicators
		    in

		    let mscg_open_vars =
		      List.find_all
			(fun open_var ->
			   not (Var.is_indicator open_var.Subst.sv_var)
			)
			(Subst.get_bound_vars mscg_subst [self#get_root_var])
		    in

		    let mscg_indicators =
		      self#get_bound_indicators mscg_subst []
		    in
		    let left_indicators =
		      self#get_bound_indicators mscg_query mscg_indicators
		    in

		    let left_node = Leaf {
		      sl_subst = mscg_query;
		      sl_indicators = left_indicators;
		      sl_term = term;
		      sl_data = data;
		    }
		    in
		    let right_node = Node {
		      node with
		      sn_subst = mscg_tree;
		    }
		    in
		    let mscg_closed_vars =
		      Subst.map'
			(fun binding ->
			   binding.Subst.sb_var
			)
			mscg_subst
		    in
		    let mscg_node = Node {
		      sn_subst = mscg_subst;
		      sn_closed_vars = mscg_closed_vars;
		      sn_open_vars = mscg_open_vars;
		      sn_children = [left_node; right_node];
		      sn_depth = node.sn_depth + 1;
		      sn_indicators = mscg_indicators;
		    }
		    in
		      _root <- Some mscg_node
	  end









  (*** remove ***)

  (* merge the substitution of a node and its only child such that
     open vars/indicator vars, ... remains unchanged the child's children *)
  method private fold_node_with_child
    (* the open vars of the parent of node *)
    (node_parent_open_vars: Subst.var list)
    (* the mscg and its child to fold *)
    (node_subst: subst) (child_subst: subst)
    : subst =

    (* replace the vars in the child subst
       which are bound to another var by their bindings *)
    let applied_child_subst =
      Subst.fold
	(fun acc binding ->
	   Subst.replace_in_bound_terms
	     acc
	     (Term.request_var binding.Subst.sb_var.Subst.sv_var)
	     binding.Subst.sb_term.Subst.st_term
	)
	child_subst
	child_subst
    in

    (* replace the open vars of the parent subst
       which are bound in the child node
       by their bindings *)
    let applied_parent_subst =
      Subst.fold
	(fun acc binding ->
	   Subst.replace_in_bound_terms
	     acc
	     (Term.request_var binding.Subst.sb_var.Subst.sv_var)
	     binding.Subst.sb_term.Subst.st_term
	)
	node_subst
	applied_child_subst
    in
      (* the open vars of node are closed in applied_subst.
	 the bindings open vars of node's parent which were not bound in node
	 but in child must be propagated. *)
      Subst.fold
	(fun acc binding ->
	   if
	     List.exists
	       (fun open_var ->
		  Subst.var_equal binding.Subst.sb_var open_var
	       )
	       node_parent_open_vars
	   then
	     Subst.append binding acc
	   else
	     acc
	)
	applied_parent_subst
	applied_child_subst
	

  (* descend into node's children *)
  (* returns
     - a node (with only one child) to be replaced by its folded node
     - the data of the removed node  *)
  method private remove_in_children
    ~(equal: bool)
    (func: 'data node -> subst -> subst)
    (term: term) (data: 'data option)
    (node: 'data inner_node)
    (subst_to_node: subst)
    (children: 'data node list)
    : ('data inner_node * 'data node) option * bool =
    
    match children with
      | [] ->
	  None, false
	      
      | child_node :: tail ->
	  begin
	    try
	      let subst_to_child =
		func child_node subst_to_node
	      in
		begin
		  match child_node with
		    | Leaf leaf ->
			if
			  (
			    not equal
			    ||
			    Term.term_equal leaf.sl_term term
			  )
			  &&
			  match data with
			    | None ->
				true

			    | Some data ->
				_data#is_equal leaf.sl_data data
			then begin
			  (* found the leaf to remove *)
			  node.sn_children <-
			  List.find_all
			    (fun child -> child != child_node)
			    node.sn_children;
			  
			  None, true
			end

			else
			  (* didn't find the node under inner_node,
			     so try the next child *)
			  self#remove_in_children ~equal:equal func term data node subst_to_node tail
			  
		    | Node inner_node ->
			let replace_node_with, removed =
			  self#remove_node ~equal:equal func term data inner_node subst_to_child node.sn_open_vars
			in
			  begin
			    if removed then
			      (* found the node under inner_node, so return *)
			      replace_node_with, removed

			    else
			      (* didn't find the node under inner_node,
				 so try the next child *)
			      self#remove_in_children ~equal:equal func term data node subst_to_node tail
			  end
		end
	    with
	      | Unification.UNIFICATION_FAIL ->
		  (* node not suited for descending *)
		  self#remove_in_children ~equal:equal func term data node subst_to_node tail
	  end
	  

  method private remove_node
    ~(equal: bool)
    (func: 'data node -> subst -> subst)
    (term: term) (data: 'data option)
    (node: 'data inner_node)
    (subst_to_node: subst)
    (parent_open_vars: Subst.var list)
    : ('data inner_node * 'data node) option * bool =

    let replace_node_with, removed =
      self#remove_in_children ~equal:equal func term data node subst_to_node node.sn_children
    in
      (* just removed a leaf of a child of this node
	 and then folded the child and its now only remaining child? *)
      begin
	match replace_node_with with
	  | None ->
	      ()
	      
	  | Some (replace_node, replace_with) ->
	      begin
		let new_children =
		  List.map
		    (fun child ->
		       match child with
			 | Node inner_node when
			     inner_node == replace_node ->
			     replace_with
			       
			 | _ ->
			     child
		    )
		    node.sn_children 
		in
		  node.sn_children <- self#sort_nodes new_children;
	      end
      end;

      (* removed a node somewhere under node? *)
      begin
	if not removed then begin
	  replace_node_with, removed
	end
	else begin
	  (* yes, so update the depth *)
	  node.sn_depth <- 1 +
	  (List.fold_left
	     (fun acc child ->
		match child with
		  | Leaf _ ->
		      acc
		      
		  | Node inner_node ->
		      max acc inner_node.sn_depth
	     )
	     0
	     node.sn_children
	  );

	  (* is this the node whose child has been removed?
	     if so, it might have only once child left which needs to be folded.
	     every node must have at least two children. *)
	  begin
	    match node.sn_children with
	      | [] ->
		  failwith "Substitution_tree.remove_node: no children"
		  
	      | Leaf only_child :: [] ->
		  let new_subst =
		    self#fold_node_with_child parent_open_vars node.sn_subst only_child.sl_subst
		  in
		  let replace_with =
		    Leaf {
		      only_child with
			sl_subst = new_subst;
		    }
		  in
		    Some (node, replace_with), removed
		      
	      | Node only_child :: [] ->
		  let new_subst =
		    self#fold_node_with_child parent_open_vars node.sn_subst only_child.sn_subst
		  in
		  let replace_with =
		    Node {
		      only_child with
			sn_subst = new_subst;
		    }
		  in
		    Some (node, replace_with), removed
		      
	      | _ ->
		  None, removed
	  end
	end
	  
      end



  method remove (term: term) (data: 'data option) : bool =
    match _root with
      | None ->
	  false
	  
      | Some node ->
	  let normalized_term =
	    self#clone_term_renumbered term
	  in
	    
	  let query_subst =
	    Subst.set (Subst.create ())
	      self#get_root_var
	      (Subst.make_term normalized_term query_offset)
	  in
	    
	  let func node subst =
	    Subst.to_immutable (
	    Unification.match_substs
	      ~p_preserving:true ~i_preserving:true
	      tree_offset (Subst.to_mutable subst) (Subst.to_mutable (self#get_node_subst node))
	    )
	  in

	    try
	      let subst_to_node =
		func node query_subst
	      in
	      let removed =
		match node with
		  | Leaf leaf ->
		      _root <- None;
		      true

		  | Node inner_node ->
		      begin
			let replace_with_node, removed =
			  self#remove_node ~equal:true func term data inner_node subst_to_node [self#get_root_var];
			in
			  begin
			    match replace_with_node with
			      | None ->
				  ()

			      | Some (replace_node, replace_with) ->
				  if inner_node != replace_node then
				    failwith "Substitution_tree.remove_variant";
				  _root <- Some replace_with;
			  end; 
			  
			  removed
		      end
	      in
		if removed then
		  _size <- _size - 1;

		removed
	    with
	      | Unification.UNIFICATION_FAIL ->
		  false









  (*** requests ***)

  method private find_node ?(find_all: bool = false) (func: 'data node -> subst -> subst) (acc: 'data list)
    (node: 'data node) (subst_to_node: subst) : 'data list =

    match node with
      | Leaf leaf ->
	  if find_all then
	    leaf.sl_data :: acc
	  else begin
	    self#set_found leaf.sl_data;
	    raise FOUND;
	  end
	      
      | Node inner_node ->
	  List.fold_left
	  (fun acc child_node ->
	     let subst_to_child, success =
	       try
		 func child_node subst_to_node, true
	       with
		 | Unification.UNIFICATION_FAIL ->
		     Subst.create (), false
	     in
	       if success then
		 self#find_node ~find_all:find_all func acc child_node subst_to_child
	       else
		 acc
	  )
	  acc
	  inner_node.sn_children


  (* find the first or all elements according to the specified function *)
  method private start_find ~(find_all: bool) ~(p_preserving: bool)
    (func_root: term -> subst)
    (func: 'data node -> subst -> subst)
    (term: term) : 'data list =
    match _root with
      | None ->
	  []

      | Some node ->
	  begin
	    try
	      let subst_to_node =
		func_root term
	      in
		self#find_node ~find_all:find_all func [] node subst_to_node
	    with
	      | Unification.UNIFICATION_FAIL ->
		  []
	  end


  (* find the first element according to the specified function *)
  method private start_find_first ~(p_preserving: bool)
    (func_root: term -> subst)
    (func: 'data node -> subst -> subst)
    (term: term) :
    'data option =

    try
      let found =
	self#start_find
	  ~find_all:false ~p_preserving:p_preserving
	  func_root
	  func
	  term
      in
	match found with
	  | [] ->
	      None
	      
	  | _ ->
	      failwith "Substitution_tree.start_find_first"
    with
      | FOUND ->
	  self#get_found





  (*** variants ***)

  method find_all_variants (term: term) : 'data list =
    match _root with
      | None ->
	  []

      | Some node ->
	  let normalized_term =
	    self#clone_term_renumbered term
	  in

	  let func node subst =
	    Subst.to_immutable (
	    Unification.match_substs
	      ~p_preserving:true ~i_preserving:true
	      tree_offset (Subst.to_mutable subst) (Subst.to_mutable (self#get_node_subst node))
	    )
	  in
	    try
	      let subst_to_node =
		Unification.match_terms
		  ~p_preserving:true ~i_preserving:true
		  self#get_root_term tree_offset
		  normalized_term query_offset
	      in
		self#find_node ~find_all:true func [] node (Subst.to_immutable subst_to_node)
	    with
	      | Unification.UNIFICATION_FAIL ->
		  []

  method find (term: term) : 'data option =
    failwith "Substition_tree.find"


  (*** generalization ***)

  method private func_generalization_root ~(p_preserving: bool) (query_term: term) : subst =
    Subst.to_immutable (
    Unification.match_terms
      ~p_preserving:p_preserving
      self#get_root_term tree_offset
      query_term query_offset
    )

  method private func_generalization ~(p_preserving: bool) (node: 'data node) (subst: subst) : subst =
    Subst.to_immutable (
    Unification.match_substs
      ~p_preserving:p_preserving
      tree_offset (Subst.to_mutable subst) (Subst.to_mutable (self#get_node_subst node))
    )

  method find_all_generalizations ~(p_preserving: bool) (term: term) : 'data list =
    self#start_find
      ~find_all:true ~p_preserving:p_preserving
      (self#func_generalization_root ~p_preserving:p_preserving)
      (self#func_generalization ~p_preserving:p_preserving)
      term

  method find_generalization ~(p_preserving: bool) (term: term) : 'data option =
    self#start_find_first
      ~p_preserving:p_preserving
      (self#func_generalization_root ~p_preserving:p_preserving)
      (self#func_generalization ~p_preserving:p_preserving)
      term





  (*** unifiable ***)

  method private func_unifiable_root ~(p_preserving: bool) (query_term: term) : subst =
    Subst.to_immutable (
    Unification.unify_terms
      ~p_preserving:p_preserving
      self#get_root_term tree_offset
      query_term query_offset
    )

  method private func_unifiable ~(p_preserving: bool) (node: 'data node) (subst: subst) : subst =
    Subst.to_immutable (
    Unification.unify_substs
      ~p_preserving:p_preserving
      (Subst.to_mutable subst) (Subst.to_mutable (self#get_node_subst node))
    )

  method find_all_unifiable ~(p_preserving: bool) (term: term) : 'data list =
    self#start_find
      ~find_all:true ~p_preserving:p_preserving
      (self#func_unifiable_root ~p_preserving:p_preserving)
      (self#func_unifiable ~p_preserving:p_preserving)
      term

  method find_unifiable ~(p_preserving: bool) (term: term) : 'data option =
    self#start_find_first
      ~p_preserving:p_preserving
      (self#func_unifiable_root ~p_preserving:p_preserving)
      (self#func_unifiable ~p_preserving:p_preserving)
      term





  (*** instance ***)

  method private func_instance_root ~(p_preserving: bool) (query_term: term) : subst =
    Subst.to_immutable (
    Unification.unify_terms
      ~p_preserving:p_preserving ~i_preserving:true
      self#get_root_term tree_offset
      query_term query_offset
    )

  method private func_instance ~(p_preserving: bool) (node: 'data node) (subst: subst) : subst =
    Subst.to_immutable (
    Unification.unify_substs
      ~p_preserving:p_preserving ~i_preserving:true
      (Subst.to_mutable subst) (Subst.to_mutable (self#get_node_subst node))
    )

  method find_all_instances ~(p_preserving: bool) (term: term) : 'data list =
    self#start_find
      ~find_all:true ~p_preserving:p_preserving
      (self#func_instance_root ~p_preserving:p_preserving)
      (self#func_instance ~p_preserving:p_preserving)
      term

  method find_instance ~(p_preserving: bool) (term: term) : 'data option =
    self#start_find_first
      ~p_preserving:p_preserving
      (self#func_instance_root ~p_preserving:p_preserving)
      (self#func_instance ~p_preserving:p_preserving)
      term
      

      


  (*** strongly shielding ***)


  method private find_strongly_shielding_node
    (general: term) (node: 'data node)
    (subst_general_to_node: subst)
    (subst_node_to_instance: subst)
    : unit =

    match node with
      | Leaf leaf ->
	  (* ignore the producer? *)
	  if not (Term.term_equal leaf.sl_term general) then begin
	    self#set_found leaf.sl_data;
	    raise FOUND;
	  end
	      
      | Node inner_node ->
	  List.iter
	  (fun child_node ->
	     let subst_general_to_child, subst_child_to_instance, success =
	       try
		 self#func_instance ~p_preserving:false child_node subst_general_to_node,
		 self#func_generalization ~p_preserving:false child_node subst_node_to_instance,
		 true
	       with
		 | Unification.UNIFICATION_FAIL ->
		     Subst.create (), Subst.create (), false
	     in
	       if success then
		 self#find_strongly_shielding_node
		   general child_node
		   subst_general_to_child
		   subst_child_to_instance
	  )
	  inner_node.sn_children


  method find_strongly_shielding (general: term) (instance: term) : 'data option =

    match _root with
      | None ->
	  None

      | Some node ->
	  begin
	    try
	      let subst_general_to_node =
		self#func_instance_root ~p_preserving:false general
	      in
	      let subst_node_to_instance =
		self#func_generalization_root ~p_preserving:false instance
	      in
		self#find_strongly_shielding_node general node subst_general_to_node subst_node_to_instance;

		None
	    with
	      | Unification.UNIFICATION_FAIL ->
		  None

	      | FOUND ->
		  self#get_found
	  end





  (*** shielding ***)


  method private find_shielding_node
    ~(find_all: bool) (general: term) (node: 'data node)
    (subst_node_with_general: subst) (subst_node_to_instance: subst)
    (shielding_literals: 'data list)
    : 'data list =

    match node with
      | Leaf leaf ->
          if
	    (* the intersection term is instantiated p-preservingly *)
            (not (Subst.is_p_renaming subst_node_with_general tree_offset))
            ||
	    (* it's not the producer itself *)
	    (Term.term_equal leaf.sl_term general)
          then
	    shielding_literals

	  else if find_all then
	    leaf.sl_data :: shielding_literals

	  else begin
	    self#set_found leaf.sl_data;
	    raise FOUND
	  end

	      
      | Node inner_node ->
	  List.fold_left
	    (fun acc child_node ->
	       let subst_child_with_general, subst_child_to_instance, success =
		 try
		   self#func_unifiable ~p_preserving:false child_node subst_node_with_general,
		   self#func_generalization ~p_preserving:false child_node subst_node_to_instance,
		   true
		 with
		   | Unification.UNIFICATION_FAIL ->
		       Subst.create (), Subst.create (), false
	       in
		 if success then
		   self#find_shielding_node
		     ~find_all:find_all general child_node
		     subst_child_with_general subst_child_to_instance
		     acc
		 else
		   acc
	    )
	    shielding_literals
	    inner_node.sn_children



  method find_all_shielding (general: term) (instance: term) : 'data list =

    match _root with
      | None ->
	  []

      | Some node ->
	  try
	    let subst_node_with_general, subst_node_to_instance =
	      self#func_unifiable_root ~p_preserving:false general,
	      self#func_generalization_root ~p_preserving:false instance
	    in
	      self#find_shielding_node
		~find_all:true general node
		subst_node_with_general subst_node_to_instance
		[]
	  with
	    | Unification.UNIFICATION_FAIL ->
		[]


  method find_shielding (general: term) (instance: term) : 'data option =

    match _root with
      | None ->
	  None

      | Some node ->
	  try
	    let subst_node_with_general, subst_node_to_instance =
	      self#func_unifiable_root ~p_preserving:false general,
	      self#func_generalization_root ~p_preserving:false instance
	    in
	      ignore (
		self#find_shielding_node
			~find_all:false general node
			subst_node_with_general subst_node_to_instance
			[]
	      );

	      None
	  with
	    | Unification.UNIFICATION_FAIL ->
		None

	    | FOUND ->
		self#get_found








  (*** iteration ***)


  (* for debugging: checks that no variable is bound twice in a path *)
  method private validate_node
    (substs: subst list) (node: 'data node) : unit =

    let node_subst =
      self#get_node_subst node
    in
      try
	let reused_var =
	  Subst.find
	    (fun node_binding ->
	       List.exists
	       (fun previous_subst ->
		  Subst.exists
		  (fun previous_binding ->
		     Subst.var_equal
		     node_binding.Subst.sb_var
		     previous_binding.Subst.sb_var
		  )
		  previous_subst
	       )
	       substs
	    )
	    node_subst
	in
	  failwith ("variable reused: " ^ Subst.binding_to_string reused_var)
      with
	| Not_found ->
	    begin
	    match node with
	      | Leaf _ ->
		  ()

	      | Node inner_node ->
		  let new_substs =
		    node_subst :: substs
		  in
		    List.iter (self#validate_node new_substs) inner_node.sn_children
	  end

  method private validate : unit =
    match _root with
      | None ->
	  ()

      | Some node ->
	  self#validate_node [] node
	  



  method private iter_node
    (func: term -> 'data -> unit) (root_term: term) (subst_to_node: subst) (node: 'data node) : unit =

    match node with
      | Leaf leaf ->
	  let node_term =
	    Subst.apply_to_term
	      (Subst.to_mutable subst_to_node)
	      root_term
	      tree_offset
	  in
	  let normalized_term =
	    self#clone_term_without_indicator_vars node_term
	  in
	    func normalized_term leaf.sl_data;

      | Node inner_node ->
	  List.iter
	  (fun child_node ->
	     let child_subst =
	       Subst.to_immutable (
	       Unification.match_substs
		 ~p_preserving:true
		 tree_offset (Subst.to_mutable subst_to_node) (Subst.to_mutable (self#get_node_subst child_node))
	       )
	     in
	       self#iter_node func root_term child_subst child_node
	  )
	  inner_node.sn_children

  method iter (func: term -> 'data -> unit) : unit =
    match _root with
      | None ->
	  ()

      | Some node ->
	  self#iter_node
	    func
	    (self#get_root_term)
	    (Subst.create ())
	    node
	  



  method private fold_node:
    'a. ('a -> term -> 'data -> 'a) -> 'a -> term -> subst -> 'data node -> 'a =
    fun func acc term subst_to_node node ->

      match node with
	| Leaf leaf ->
	    let node_term =
	      Subst.apply_to_term
		(Subst.to_mutable subst_to_node)
		term
		tree_offset
	    in
	    let normalized_term =
	      self#clone_term_without_indicator_vars node_term
	    in
	      func acc normalized_term leaf.sl_data
		
	| Node inner_node ->
	    List.fold_left
	      (fun acc child_node ->
		 let child_subst =
		   Subst.to_immutable (
		   Unification.match_substs
		     ~p_preserving:true
		     tree_offset (Subst.to_mutable subst_to_node) (Subst.to_mutable (self#get_node_subst child_node))
		   )
		 in
		   self#fold_node func acc term child_subst child_node
	      )
	      acc
	      inner_node.sn_children

  method fold:
    'a. ('a -> term -> 'data -> 'a) -> 'a -> 'a =
    fun func acc ->

      match _root with
	| None ->
	    acc
	      
	| Some node ->
	    self#fold_node
	      func
	      acc
	      (self#get_root_term)
	      (Subst.create ())
	      node



  (* polymorphic function for creating an iterator object.
     creating the iterator object/class within a method
     makes the substitution tree's interna available to the iterator object. *)
  method private create_iterator:
    'b.
    (('data node * 'b) list) ->
    (('data leaf * 'b) -> bool) ->
    (('data node * 'b) -> 'data node -> ('data node * 'b)) ->
(*   < advance : unit; is_advanced : bool; is_empty : bool; next : 'data >*)
    'data iterator
      =
    fun initial_nodes is_leaf_valid create_child_node ->

    let iterator =
      object (iterator)
	(* contains the root nodes of unexplored branches of the index tree. *)
	val mutable _nodes = initial_nodes
			       
	(* contains the next element in the iteration. *)
	val mutable _next: 'data option = None
	
	method is_empty =
	  iterator#advance;
	  match _next with
	    | None -> true
	    | Some _ -> false
		
	method next =
	  iterator#advance;
	  match _next with
	    | None ->
		raise ITERATOR_EMPTY

	    | Some data ->
		_next <- None;
		data

	(* is the next element in the iteration already known? *)
	method is_advanced =
	  match _next with
	    | None -> false
	    | Some data -> true
		
   
	method advance =
	  (* advance only, if the next element is currently unknown. *)
	  if iterator#is_advanced then
	    ()

	  else
	    (* search for the next element. *)
	    match _nodes with
	      | [] ->
		  (* tree is completely explored *)
		  _next <- None
		    
	      | node :: tail ->
		  (* check the next branch... *)
		  _nodes <- tail;
		  
		  begin
		    match node with
		      | (Leaf leaf, x) ->
			  (* ... a leaf, so check if it is of interested
			     and can be returned as the next element *)
			  if is_leaf_valid (leaf, x) then
			    _next <- Some leaf.sl_data
			  else
			    iterator#advance
			      
		      | (Node inner_node, _) ->
			  (* ... a node, so add its children for iteration *)
			  _nodes <-
			    List.fold_left
			    (fun acc child_node ->
			       try
				 let new_node =
				   create_child_node node child_node
				 in
				   (* this order corresponds to a depth first search,
				      which is actually wished for to find a first result quickly. *)
				   new_node :: acc;
			       with
				 | Unification.UNIFICATION_FAIL ->
				     acc
			    )
			    _nodes
			    inner_node.sn_children;

			  (* and no continue to search for the next element *)
			  iterator#advance
		  end
		    
		    
	initializer
	  (* advance to the first element on initialization *)
	  iterator#advance		  
      end
    in	
      (iterator :> 'data iterator)

  method get_shielding_iterator (general: term) (instance: term) : 'data iterator =
    let initial_nodes =
      match _root with
	| None ->
	    []
	      
	  | Some node ->
	      try
		let top_node = (
		  node,
		  (
		    self#func_unifiable_root ~p_preserving:false general,
		    self#func_generalization_root ~p_preserving:false instance
		  )
		)
		in
		  [top_node]
	      with
		| Unification.UNIFICATION_FAIL ->
		    []
    in

    let is_leaf_valid node =
      let (leaf, (subst_general, _)) =
	node
      in
	(* the intersection term is instantiated p-preservingly *)
        (Subst.is_p_renaming subst_general tree_offset)
	&&
	(* it's not the producer itself *)
	(not (Term.term_equal leaf.sl_term general))
    in
      
    let create_child_node node child_node =
      let (_, (subst_general, subst_instance)) =
	node
      in
	(
	  child_node,
	  (
	    self#func_unifiable ~p_preserving:false child_node subst_general,
	    self#func_generalization ~p_preserving:false child_node subst_instance
	  )
	)
    in
      self#create_iterator initial_nodes is_leaf_valid create_child_node


  method get_generalization_iterator ~(p_preserving: bool) (term: term) : 'data iterator =
    let initial_nodes =
      match _root with
	| None ->
	    []
	      
	  | Some node ->
	      try
		let top_node = (
		  node,
		  self#func_generalization_root ~p_preserving:p_preserving term
		)
		in
		  [top_node]
	      with
		| Unification.UNIFICATION_FAIL ->
		    []
    in
      
    let is_leaf_valid node =
      true
    in
      
    let create_child_node node child_node =
      let (_, parent_subst) =
	node
      in
	(
	  child_node,
	  self#func_generalization ~p_preserving:p_preserving child_node parent_subst
	)
    in
      self#create_iterator initial_nodes is_leaf_valid create_child_node

  end



class ['data] index
  (__data: 'data data)
   =
object (self)
  inherit ['data] Term_indexing.index
    __data
    (new predicate_index)
end




(*** public interface ***)

let create_predicate_index (data: 'data data) : 'data Term_indexing.predicate_index =
  new predicate_index data


let create_index (data: 'data data) : 'data Term_indexing.index =
  new index data
