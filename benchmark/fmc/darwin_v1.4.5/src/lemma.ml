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


type symbol = Symbol.symbol
type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type sort = Sort_inference.sort
type sorts = Sort_inference.sorts
module LiteralTable = Term.LiteralTable


(* pseudo-context literal to regress,
   used to mark the regression task of the closing clause. *)
let close_literal =
  Term.request_literal
    true
    (Term.request_const Symbol.lemma_root)


(* accepts a list of literal lists,
   where each literal is the class of all regressed instances of the same context literal.

   simplifies by trying to unify and reduce all instances of a class to a singleton *)
let simplify (regressed: literal list list) : literal list =

  let rec simplify (simplified: literal list) (classes: literal list list) =
    match classes with
      | [] ->
	  simplified
	    
      | literals :: tail ->
	  (* don't want to 'simplify' equality constraints,
	     this leads to type unsoundness. *)
	  if
	    match literals with
	      | [] -> failwith "Lemma.simplify: empty class"
	      | literal :: _ ->
		  Term.is_fd_constraint literal
	  then
	    simplify (literals @ simplified) tail

	  else begin
	    try
	      (* try to unify all literals in this class by unifying its literals pairwise *)
	      let unifier =
		let rec unify' unifier literals =
		  match literals with
		    | [] ->
			failwith "Lemma: simplifying empty class"
			
		    | _ :: [] ->
			unifier
			
		    | l1 :: l2 :: tail ->
			let unifier' =
			  Unification.unify_literals_ ~recompute:false unifier l1 0 l2 0
			in
			  unify' unifier' (l2 :: tail)
		in
		  unify' Subst.empty literals
	      in
		(* already a singleton, or only constants *)
		if Subst.is_empty unifier then begin
		  simplify (literals @ simplified) tail
		end
		  
		(* apply unifier to all literals -
		   have to avoid independent normalization *)
		else begin
		  (* only need to use one literal of the unified class *)
		  let simplified' =
		    List.map
		      (fun literal ->
			 Subst.apply_to_literal ~normalize:false unifier literal 0
		      )
		      ((List.hd literals) :: simplified)
		  in
		    (*print_endline ("\nsimplified:");
		    List.iter
		      (fun literal ->
			 print_endline (Term.literal_to_string literal)
		      )
		      simplified;
		    print_endline ("\nsimplified':");
		    List.iter
		      (fun literal ->
			 print_endline (Term.literal_to_string literal)
		      )
		      simplified';*)

		  let classes' =
		    List.map
		      (fun literals ->
			 List.map
			   (fun literal ->
			      Subst.apply_to_literal ~normalize:false unifier literal 0
			   )
			   literals
		      )
		      tail
		  in
		    (*print_endline ("\nclasses:");
		    List.iter
		      (fun literals ->
			 List.iter
			   (fun literal ->
			      print_endline (Term.literal_to_string literal)
			   )
			   literals
		      )
		      classes;
		    print_endline ("\nclasses':");
		    List.iter
		      (fun literals ->
			 List.iter
			   (fun literal ->
			      print_endline (Term.literal_to_string literal)
			   )
			   literals
		      )
		      classes';*)

		    simplify simplified' classes'
		end
		  
	    with
	      | Unification.UNIFICATION_FAIL ->
		  (* simplification of this class failed *)
		  simplify (literals @ simplified) tail
	  end
  in
    
    simplify [] regressed






exception TAUTOLOGY

let remove_falsified_constraints lemma =
  List.find_all
    (fun literal ->
       not (
	 match literal.Term.atom with
	   | Term.Func func when
	       Symbol.equal Symbol.equality func.Term.symbol
	       ||
	       Symbol.equal Symbol.diff func.Term.symbol ->

	       (* x = x or 1 = 1 *)
	       if		 
		 Term.term_equal func.Term.subterms.(0) func.Term.subterms.(1)
		 &&
		 ((Symbol.equal Symbol.equality func.Term.symbol && literal.Term.sign)
		 ||
		 (Symbol.equal Symbol.diff func.Term.symbol && not literal.Term.sign))
	       then begin
(*		 Statistic.inc_global_debug2 ();*)
		 raise TAUTOLOGY;
	       end;

	       (* ground? then it must be falsified, i.e. 1 != 2 *)
	       Term.is_literal_ground literal

	   | Term.Func func when
	       Symbol.is_fd_permutable func.Term.symbol
	       &&
	       Term.is_term_ground literal.Term.atom ->
	       (* permutable restriction to domain element -
		  either permanently satisfied or falsified,
		  but we can't tell. *)
	       raise TAUTOLOGY
		 
	   | _ ->
	       false
       )
    )
    lemma






(*** isomorphism abstraction ***)

module SymbolTable = Symbol.SymbolTable
module VarTable = Var.VarTable
module SortTable = Sort_inference.SortTable
module TermTable = Term.TermTable
type abstracted_elements = term TermTable.t
type abstracted_sorts = abstracted_elements SortTable.t

let find_blocked (sorts: sorts) (clause: clause) : term list =
  List.fold_left
    (fun acc literal ->
       match literal.Term.atom with
	 | Term.Var _
	 | Term.Const _ -> acc
	 | Term.Func func ->
	     if
	       Symbol.equal Symbol.equality func.Term.symbol
	       ||
	       Symbol.equal Symbol.diff func.Term.symbol
	     then begin
	       match func.Term.subterms.(0), func.Term.subterms.(1) with
		 | Term.Var var, (Term.Const _ as element)
		 | (Term.Const _ as element), Term.Var var ->
		     begin
		       match Sort_inference.get_var_sort sorts clause var with
			 | None ->
			     (* found domain element of undetermined sort *)
			     element :: acc

			 | Some _ ->
			     acc
		     end
		 | _ -> acc
	     end

	     else
	       acc
    )
    []
    clause

let is_blocked (blocked: term list) (element: term) =
  List.exists (Term.term_equal element) blocked



let abstract_element (abstracted_sorts: abstracted_sorts) (blocked: term list)
    (sort: sort) (element: term) (var_counter: int ref) : term =

  (* not to be abstracted *)
  if is_blocked blocked element then
    element

  (* find elements of same sort *)
  else
    let abstracted_elements =
      try
	SortTable.find abstracted_sorts sort
      with
	| Not_found ->
	    let abstracted_elements =
	      TermTable.create 32
	    in
	      SortTable.add abstracted_sorts sort abstracted_elements;
	      abstracted_elements
    in
      (* find element *)
      try
	(* reuse previous abstraction *)
	TermTable.find abstracted_elements element
      with
	| Not_found ->
	    (* create new abstraction *)
	    let var =
	      Term.request_var (Var.create_universal (!var_counter))
	    in
	      var_counter := !var_counter + 1;
	      TermTable.add abstracted_elements element var;
	      var



let abstract_atom (sorts: sorts) (abstracted_sorts: abstracted_sorts) (blocked: term list)
    (var_counter: int ref) (clause: clause) (literal: literal) : term =
  match literal.Term.atom with
    | Term.Var _	    
    | Term.Const _ ->
	literal.Term.atom
	  
    | Term.Func func ->
	(* 
	   we can have only positive equalities in a lemma,
	   as the flattened transformed input contains only positive equalities,
	   and isomorphism abstraction also only adds positive equalities.

	   now, we can have:
	   - a ground equality:
	     must be of the form 1 = 2, i.e. permanently falsified,
	     and can be ignored.	  

	   - an equality between a variable and a domain element:
	     need to figure out the sort of the domain element,
	     by figuring out the sort of the variable,
	     by looking at its other occurrences in the clause.
	     
	     if there are no other occurences (transitively over variables),
	     this literal has no influence on the rest of the clause,
	     we don't know the sort of the domain element,
  	     and it is not abstracted.
	   
	     otherwise, the domain element is treated like any
	     domain element to abstract.

	   - an equality between two variables:
	     nothing to extract here, so just keep it *)
	if
	  Symbol.equal Symbol.equality func.Term.symbol
	  ||
	  Symbol.equal Symbol.diff func.Term.symbol
	then begin
	  match func.Term.subterms.(0), func.Term.subterms.(1) with
	    | Term.Const _, Term.Const _ ->
		(* ground, permanently falsified, ignore *)
		if Const.debug && Term.term_equal func.Term.subterms.(0) func.Term.subterms.(1) then
		  failwith ("Lemma.abstract_atom: lemma contains equality: "
			    ^ Term.literal_to_string literal);
		literal.Term.atom	      

	    | Term.Var _, Term.Var _ ->
		(* non-ground, nothing to abstract, ignore *)
		
		literal.Term.atom

	    | (Term.Var var as var_term), (Term.Const _ as element)
	    | (Term.Const _ as element), (Term.Var var as var_term) ->
		(* not to be abstracted *)
		if is_blocked blocked element then begin
		  literal.Term.atom
		end

		(* hm, this whole thing is neither elegant nor efficient,
		   hopefully it doesn't happen to often... *)
		else begin
		  match Sort_inference.get_var_sort sorts clause var with
		    | None ->
			(* if we can't determine the sort, we are in trouble *)
			print_endline (Term.clause_to_string clause);
			print_endline (Term.literal_to_string literal);
			failwith ("Lemma.abstract_atom: unknown sort of domain element");
		    | Some sort ->
			(* abstract domain element *)
			let symbol =
			  Term.top_symbol_term element
			in
			  if Sort_inference.is_permutable sorts sort symbol then
			    let abstracted =
			      abstract_element abstracted_sorts blocked sort element var_counter
			    in
			      Term.request_func ~insert_db:true
				(func.Term.symbol, [| var_term; abstracted |])

			  (* not permutable, so nothing to do *)
			  else
			    literal.Term.atom
		end

	    | _ ->
		failwith ("Lemma.abstract_atom: equality: " ^ Term.literal_to_string literal);
	end
	 
	 (* generalized statement over more than one symbol,
	    so potentially over many sorts.
	    should not be possible in this configuration:
	    Const.fd_isomorphism_abstraction *)
	else if
	   Const.debug
	   &&
	   Symbol.is_fd_relation func.Term.symbol
	   &&
	   (
	     Array.length func.Term.subterms == 0
	     ||
	     Term.is_term_var func.Term.subterms.(0)
	   )
	 then begin
	   failwith ("Lemma.abstract_atom: " ^ Term.literal_to_string literal);
	 end

	(* to figure out the sorts of the domain elements,
	   we need to get the function symbol,
	   and the sorts at its argument positions. *)
	else begin
	  let function_symbol =
	    Sort_inference.get_function_symbol_of_func func
	  in
	    
	  let subterms' =
	    Array.mapi
	      (fun i subterm ->
		 match subterm with
		   | Term.Var _ -> subterm
		   | Term.Func _ -> failwith "Lemma.abstract_atom: not flat."
		   | Term.Const symbol ->
		       (* first argument of relation literal is the function symbol *)
		       if i = 0 && Symbol.is_fd_symbol symbol then
			 subterm

		       (* all other arguments must be domain elements *)
		       else if Const.debug && not (Symbol.is_fd_element symbol) then
			 failwith "Lemma.abstract_atom: not a domain element."

		       else begin
			 let sort =
			   (* function symbol disguised as relation symbol *)
			   if not (Symbol.equal function_symbol func.Term.symbol) then
			     Sort_inference.get_argument_sort sorts function_symbol (i - 1)
		           (* relation symbol *)
			   else
			     Sort_inference.get_argument_sort sorts function_symbol i
			 in
			   if Sort_inference.is_permutable sorts sort symbol then
			     abstract_element abstracted_sorts blocked sort subterm var_counter
			       
			   else
			     subterm
		       end
	      )
	      func.Term.subterms
	  in
	    Term.request_func (func.Term.symbol, subterms')
	end

  
let abstract_permutable (lemma: clause) (sorts: Sort_inference.sorts) : clause =
(*  print_endline ("abstract_permutable: " ^ Term.clause_to_string lemma);*)

  (* find domain elements of unknown sort, these can't be safely abstracted. *)
  let blocked =
    find_blocked sorts lemma
  in

  (* mapping from abstracted domain elements to abstraction variable *)
  let abstracted_sorts =
    SortTable.create 128
  in

  (* to create fresh variables with new ids *)
  let var_counter =
    ref (List.length (Term.vars_of_clause lemma))
  in

  (* abstract all permutable domain elements,
     and put them into isomorphic_abstraction *)
  let lemma' =
    List.map 
      (fun literal ->
	 Term.request_literal
	   literal.Term.sign
	   (abstract_atom sorts abstracted_sorts blocked var_counter lemma literal)
      )
      lemma
  in

  (* add pairwise disequalities between abstracted elements *)
  let distinct =
    let rec create_pairs x tail =
      match tail with
	| [] ->
	    []
		     
	| y :: tail' ->
	    let pair =
	      Term.request_literal
		true
		(Term.request_func (Symbol.equality, [| x; y |]))
	    in
	    let pair =
	      Finite_domain.to_diff_literal pair
	    in
	      (create_pairs y tail') @ (pair :: create_pairs x tail')
    in
    SortTable.fold
      (fun sort abstracted_elements acc ->
	 (* need no axiom if all elements are permutable *)
	 if Sort_inference.is_permutable_ sorts sort 1 then
	   acc

	 else begin
	   let elements =
	     TermTable.fold
	       (fun _ var acc ->
		  var :: acc
	       )
	       abstracted_elements
	       []
	   in
	   let distinct =
	     match elements with
	       | h :: t ->
		   create_pairs h t
	       | _ ->
		   []
	   in
	     distinct @ acc
	 end
      )
      abstracted_sorts
      []
  in

  (* add domain restriction axioms *)
  let domain_restriction =
    SortTable.fold
      (fun sort abstracted_elements acc ->
	 (* need no axiom if all elements are permutable *)
	 if Sort_inference.is_permutable_ sorts sort 1 then
	   acc

	 else
	   TermTable.fold
	     (fun _ var acc ->
		Term.request_literal
		  false
		  (Term.request_func (Sort_inference.get_permutable sort, [| var |]))
		::
		acc
	     )
	     abstracted_elements
	     acc
      )
      abstracted_sorts
      []
  in

  (* keep old lemma if nothing was abstracted *)
  let abstracted =
    match distinct, domain_restriction with
      | [], [] -> lemma
      | _ -> lemma' @ distinct @ domain_restriction
  in
    if lemma != abstracted then begin
(*      Statistic.inc_global_debug ();*)
      (*if List.length blocked != 0 then
	Statistic.inc_global_debug2 ();*)
      (*
      print_endline ("Blocked: " ^ String.concat " , " (List.map Term.term_to_string blocked));
      print_endline ("Abstracted: ");
      print_endline (Term.clause_to_string lemma);
      print_endline (Term.clause_to_string abstracted);
      print_newline ();*)
    end;
    abstracted
