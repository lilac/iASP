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


(*
  flattening:

  1) break equalities
  
    orient:
    C \/    (x = t)
    ->
    C \/    (t = x)

    one side must be variable:
    C \/ (-)(t = s), t and s not variables
    ->
    C \/ (-)(t = y) \/ -(s = y)

    for pos. equality both sides must be variables:
    C \/    (t = x)
    ->
    C \/    (y = x) \/ -(t = y)

  2) flatten

    C \/ (-)p(..., f(t1, ..., tn), ...)
    ->
    C \/ (-)p(..., y, ...) \/ -(f(t1, ..., tn) = y)

  3) replace functions by relations

    C \/ (-)(f/n(t1, ..., tn) = y)
    ->
    C \/ (-) R_n(f, t1, ..., tn, y)

*)


type bound = Bound.bound
type symbol = Symbol.symbol
type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type problem = Problem.problem
type arities = Problem.arities
type sorts = Sort_inference.sorts


class counter_data =
object
  method is_equal (x0, x1: term * int) (y0, y1: term * int) =
    Term.term_equal x0 y0
    &&
    x1 == y1

  method to_string (x, y) =
    Term.term_to_string x ^ " : " ^ string_of_int y
end

(* for index over term_definitions *)
class definition_data =
object
  method is_equal (x0, x1) (y0, y1) =
    Term.term_equal x0 y0
    &&
    Term.term_equal x1 y1

  method to_string (x, y) =
    Term.term_to_string x ^ " : " ^ Term.term_to_string y
end


(* see Const.fd_use_term_definitions *)
type term_definitions = {
  (* definition index: abstraction -> abstracted term

     e.g if p(f(g(x), y)) is transformed into
     p(s0(x))
     and
     s0(x, y) = f(g(x), y),
     then s0(x, y) -> f(g(x), y) is entered into the index.
  *)
  definitions : (term * term) Term_indexing.index;

  (* new definition clauses generated while flattening clauses.
     need to be flattened as well.
  *)
  mutable new_definitions: clause list;

  (* arities of the created abstraction skolem functions.
     if initialized with the original problems function arities,
     this contains all function arities of the transformed problem.
  *)
  mutable function_arities: arities;

  (* new abstraction skolem constants,
     mapped to the top symbol of the abstracted term. *)
  mutable constants: (symbol * symbol) list;
}


type finite_domain = {
  mutable bound: bound option;

  (* the original problem. *)
  original: problem;

  (* the flattened problem. *)
  flattened: problem;

  (* the inferred sorts of the original problem. *)
  sorts: sorts;

  (* the term definitions used when flattening the original problem *)
  term_definitions: term_definitions;
}









(*** flattening: equality ***)


let create_equality (left: term) (right: term) : term =
  Term.request_func (Symbol.equality, [| left; right |])

let create_pos_equality (left: term) (right: term) : literal =
  Term.request_literal
    true
    (create_equality left right)

let create_neg_equality (left: term) (right: term) : literal =
  Term.request_literal
    false
    (create_equality left right)


(* replace equality by -diff.
   as except for the equality axioms
   all clauses contain only positive equalities,
   this has the effect that -v unifies less often,
   and there are less context unifiers in most cases. *)   
let to_diff_literal literal =
  if not Const.fd_use_diff then
    literal

  else
  match literal.Term.atom with
    | Term.Func func when Symbol.equal Symbol.equality func.Term.symbol ->
	Term.request_literal
	  (not literal.Term.sign)
	  (Term.request_func (Symbol.diff, func.Term.subterms ))

    | _ ->
	literal

let to_diff_clause clause =
  List.map to_diff_literal clause



(*** flattening: term definitions ***)


let is_flat term =
  match term with
    | Term.Func func ->
	Tools.array_for_all
	  (fun term' ->
	    match term' with
	      | Term.Var _ -> true
	      | _ -> false
	  )
	  func.Term.subterms

    | _ ->
	true


(* term definitions for flattening of deep terms.

   for example, p(f(g(x), y))
   will be transformed into the two clauses
   p(s0(x)) and the term definition s0(x, y) = f(g(x), y),
   where s0 is a fresh skolem symbol.

   The definition term s0(x, y) is reused whenever
   an instance of f(g(x), y) has to be flattened.
*)




(* scan all literals for deep terms -
   based on tests we just take ground terms *)
let scan_clauses (clauses: clause list) : term Term_indexing.index =
  let occurrences =
    Discrimination_tree.create_term_index false
  in

  let rec scan_term term =
    (* scan all subterms *)
    match term with
      | Term.Func func ->
	  (* register term *)
	  if Term.is_term_ground term then begin
	    let index =
	      occurrences#find (Term.request_literal ~insert_db:false true term)
	    in
	    let definition =
	      index#find_generalization ~p_preserving:true term
	    in
	      match definition with
		| None ->
		    (* register term - replace all subsumed terms *)
		    let instances =
		      index#find_all_instances ~p_preserving:true term
		    in
		      List.iter
			(fun term ->
			  if not (index#remove term (Some term)) then
			    failwith "Finite_domain.find_term_definitions 1";
			)
			instances;
		      index#add term term
			    
		| Some _ ->
		    (* already registered *)
		    ()
	    end;
	  
	  Array.iter scan_term func.Term.subterms
	    
      | _ ->
	  ()
  in
    List.iter
      (fun clause ->
	List.iter
	  (fun literal ->
	    match literal.Term.atom with
	      | Term.Func func ->
		  Array.iter scan_term func.Term.subterms
		    
	      | _ ->
		  ()
	  )
	  clause
      )
      clauses;

    occurrences



(* scan clauses for deep not-flat terms,
   and decide which of those should be turned into term definitions *)
let create_term_definitions ~(print: bool) (problem: problem) : term_definitions =
  let term_definitions =
    {
	definitions = Discrimination_tree.create_index false (new definition_data);
	new_definitions = [];
	constants = [];
	function_arities = problem#getFunctionArities;
    }
  in

  if not Const.fd_use_term_definitions then begin
    term_definitions
  end

  else begin
    let occurrences =
      scan_clauses problem#getClauses
    in

    (* find the good terms to create definitions for -
       all heuristics failed, so just take all. *)
      occurrences#iter
	(fun _ index ->
	  index#iter
	    (fun _ term ->
	      (* create definition *)
	      let normalized =
		Subst.normalize_term term
	      in
	      let vars =
		Term.vars_of_term normalized
	      in
	      let number_of_vars =
		List.length vars
	      in
	      let symbol =
		Symbol.create_skolem ~arity:number_of_vars ()
	      in
	      let definition =
		Term.create_schema_term symbol
	      in
		
	      (* create definition clause *)
	      let var0, var1 =
		Term.request_var (Var.create_universal number_of_vars),
		Term.request_var (Var.create_universal (number_of_vars + 1))
	      in
	      let head =
		create_pos_equality var0 var1
	      and left =
		create_neg_equality definition var0
	      and right =
		create_neg_equality normalized var1
	      in
	      let definition_clause =
		head :: left :: right :: []
	      in

	      (* register new term definition *)
	      let index =
		term_definitions.definitions#find (Term.request_literal ~insert_db:false true term)
	      in
		index#add normalized (normalized, definition);
		term_definitions.new_definitions <- definition_clause :: term_definitions.new_definitions;

		(* update symbols for axioms *)
		if number_of_vars > 0 then begin
		  term_definitions.function_arities <- Problem.update_arities term_definitions.function_arities symbol;
		end
		else begin
		  match term with
		    | Term.Func func ->
			term_definitions.constants <- (symbol, func.Term.symbol) :: term_definitions.constants;
		    | _ ->
			failwith "Finite_domain.get_term_definition"
		end;

		if print then begin
		    print_endline ("Term-Def: " ^ Term.clause_to_string definition_clause);
		end;
	    )
	);

      term_definitions
  end      


(* returns a term definition for a term, if it exists *)
let rec get_term_definition ~(print: bool) (term: term) (definitions: term_definitions) : term =
  let index =
    definitions.definitions#find (Term.request_literal ~insert_db:false true term)
  in
  let definition =
    index#find_generalization ~p_preserving:true term
  in
    match definition with
      | Some (term', definition') ->
	  (* found an at least as general term,
	     so get its definition and replace variables accordingly *)
	  let subst =
	    Unification.match_terms ~recompute:true
	      term' 0
	      term 1
	  in
	    Subst.apply_to_term ~normalize:false subst definition' 0;

      | None ->
	  term







(*** flattening: abstractions ***)



(*
  abstractions:
  mappings from a flattened term to the abstraction variable.
  E.g. p(f(a)) is flattened to p(x), -(f(a) = x),
  where f(a) --> x  is a new abstraction.

  the intent is to use the same abstraction
  for each occurence of the same term.

  already existing disequalities are used abstractions.
*)


(* retrieves an existing abstraction variable of term,
   i.e. a disequality -(term = abstraction_var),
   or creates an abstraction and returns it *)
let get_abstraction term abstractions fresh_var_id =
  try
    let (_, abstraction_var) =
      List.find
	(fun (term', _) -> Term.term_equal term term')
	abstractions
    in
      abstraction_var, abstractions, [], fresh_var_id
  with
    | Not_found ->
	let abstraction_var =
	  Term.request_var (Var.create_universal fresh_var_id)
	in
	let abstraction =
	  create_neg_equality term abstraction_var
	in
	  abstraction_var,
	  ((term, abstraction_var) :: abstractions),
	  [abstraction],
	  (fresh_var_id + 1)

let is_abstracted term abstractions =
  List.exists
    (fun (term', _) -> Term.term_equal term term')
    abstractions



(* tries first to introduce a term definition for a deep term,
   and then flattens the shallower term. *)
let abstract ~(print: bool) term abstractions definitions fresh_var_id =
  let term' =
(*    if
      not (is_flat term)
      (*
	Term_attributes.depth_of_term term > 1
      ||
      (Term_attributes.depth_of_term term > 0 && Term.is_term_ground term) *)
    then*)
      get_term_definition ~print:print term definitions
(*    else
      term*)
  in
  let abstraction_var', abstractions', fresh_literals, fresh_var_id' =
    get_abstraction term' abstractions fresh_var_id
  in
    abstraction_var', abstractions', fresh_literals, fresh_var_id'


(* requires:
     input clause is normalized,
     fresh_var_id and higher numbers can be used for fresh variable ids.
   ensures:
     contains only positive equalities between variables
     contains only negative equalities where the left side is a variable: x != t

   abstractions:
     if the clause contains a disequality -(x = t),
     then the abstraction (t, x) will be use whenever t has to be abstracted.
*)
let rec break_equalities' ~(print: bool) (broken: clause) (to_break: clause)
    (abstractions: (term * term) list) (definitions: term_definitions) (fresh_var_id: int)
    : clause * (term * term) list * int =
  match to_break with
    | [] ->
	broken, abstractions, fresh_var_id
	  
    | literal :: tail ->
	begin
	  match literal.Term.atom with
	    | Term.Func func when
		Symbol.equal Symbol.equality func.Term.symbol ->
		(* equality literal, break it *)
		begin
		  match func.Term.subterms.(0), func.Term.subterms.(1) with
		    | Term.Var _, Term.Var _ when
			literal.Term.sign ->
			(* x = y is fine, keep it *)
			break_equalities' ~print:print (literal :: broken) tail abstractions definitions fresh_var_id

		    | (Term.Var _ as var_term), term
		    | term, (Term.Var _ as var_term) ->
			(* orient disequality of a variable and a non-variable *)
			if not literal.Term.sign then begin
			  let oriented =
			    Term.request_literal
			      literal.Term.sign
			      (Term.request_func (func.Term.symbol, [| term; var_term |]))
			  in
			    break_equalities' ~print:print (oriented :: broken) tail
			      ((term, var_term) :: abstractions) definitions fresh_var_id
			end

			(* break an equality of a variable and a non-variable *)
			(*
			  C \/ (x = s) -> C \/ (t = y) \/ -(s = y)
			*)
			else begin
			  let abstraction_var, abstractions', fresh_literals', fresh_var_id' =
			    abstract ~print:print term abstractions definitions fresh_var_id
			  in
			  let literal' =
			    create_pos_equality var_term abstraction_var
			  in
			    break_equalities' ~print:print (literal' :: broken) (fresh_literals' @ tail)
			      abstractions' definitions fresh_var_id'
			end

		    | term1, term2 ->
			(* break a (dis)equality between terms *)
			(*
			  C \/ (-)(t = s), t and s not variables
			  ->
			  C \/ (-)(t = y) \/ -(s = y)
			*)

			(* if one subterm corresponds to an existing abstraction,
			   prefer to abstract it. *)
			let replace, keep =
			  if is_abstracted term2 abstractions then
			    term2, term1
			  else
			    term1, term2
			in
			let abstraction_var, abstractions', fresh_literals', fresh_var_id' =
			  abstract ~print:print replace abstractions definitions fresh_var_id
			in
			let literal' =
			  Term.request_literal
			    literal.Term.sign
			    (create_equality keep abstraction_var)
			in
			  (* literal' might still be of the from (x = a), so recheck it *)
			  break_equalities' ~print:print broken (literal' :: fresh_literals' @ tail)
			    abstractions' definitions fresh_var_id'
		end
		
	    | _ ->
		(* not an equality literal, just keep it *)
		break_equalities' ~print:print (literal :: broken) tail abstractions definitions fresh_var_id
	end


(*
  1) break equalities
  
    orient:
    C \/    (x = t)
    ->
    C \/    (t = x)

    one side must be variable:
    C \/ (-)(t = s), t and s not variables
    ->
    C \/ (-)(t = y) \/ -(s = y)

    for pos. equality both sides must be variables:
    C \/    (t = x)
    ->
    C \/    (y = x) \/ -(t = y)
 *)
let break_equalities ~(print: bool) (clause: clause) (definitions: term_definitions) (fresh_var_id: int) 
    : clause * (term * term) list * int =
  break_equalities' ~print:print [] clause [] definitions fresh_var_id








(*** flattening: flattening ***)


(*
  assumes break_equalities has been run first,
  abstractions contains all disequalities of the clause

  3) flatten

    C \/ (-)p(..., f(t1, ..., tn), ...)
    ->
    C \/ (-)p(..., y, ...) \/ -(f(t1, ..., tn) = y)

  a term is flat if:
  - it is a constant: a
  - all its subterms are variables: p(x, y)
  - it is an equality where both subterms are variables
  - it is a disequality where one subterm is flat and the other a variable
*)


(* returns the flattened subterms.
   all created abstractions are added to to_flatten and abstractions.
*)
let rec flatten_at  ~(print: bool) (i : int) symbol subterms to_flatten abstractions definitions fresh_var_id :
    (* subterms * to_flatten * abstractions * fresh_var_id *)
    term array * literal list * (term * term) list * int =

  if i >= Array.length subterms then begin
    subterms, to_flatten, abstractions, fresh_var_id
  end

  else begin
    let term =
      subterms.(i)
    in
      (* for disequalities the left argument must be a flat term,
	 not a variable *)
      if
	Symbol.equal Symbol.equality symbol
	&&
	i = 0
      then begin
	match term with
	  | Term.Var _
	  | Term.Const _ ->
	      (* already flat:
		 if a var, then this must be a positive equality x = y,
		 if a constant, then this is a disequality x != y.
	      *)
	      flatten_at ~print:print (i + 1) symbol subterms to_flatten abstractions definitions fresh_var_id

	  | Term.Func func' ->
	      (* flatten subterm *)
	      let subterms', to_flatten', abstractions', fresh_var_id' =
		flatten_at ~print:print 0 func'.Term.symbol func'.Term.subterms to_flatten abstractions definitions fresh_var_id
	      in
	      let subterms'' =
		Array.copy subterms
	      in
		subterms''.(i) <- Term.request_func (func'.Term.symbol, subterms');
		flatten_at ~print:print (i + 1) symbol subterms'' to_flatten' abstractions' definitions fresh_var_id'
      end

      else begin
	match term with
	  | Term.Var _ ->
	      (* already flat *)
	      flatten_at ~print:print (i + 1) symbol subterms to_flatten abstractions definitions fresh_var_id

	  | Term.Const _ 
	  | Term.Func _ ->
	      (* abstract term *)
	      let abstraction_var, abstractions', fresh_literals', fresh_var_id' =
		abstract ~print:print term abstractions definitions fresh_var_id
	      in
	      let subterms' =
		Array.copy subterms
	      in
		subterms'.(i) <- abstraction_var;
		flatten_at ~print:print (i + 1) symbol subterms' (fresh_literals' @ to_flatten)
		  abstractions' definitions fresh_var_id'
	end
    end


let rec flatten' ~(print: bool) (flattened: clause) (to_flatten: clause)
    (abstractions: (term * term) list) (definitions: term_definitions) (fresh_var_id: int) :
    clause * int =
  match to_flatten with
    | [] ->
	flattened, fresh_var_id

    | literal :: tail ->
	begin
	  match literal.Term.atom with
	    | Term.Func func ->
		let term', to_flatten', abstractions', fresh_var_id' =
		  flatten_at ~print:print 0 func.Term.symbol func.Term.subterms tail abstractions definitions fresh_var_id
		in
		let flat =
		  Term.request_literal literal.Term.sign (Term.request_func (func.Term.symbol, term'))
		in
		  flatten' ~print:print (flat :: flattened) to_flatten' abstractions' definitions fresh_var_id'

	    | Term.Var _
	    | Term.Const _ ->
		(* literal already flat : a or x *)
		flatten' ~print:print (literal :: flattened) tail abstractions definitions fresh_var_id
	end


let flatten_clause ~(print:bool) (clause: clause) (abstractions: (term * term) list)
    (definitions: term_definitions) (fresh_var_id: int) : clause * int =
  flatten' ~print:print [] clause abstractions definitions fresh_var_id




(*** flattening: relation ***)

(*
  3) replace functions (in equalities) by relations

    C \/ (-)(f/n(t1, ..., tn) = y)
    ->
    C \/ (-) R_n(f, t1, ..., tn, y)
*)

(* create a relation literal *)
let create_relation (sign: bool) (symbol: symbol) (subterms: term array) (result: term) : literal =
  let arity =
    Symbol.arity symbol
  in
  let relation_symbol =
    Symbol.get_fd_relation (arity + 2)
  in
  let function_symbol =
    Symbol.create_fd_symbol symbol
  in
  let relation_subterms =
    Array.make (arity + 2) result
  in
    relation_subterms.(0) <- Term.request_const function_symbol;
    Array.blit subterms 0 relation_subterms 1 arity;

  let relation_term =
    Term.request_func (relation_symbol, relation_subterms)
  in
  let relation_literal =
    Term.request_literal sign relation_term
  in
    relation_literal


(* replace each disequality by a relation *)
let rec make_relational (clause: clause) : clause =
  match clause with
    | [] ->
	[]

    | literal :: tail ->
	begin
	  match literal.Term.atom with
	    | Term.Func func when
		not literal.Term.sign
		&&
		Symbol.equal Symbol.equality func.Term.symbol
		->
		(* a disequality to make relational *)
		begin
		  match func.Term.subterms.(0) with
		    | Term.Var _ ->
			failwith ("Finite_domain.make_relational: " ^ Term.literal_to_string literal);

		    | Term.Const symbol ->
			let relation_literal =
			  create_relation literal.Term.sign symbol [| |] func.Term.subterms.(1)
			in
			  relation_literal :: make_relational tail
			    
		    | Term.Func func' ->
			let relation_literal =
			  create_relation literal.Term.sign func'.Term.symbol func'.Term.subterms func.Term.subterms.(1)
			in
			  relation_literal :: make_relational tail
		end


	    | _ ->
		(* already relational *)
		literal :: make_relational tail
	end




(*** flattening: definitions ***)

(*
  * requires: the clause is flattened into relational form

  * does: if a clause
    - contains only negative literals
    - expect for one positive equality
    then convert it into a definition of this equality

  * pattern:
  false :- (X = Y) \/ -rel/n(..., X) \/ C

  becomes
  rel/n(..., Y) :- C

  where C contains no positive literals.
*)

(* need exactly one positive equality literal *)
let rec find_pos_equality clause diff =
  match clause with
    | [] ->
	diff
	  
    | literal :: tail ->
	begin
	  match literal.Term.atom with
	    | Term.Func func when
		literal.Term.sign
		&&
		Symbol.equal Symbol.equality func.Term.symbol ->
		(* diff literal found *)
		begin
		  match diff with
		    | None ->
			(* first equality literal found *)
			find_pos_equality tail (Some literal)
			  
		    | Some _ ->
			(* second equality literal found *)
			None
		end
		  
	    | _ ->
		(* no new equality literal found *)
		find_pos_equality tail diff
	end	  


(* the 'inlined' variable might not occur anywhere else in the clause,
   otherwise it would have to be added to the premises as well:
   domain(X) = codomain(domain(X))
   |
   v
   domain(X) = codomain(Y) :- domain(X) = Y
   |
   v
   domain(X) = Z           :- domain(X) = Y, codomain(Y) = Z
   |
   v
   Y = Z                   :- domain(X) = Y, codomain(Y) = Z
   |
   v
   domain(X) = Z           :- codomain(domain(X)) = Z
   |
   v (back to)
   domain(X) = Z           :- domain(X) = Y, codomain(Y) = Z
   or
   domain(X) = Z           :- domain(X) = Z, codomain(Z) = Z
   
*)

(* find a functional relation in clause, whose result value is equal to var *)
let rec inline_pos_equality _clause clause equality var rep =
  match clause with
    | [] ->
	None
	  
    | literal :: tail ->
	begin
	  match literal.Term.atom with
	    | Term.Func func when
		  Symbol.is_fd_relation func.Term.symbol
		  &&
		  Term.term_equal func.Term.subterms.(Array.length func.Term.subterms - 1) var
		  ->
		(* result of this relation equal to var *)

		(* the 'inlined' variable might not occur anywhere else in the clause *)
		if
		  List.exists
		    (fun literal' ->
		      not (Term.literal_equal literal' equality)
		      &&
		      not (Term.literal_equal literal' literal)
		      &&
		      Term.term_contains_term literal'.Term.atom var
		    )
		    _clause
		then
		  None

		(* merge diff and relation literal *)
		else begin
		  let subterms' =
		    Array.copy func.Term.subterms
		  in
		    subterms'.(Array.length func.Term.subterms - 1) <- rep;
		  let literal' =
		    Term.request_literal
		      true
		      (Term.request_func (func.Term.symbol, subterms'))
		  in

		  (* replace original clause by clause where equality has been inlined *)
		  let clause' =
		    List.find_all
		      (fun literal' ->
			not (Term.literal_equal equality literal')
			&&
			not (Term.literal_equal literal literal')
		      )
		      _clause
		  in
		    Some (Subst.normalize_clause (Term.sort_clause (literal' :: clause')))
		end

	    | _ ->
		inline_pos_equality _clause tail equality var rep
	end


let create_definitions (clause: clause) : clause list =
  (* no positive literal allowed *)
  if List.exists (fun literal -> literal.Term.sign) clause then
    []

  else begin
    (* find exactly one positive equality *)
    match find_pos_equality clause None with
      | None ->
	  (* none contained in the clause, or more than one *)
	  []
	    
      | Some equality ->
	  (* find a relation having one of the equality literal's variables
	     as its result. *)
	  begin
	    match equality.Term.atom with
	      | Term.Func func ->
		  let left_def =
		    inline_pos_equality clause clause equality func.Term.subterms.(0) func.Term.subterms.(1)
		  and right_def =
		    inline_pos_equality clause clause equality func.Term.subterms.(1) func.Term.subterms.(0)
		  in
		    begin
		      match left_def, right_def with
			| None, None ->
			    []
			| None, Some definition
			| Some definition, None ->
			    definition :: []
			| Some definition1, Some definition2 ->
			    definition1 :: definition2 :: []
		    end

	      | _ ->
		  failwith "Finite_model.create_definitions"
	  end
  end









(* flattens a clause set *)
let rec flatten ~(print: bool) (clauses: clause list) (term_definitions: term_definitions) : clause list =
  match clauses with
    | [] ->
	[]
	  
    | clause :: tail ->
	(* flattening requires fresh variables,
	   so we start fresh variable id's after the hightest existing variable id.
	   as clauses are normalized this is identical to the number of variables. *)
       let fresh_var_id =
	 List.length (Term.vars_of_clause clause)
       in
       let clause_equalities_broken, abstractions, fresh_var_id =
	 break_equalities ~print:print clause term_definitions fresh_var_id
       in
       let flattened, _ =
	 flatten_clause ~print:print clause_equalities_broken abstractions term_definitions fresh_var_id
       in
       let relational =
	 make_relational flattened
       in
       let definitions =
	 if Const.fd_use_definitions then
	   create_definitions relational
	 else
	   []
       in
       	
	 if print then begin
	   print_endline ("Clause  : " ^ Term.clause_to_string clause);
	   print_endline ("EQ-Break: " ^ Term.clause_to_string clause_equalities_broken);
	   print_endline ("Flat    : " ^ Term.clause_to_string flattened);
	   print_endline ("Relation: " ^ Term.clause_to_string relational);
	   List.iter
	     (fun definition ->
	       print_endline ("Definite: " ^ Term.clause_to_string definition);
	     )
	     definitions;
	   print_newline ();
	end;

	let relational_clause =
	  Subst.normalize_clause (Term.sort_clause relational)
	in
	let flattened_tail =
	  flatten ~print:print tail term_definitions
	in
	  Tools.lists_merge
	    Term.clause_equal
	    (relational_clause :: flattened_tail)
	    definitions



(*** interface ***)

let create ~(print_transformation: bool) ~(print_sorts: bool) (problem: problem) (bound: bound option) (una: bool)
    : finite_domain =
  (* any preprocessing simplifications applied to the flattened clauses
     didn't do any good, so we just keep them as they are. *)
  if print_transformation then begin
    print_endline ("Finite domain flattening:");
    print_newline ();
  end;

  (* term definitions *)
  let definitions =
    create_term_definitions ~print:print_transformation problem
  in
  (* flatten *)
  let flattened =
    flatten ~print:print_transformation (definitions.new_definitions @ problem#getClauses) definitions
  in
  (* non-ground split - ground splitting didn't help in tests *)
    (* but we don't really want to have any non-ground splitting here either,
       because then we need to figure out the sorts of the new connection predicates
       (at least for Const.fd_isomorphism_abstraction)
       
       so, just don't do any splitting
    *)
  let flattened =
(*    Preprocessing_split_nonground.split ~print:true flattened*)
    let ground =
      Preprocessing_split_ground.split
	~print:false
	flattened
    in
      Preprocessing_split_nonground.split
	~print:false
	ground
  in
  (* replace = by -diff to improve performance (less context unifiers with -v) *)
  let flattened =
    List.map to_diff_clause flattened
  in
  let flattened_problem =
    Problem.create ~equality:problem#containsEquality ~horn:false flattened [] []
  in

  (* do sort inference *)
(*    print_endline ("Original:");
    List.iter
      (fun clause -> print_endline (Term.clause_to_string clause))
      problem#getClauses;
    print_newline ();
    print_endline ("Unrelational:");
    List.iter
      (fun clause -> print_endline (Term.clause_to_string clause))
      (List.map make_non_relational_clause flattened_problem#getClauses);*)
  let sorts =
(*    Sort_inference.infer ~print:false problem#getClauses*)
    Sort_inference.infer ~print:false flattened_problem#getClauses
(*    Sort_inference.infer ~print:false
      (List.map make_non_relational_clause flattened_problem#getClauses)*)
  in
    Sort_inference.compute_clique sorts una problem#getClauses;
(*
    (* add the skolem constants generated by the term definitions *)
    List.iter
      (fun (definition_symbol, term_symbol) ->
	Sort_inference.add_constant sorts definition_symbol term_symbol
      )
      definitions.constants;
*)
    if print_sorts then
      Sort_inference.print sorts;


    {
      bound = bound;
      original = problem;
      flattened = flattened_problem;
      sorts = sorts;
      term_definitions = definitions;
    }


let set_bound finite_domain bound =
  finite_domain.bound <- Some bound


let get_problem (finite_domain: finite_domain) : problem =
  finite_domain.original

let get_flattened (finite_domain: finite_domain) : problem =
  finite_domain.flattened

let get_sorts (finite_domain: finite_domain) : sorts =
  finite_domain.sorts





(*** special term creators ***)


(* domain size marker for this domain size *)
let get_domain_size_marker (domain_size: int) : term =
  if domain_size <= 0 then
    failwith "get_domain_size_marker";

  Term.request_const (Symbol.get_fd_size_marker domain_size)


(* get the i.th domain element *)
let get_domain_element (number: int) : term =
  if number <= 0 then
    failwith "Finite_domain.get_domain_element";

  let symbol =
    Symbol.get_fd_element number
  in
    Term.request_const symbol


(* get the i.th domain element *)
let get_domain_elements (domain_size: int) : term list =

  let rec get_domain_elements (current_domain_size: int) : term list =
    if current_domain_size > domain_size then
      []
    else
      (get_domain_element current_domain_size)
      ::
      (get_domain_elements (current_domain_size + 1))
  in
    if domain_size <= 0 then
      failwith "Finite_domain.get_domain_elements"
    else
      get_domain_elements 1


let get_id_of_domain_element element =
  match element with
    | Term.Const symbol when Symbol.is_fd_element symbol ->
	int_of_string (Symbol.to_string ~pretty:true symbol)

    | _ ->
	print_endline (Term.term_to_string element);
	failwith "Finite_domain.get_id_of_domain_element"



(*** axioms ***)




let get_domain_size_axioms ~(print: bool) ~(print_tptp: bool) (domain_size: int) : clause list =
  let domain_size_marker =
    Term.request_literal false (get_domain_size_marker domain_size)
  in
  let axiom =
    [domain_size_marker]
  in
    if print then begin
      print_endline ("Domain size axioms:");
      print_endline (Term.clause_to_string axiom);
      print_newline ();
    end;
    if print_tptp then begin
      print_endline (
	  Term.tptp_clause_to_tptp_string
	    ("domain_size")
	    axiom
	  )
    end;
    [axiom]



(* for constants and unary function symbols. *)
let get_symmetry_reduction_axioms (sorts: sorts) (domain_size: int) : clause list =
  let domain_size_marker =
    Term.request_literal true (get_domain_size_marker domain_size)
  in


  (* get the axioms for a unary function *)
  let rec get_symmetry_reduction_axioms_unary_function
      (unary_function: symbol) (number_of_constants: int) (i : int) : clause list =
    if i > domain_size then
      []

    else begin
      let axiom =
	get_symmetry_reduction_axiom_unary_function unary_function (i - number_of_constants) i 1
      in
	axiom :: get_symmetry_reduction_axioms_unary_function unary_function number_of_constants (i + 1)
    end

  (* get the axiom for this unary function.
     its range starts after the symbols of the same sort. *)
  and get_symmetry_reduction_axiom_unary_function
      (unary_function: symbol) (function_argument: int) (symbol_domain_size: int) (i: int) : clause =
    (* domain range of this element exhausted. *)
    if i > symbol_domain_size then
      []
	
    (* add the current domain element *)
    else begin
      let literal =
	create_relation true unary_function [| (get_domain_element function_argument) |] (get_domain_element i)
      in
	literal :: get_symmetry_reduction_axiom_unary_function unary_function function_argument symbol_domain_size (i + 1)
    end
  in



  (* get the axioms for these constants *)
  let rec get_symmetry_reduction_axioms' (symbols: symbol list) (unary_function: symbol option) (i : int) clique_size : clause list =
    match symbols with
      | [] ->
	  if
	    Const.fd_static_symmetry_reduction_unary
	    &&
	    i <= domain_size
	  then begin
	    match unary_function with
	      | None ->
		  []

	      | Some unary_function ->
		  if i > 1 then
		    get_symmetry_reduction_axioms_unary_function unary_function (i - 1) i
		  else
		    (* no constant at all,
		       but can't put f('1) = '1, this is unsound,
		       have to start with f('2) = '1 \/ f('2) = '2 *)
		    get_symmetry_reduction_axioms_unary_function unary_function 0 2
	  end
	  else
	    []
	    
      | symbol :: tail ->
	  let axiom =
	    if i <= clique_size then
	      (* fix constant to this domain element *)
	      get_symmetry_reduction_axiom symbol i i
	    else
	      (* let constant range over this or smaller domain elements *)
	      get_symmetry_reduction_axiom symbol i 1
	  in
	    axiom :: get_symmetry_reduction_axioms' tail unary_function (i + 1) clique_size

  (* get the axiom for this constant. it ranges over domain 1 .. symbol_domain_size  *)
  and get_symmetry_reduction_axiom (symbol: symbol) (symbol_domain_size: int) (i: int) : clause =
    (* domain range of this element exhausted. *)
    if i > symbol_domain_size then
      []
	
    (* complete domain range exhausted, so add more axiom *)
    else if i > domain_size then
      domain_size_marker :: []
	
    (* add the current domain element *)
    else begin
      let literal =
	create_relation true symbol [| |] (get_domain_element i)
      in
	literal :: get_symmetry_reduction_axiom symbol symbol_domain_size (i + 1)
    end
  in

  let axioms =
    List.fold_left
      (fun acc sort ->
	(* get the axioms for the symbols of each sort *)
	let constant_symbols =
	  Sort_inference.get_constants_for_sort sorts sort
	in
	let unary_function =
	  Sort_inference.get_unary_function_for_sort sorts sort
	in
	let clique_size =
	  Sort_inference.get_max_clique_for_sort sorts sort
	in
	  get_symmetry_reduction_axioms' constant_symbols unary_function 1 clique_size @ acc
      )
      []
      (List.rev (Sort_inference.get_sorts sorts))
  in
    axioms



(* returns totality and symmetry reduction axioms. *)
let get_totality_axioms ~(print: bool) ~(print_tptp: bool) (finite_domain: finite_domain) (domain_size: int) : clause list =
  let domain_size_marker =
    Term.request_literal true (get_domain_size_marker domain_size)
  in

  (* get totality axioms for all arities *)
  let rec get_totality_axioms' (arities': arities) : clause list =
    match arities' with
      | [] ->
	  []

      | (arity, symbols) :: tail ->
	  (* generalize over all function symbols or instantiate for each? *)
	  let axioms =
	    (* instantiate if ... *)
	    if
	      (* we can't generalize if we try the isomorphism abstraction,
		 as then we would generalize over different sorts.
		 this makes abstraction harder,
		 as then during abstraction the axiom would need to be instantiated
		 by sort.
		 while this is possible, not generalizing in the first place
		 is just simpler.
		 also, generalization didn't give too much of a performance
		 win in the first place. *)
	      Const.fd_isomorphism_abstraction
	      ||
	      (* explicit request for instantiation *)
	      Const.fd_instantiate_axioms
	      ||
	      (* if there is only one function symbol, then generalization
		 doesn't make any sense, and might only add some overhead. *)
	      (match symbols with | _ :: [] -> true | _ -> false)
	    then
	      List.map
		(fun symbol -> get_totality_axiom arity (Some symbol) 1)
		symbols

	    (* otherwise generalize *)
	    else
	      match symbols with
		| [] -> [] (* no symbols of this arity anyways ... *)
		| _ -> get_totality_axiom arity None 1 :: []
	  in
	    axioms @ get_totality_axioms' tail

  (* get totality axiom for this arity.
     if symbol is given, then instantiate the totality axiom for this symbol,
     otherwise use a universal version for all symbols of this arity. *)
  and get_totality_axiom (arity: int) (symbol: symbol option) (i: int) : clause =
    if i > domain_size then begin
      domain_size_marker :: []
    end

    else begin
      (* create arguments of axiom literal *)
      let subterms =
	Array.make (arity + 2) (get_domain_element i)
      in
	for i = 0 to Array.length subterms - 2 do
	  subterms.(i) <- Term.request_var (Var.create_universal i);
	done;

	begin
	  match symbol with
	    | Some symbol ->
		subterms.(0) <- Term.request_const (Symbol.create_fd_symbol symbol);
	    | None -> ()
	end;

      (* create axiom literal *)
      let relation_term =
	Term.request_func (Symbol.get_fd_relation (arity + 2), subterms)
      in
      let relation_literal =
	Term.request_literal true relation_term
      in
	relation_literal :: get_totality_axiom arity symbol (i + 1)
    end
  in

  (* might perform static symmetry reduction *)
  let constants_axioms =
    if Const.fd_static_symmetry_reduction then
      get_symmetry_reduction_axioms finite_domain.sorts domain_size
    else
      let constants =
	(List.map fst finite_domain.term_definitions.constants) @ finite_domain.original#getConstantSymbols
      in
	get_totality_axioms' [(0, constants)]
  in

  let functions_axioms =
    get_totality_axioms' finite_domain.term_definitions.function_arities
  in
  let axioms =
    constants_axioms @ functions_axioms
  in
    if print then begin
      print_endline ("Totality axioms:");
      List.iter (fun axiom -> print_endline (Term.clause_to_string axiom)) axioms;
      print_newline ();
    end;
    if print_tptp then begin
      Array.iteri
	(fun i axiom ->
	  print_endline (
	      Term.tptp_clause_to_tptp_string
		("totality_" ^ string_of_int i)
		axiom
	  )
	)
	(Array.of_list axioms);
    end;
    axioms






(* use canonicity axioms in conjunction with static symmetry reduction.

    E.g., if the constants are a, b, c, d, and the domain size is 3, then the axioms are:
    c = 3 -> b = 2
    d = 3 -> b = 2 \/ c = 2
    d = 4 -> c = 3
 *)
let get_canonicity_axioms ~(print: bool) ~(print_tptp: bool) (sorts: sorts) (domain_size: int) : clause list =

  (* get the axioms for this domain size *)
  let rec get_canonicity_axioms_for_domain_size_ unary_function symbols (current_domain_size : int)
      (current_symbol_index: int) : clause list =
    (* all symbols done *)
    if current_symbol_index > domain_size then
      []

    (* get axioms for this symbol and domain size *)
    else
      let rec get_axiom current_symbol_index' =
	if current_symbol_index' = current_symbol_index then
	  let literal =
	    create_relation false unary_function
	      [| get_domain_element (current_symbol_index' - Array.length symbols) |]
	      (get_domain_element current_domain_size)
	  in
	    literal :: []
	  
	else if current_symbol_index' <= Array.length symbols then
	  let literal =
	    create_relation true symbols.(current_symbol_index' - 1)
	      [| |] (get_domain_element (current_domain_size - 1))
	  in
	    literal :: get_axiom (current_symbol_index' + 1)

	else
	  let literal =
	    create_relation true unary_function
	      [| get_domain_element (current_symbol_index' - Array.length symbols) |]
	      (get_domain_element (current_domain_size - 1))
	  in
	    literal :: get_axiom (current_symbol_index' + 1)
      in
      let axiom =
	get_axiom (current_domain_size - 1)
      in
	axiom
	::
	get_canonicity_axioms_for_domain_size_ unary_function symbols current_domain_size (current_symbol_index + 1)
  in

  (* get the axioms for this domain size *)
  let rec get_canonicity_axioms_for_domain_size unary_function (symbols: symbol array) (current_domain_size : int)
      (current_symbol_index: int) : clause list =
    (* all symbols done *)
    if current_symbol_index > Array.length symbols then begin
	if Const.fd_static_symmetry_reduction_unary then
	  match unary_function with
	    | None ->
		[]
		  
	    | Some unary_function ->
		get_canonicity_axioms_for_domain_size_ unary_function symbols
		  current_domain_size current_symbol_index
	else
	  []
      end

    (* get axioms for this symbol and domain size *)
    else
      let rec get_axiom current_symbol_index' =
	if current_symbol_index' = current_symbol_index then
	  let literal =
	    create_relation false symbols.(current_symbol_index' - 1)
	      [| |] (get_domain_element current_domain_size)
	  in
	    literal :: []
	  
	else
	  let literal =
	    create_relation true symbols.(current_symbol_index' - 1)
	      [| |] (get_domain_element (current_domain_size - 1))
	  in
	    literal :: get_axiom (current_symbol_index' + 1)
      in
      let axiom =
	get_axiom (current_domain_size - 1)
      in
	axiom
	::
	get_canonicity_axioms_for_domain_size unary_function symbols current_domain_size (current_symbol_index + 1)
  in

  (* get the axioms for these constants *)
  let rec get_canonicity_axioms' unary_function (symbols: symbol array) (current_domain_size : int) : clause list =
    if current_domain_size > domain_size then
      []
	
    else
      let axioms =	
	get_canonicity_axioms_for_domain_size unary_function symbols current_domain_size current_domain_size
      in
	axioms @ get_canonicity_axioms' unary_function symbols (current_domain_size + 1)
  in

  let axioms =
    List.fold_left
      (fun acc sort ->
	(* get the axioms for the symbols of each sort.
	   for domain size < 3 nothing to do, as we always have a constant fixed to 1 *)
	let symbols =
	  Sort_inference.get_constants_for_sort sorts sort
	in
	let unary_function =
	  Sort_inference.get_unary_function_for_sort sorts sort
	in
	let clique_size =
	  Sort_inference.get_max_clique_for_sort sorts sort
	in

	(* all elements of a largest clique are mapped to a domain element *)
	let start_domain_size =
	  max 3 (clique_size + 2)
	in
	  get_canonicity_axioms' unary_function (Array.of_list symbols) start_domain_size
	  @ acc
      )
      []
      (List.rev (Sort_inference.get_sorts sorts))
  in

    if print then begin
      print_endline ("Canonicity axioms:");
      List.iter (fun axiom -> print_endline (Term.clause_to_string axiom)) axioms;
      print_newline ();
    end;
    if print_tptp then begin
      Array.iteri
	(fun i axiom ->
	  print_endline (
	      Term.tptp_clause_to_tptp_string
		("canonicity_" ^ string_of_int i)
		axiom
	  )
	)
	(Array.of_list axioms);
    end;

    axioms
(*
let get_canonicity_axioms ~(print: bool) ~(print_tptp: bool) (constant_symbols: symbol list list) (domain_size: int) : clause list =

  (* get the axioms for this domain size *)
  let rec get_canonicity_axioms_for_domain_size (symbols: symbol array) (current_domain_size : int)
      (current_symbol_index: int) : clause list =
    (* all symbols done *)
    if current_symbol_index > Array.length symbols then
      []

    (* get axioms for this symbol and domain size *)
    else
      let rec get_axiom current_symbol_index' =
	if current_symbol_index' = current_symbol_index then
	  let literal =
	    create_relation false symbols.(current_symbol_index' - 1)
	      [| |] (get_domain_element current_domain_size)
	  in
	    literal :: []
	  
	else
	  let literal =
	    create_relation true symbols.(current_symbol_index' - 1)
	      [| |] (get_domain_element (current_domain_size - 1))
	  in
	    literal :: get_axiom (current_symbol_index' + 1)
      in
      let axiom =
	get_axiom (current_domain_size - 1)
      in
	axiom
	::
	get_canonicity_axioms_for_domain_size symbols current_domain_size (current_symbol_index + 1)
  in

  (* get the axioms for these constants *)
  let rec get_canonicity_axioms' (symbols: symbol array) (current_domain_size : int) : clause list =
    if current_domain_size > domain_size then
      []
	
    else
      let axioms =	
	get_canonicity_axioms_for_domain_size symbols current_domain_size current_domain_size
      in
	axioms @ get_canonicity_axioms' symbols (current_domain_size + 1)
  in

  let axioms =
    List.fold_left
      (fun acc symbols ->
	(* get the axioms for the symbols of each sort.
	   for domain size < 3 nothing to do, as we always have a constant fixed to 1 *)
	get_canonicity_axioms' (Array.of_list symbols) 3 @ acc
      )
      []
      (List.rev constant_symbols)
  in

    if print then begin
      print_endline ("Canonicity axioms:");
      List.iter (fun axiom -> print_endline (Term.clause_to_string axiom)) axioms;
      print_newline ();
    end;
    if print_tptp then begin
      Array.iteri
	(fun i axiom ->
	  print_endline (
	      Term.tptp_clause_to_tptp_string
		("canonicity_" ^ string_of_int i)
		axiom
	  )
	)
	(Array.of_list axioms);
    end;

    axioms
*)



(* use functionality axioms

    e.g, for domain size 3 and the binary function symbol f add:

    f(x, y) != 1 \/ f(x, y) != 2

    f(x, y) != 2 \/ f(x, y) != 1

    f(x, y) != 1 \/ f(x, y) != 3

    f(x, y) != 3 \/ f(x, y) != 1

    f(x, y) != 2 \/ f(x, y) != 3

    f(x, y) != 3 \/ f(x, y) != 2

    which in essence is for all domain different elements d, d':

    f(x, y) = d => f(x, y) != d'
 *)
let get_functionality_axioms ~(print: bool) ~(print_tptp: bool) (finite_domain: finite_domain) (domain_size: int) : clause list =

  let rec get_functionality_axioms_for_arity (arity: int) (current_domain_size: int)
      (symbol: symbol option) : clause list =
    (* all elements done *)
    if current_domain_size >= domain_size then
      []

    (* create axioms with all later domain elements *)
    else begin
      let rec get_functionality_axioms_for_arity' (current_domain_size': int) : clause list =
	if current_domain_size' > domain_size then
	  []
	    
	else
	  (* f(x) != y \/ f(x) != z *)
	  let subterms =
	    Array.make (arity + 2) Term.null_term
	  in
	  let subterms' =
	    Array.make (arity + 2) Term.null_term
	  in
	    for i = 0 to arity do
	      subterms.(i) <- Term.request_var (Var.create_universal i);
	      subterms'.(i) <- Term.request_var (Var.create_universal i);
	    done;
	    subterms.(arity + 1) <- get_domain_element current_domain_size;
	    subterms'.(arity + 1) <- get_domain_element current_domain_size';

	    begin
	      match symbol with
		| Some symbol ->
		    subterms.(0) <- Term.request_const (Symbol.create_fd_symbol symbol);
		    subterms'.(0) <- Term.request_const (Symbol.create_fd_symbol symbol);
		| None -> ()
	    end;

	  let left =
	    Term.request_literal
	      false
	      (Term.request_func (Symbol.get_fd_relation (arity + 2), subterms))
	  and right =	      
	    Term.request_literal
	      false
	      (Term.request_func (Symbol.get_fd_relation (arity + 2), subterms'))
	  in
	  let axiom =
	    [ left; right ]
	  in
	    axiom :: get_functionality_axioms_for_arity' (current_domain_size' + 1)
      in
      let axioms =
	get_functionality_axioms_for_arity' (current_domain_size + 1)
      in
	axioms @ get_functionality_axioms_for_arity arity (current_domain_size + 1) symbol
    end
  in 

  (* get functionality axioms for all arities *)
  let rec get_functionality_axioms' (arities': arities) : clause list =
    match arities' with
      | [] ->
	  []

      | (arity, symbols) :: tail ->
	  let axioms =
	    (* for explanation see get_totality_axioms' *)
	    if
	      Const.fd_isomorphism_abstraction
	      ||
	      Const.fd_instantiate_axioms
	      ||
	      (match symbols with | _ :: [] -> true | _ -> false)
	    then
	      List.map
		(fun symbol -> get_functionality_axioms_for_arity arity 1 (Some symbol) )
		symbols
		(* otherwise generalize *)
	    else
	      match symbols with
		| [] -> [] (* no symbols of this arity anyways ... *)
		| _ -> get_functionality_axioms_for_arity arity 1 None :: []
	  in
	    (List.flatten axioms) @ (get_functionality_axioms' tail)
  in	    

  let axioms =
    let constants =
      (List.map fst finite_domain.term_definitions.constants) @ finite_domain.original#getConstantSymbols
    in
      get_functionality_axioms' 
	((0, constants) :: finite_domain.term_definitions.function_arities)
  in
    if print then begin
      print_endline ("Functionality axioms:");
      List.iter (fun axiom -> print_endline (Term.clause_to_string axiom)) axioms;
      print_newline ();
    end;
    if print_tptp then begin
      Array.iteri
	(fun i axiom ->
	  print_endline (
	      Term.tptp_clause_to_tptp_string
		("functionality_" ^ string_of_int i)
		axiom
	  )
	)
	(Array.of_list axioms);
    end;
    axioms





(* equality axioms, e.g. for domain size 3:
   -diff(x, x)
    diff(1, 2)
    diff(2, 1)
    diff(1, 3)
    diff(3, 1)
    diff(2, 3)
    diff(3, 2)
*)
let get_equality_axioms ~(print: bool) ~(print_tptp: bool) (domain_size: int) : clause list =
  (* iterates over the first element of a diff pair *)
  let rec outer_loop i =
    if i > domain_size then begin
      []
    end

    else begin
      inner_loop i 1
    end

  (* pairs all elements with i *)
  and inner_loop i j =
    (* done *)
    if j > domain_size then
      outer_loop (i + 1)

    (* ignore if same elemten *)
    else if i = j then
      inner_loop i (j + 1)

    (* add axiom *)
    else
      let diff =
	create_neg_equality (get_domain_element i) (get_domain_element j)
      in
	[diff] :: inner_loop i (j + 1)
  in

  (* -diff(X, X) *)
  let reflexivity =
    create_pos_equality
      (Term.request_var (Var.create_universal 0))
      (Term.request_var (Var.create_universal 0))
  in
  let axioms =
    [reflexivity] :: outer_loop 1
  in
    if print then begin
      print_endline ("Equality axioms:");
      List.iter (fun axiom -> print_endline (Term.clause_to_string axiom)) axioms;
      print_newline ();
    end;
    if print_tptp then begin
      Array.iteri
	(fun i axiom ->
	  print_endline (
	      Term.tptp_clause_to_tptp_string
		("equality_" ^ string_of_int i)
		axiom
	  )
	)
	(Array.of_list axioms);
    end;
    axioms



(* define the elements of each sort,
   which can be permutated in a model preserving isomorphism. *)
let get_perm_axioms ~(print: bool) ~(print_tptp: bool) (finite_domain: finite_domain) (domain_size: int) : clause list =
  let sorts =
    finite_domain.sorts
  in
  let rec do' sort i axioms =
    if i = 0 then
      axioms
    else
      let axiom =
	Term.request_literal
	  (Sort_inference.is_permutable_ sorts sort i)
	  (Term.request_func (Sort_inference.get_permutable sort, [| get_domain_element i |]))
	::
	  []
      in
	do' sort (i - 1) (axiom :: axioms)
  in
  let axioms =
    List.fold_left
      (fun axioms sort ->
	 (* all elements permutable *)
	 if Sort_inference.is_permutable_ sorts sort 1 then begin
	   (* no permutation restrictions added to lemma in this case,
	      so no need for axiomatization at all. *)
	   (*let axiom =
	     Term.request_literal
	       true
	       (Term.request_func (
		  Sort_inference.get_permutable sort,
		  [| Term.request_var (Var.create_universal 0) |]))
	     ::
	       []
	   in
	     axiom ::*) axioms
	 end

	 (* list the permutable elements *)
	 else
	   do' sort domain_size axioms
      )
      []
      (List.rev (Sort_inference.get_sorts sorts))
  in
    if print then begin
      print_endline ("Permutation axioms:");
      List.iter (fun axiom -> print_endline (Term.clause_to_string axiom)) axioms;
      print_newline ();
    end;
    if print_tptp then begin
      Array.iteri
	(fun i axiom ->
	  print_endline (
	      Term.tptp_clause_to_tptp_string
		("perm_" ^ string_of_int i)
		axiom
	  )
	)
	(Array.of_list axioms);
    end;
    axioms




let get_axioms ~(print: bool) ~(print_tptp: bool) ~(use_functionality_axioms: bool)
    (finite_domain: finite_domain) (domain_size: int) : clause list =
  if print then begin
    print_newline ();
    print_endline ("Finite domain axioms for domain size " ^ string_of_int domain_size ^ ":");
    print_newline ();
  end;

  (* to ensure evaluation and print order, create axioms in order *)
  let domain_size_axioms =
    get_domain_size_axioms ~print:print ~print_tptp:print_tptp domain_size
  and totality_axioms =
    get_totality_axioms ~print:print ~print_tptp:print_tptp finite_domain domain_size
  and canonicity_axioms =
    if Const.fd_use_canonicity then
      get_canonicity_axioms ~print:print ~print_tptp:print_tptp 
	finite_domain.sorts domain_size
    else
      []
  and functionality_axioms =
    if use_functionality_axioms then
      get_functionality_axioms ~print:print ~print_tptp:print_tptp finite_domain domain_size
    else
      []
  and equality_axioms =
    (* equality handled in theory, not by axioms *)
    if Const.fd_constraint_solver then
      []

    (* these axioms are needed if the clause set contains equalities *)
    else if
      (* this introduces equalities in lemmas *)
      Const.fd_isomorphism_abstraction
      ||
      (* the input clause set contains equalities *)
      finite_domain.original#containsEquality
      ||
      (* this introduces equalities in term definitions *)
      not finite_domain.term_definitions.definitions#is_empty
    then
      get_equality_axioms ~print:print ~print_tptp:print_tptp domain_size
    else
      []	
  and perm_axioms =
    if Const.fd_isomorphism_abstraction && not Const.fd_constraint_solver then
      get_perm_axioms ~print:print ~print_tptp:print_tptp finite_domain domain_size
    else
      []
  in

  let axioms =
    List.map
      (fun clause ->
	 to_diff_clause (Subst.normalize_clause clause)
      )
      (domain_size_axioms @ totality_axioms @ canonicity_axioms @ functionality_axioms @ equality_axioms @ perm_axioms)
  in

  axioms







let get_domain_size finite_domain =
  match finite_domain.bound with
    | Some bound -> bound#current_bound
    | None -> failwith "Finite_domain.get_domain_size"




let relation_to_equations (finite_domain: finite_domain) (term: term) : term list =
  match term with
    | Term.Func func ->
	(* no relation, just keep term *)
	if not (Symbol.is_fd_relation func.Term.symbol) then
	  term :: []

	else begin
	  match func.Term.subterms.(0) with
	    | Term.Func _ ->
		(* must be flat *)
		failwith ("Finite_domain.relation_to_equation: " ^ Term.term_to_string term)
				    
	    | Term.Const symbol ->
		(* transform into equation *)
		let symbol' =
		  Symbol.get_symbol_from_fd_symbol symbol
		in
		let result =
		  func.Term.subterms.(Array.length func.Term.subterms - 1)
		in
		let function_term =
		  if Symbol.arity symbol' = 0 then
		    Term.request_const symbol'
		  else
		    let arguments =
		      Array.sub func.Term.subterms 1 (Array.length func.Term.subterms - 2)
		    in
		      Term.request_func (symbol', arguments)
		in
		  Term.request_func (Symbol.equality, [| function_term; result |]) :: []
				    
	    | Term.Var _ ->
		(* transform into all equations of that arity *)
		let function_symbols =
		  Problem.get_arity
		    (get_problem finite_domain)#getFunctionArities
		    (Symbol.arity func.Term.symbol - 2)
		in
		  List.map
		    (fun symbol ->
		      (* transform into equation *)
		      let result =
			func.Term.subterms.(Array.length func.Term.subterms - 1)
		      in
		      let arguments =
			Array.sub func.Term.subterms 1 (Array.length func.Term.subterms - 2)
		      in
		      let function_term =
			Term.request_func (symbol, arguments)
		      in
			Term.request_func (Symbol.equality, [| function_term; result |])
		    )
		    function_symbols
	  end

    | _ ->
	(* no relation, just keep term *)
	term :: []
