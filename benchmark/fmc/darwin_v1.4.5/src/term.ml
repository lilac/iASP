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



(* the term database consists basically of hash sets containing the terms.

   the hash values are for a
   - variable term: its var's id
   - constant term: its symbol's id
   - function term: its hash value
   - literal: its enclosed term's hash value

   terms are simply stored in a weak set.
   as long as a term is referenced outside of the set
   it is kept alive and subsequent creation requests return the same term.
   as soon as the term is not referenced outside of the set
   it may be automatically garbage collected.
*)


(*** types ***)

type var = Var.var
type symbol = Symbol.symbol


type func = {
  symbol: symbol;
  subterms: term array;
  ground: bool;
  hash: int;
  in_db: bool;
}

and term =
  | Var of var
  | Const of symbol
  | Func of func

type literal = {
  sign: bool;
  atom: term;
  literal_in_db: bool;
}

type clause =
    literal list




(*** hashing ***)

(* the hash value of a term. *)
let hash_of_term (term: term) : int =
  match term with
    | Var var ->
	Var.hash_of_var var
	  
    | Const symbol ->
	Symbol.id symbol
	  
    | Func func ->
	func.hash


let hash_of_literal (literal: literal) : int =
  hash_of_term literal.atom


let hash_of_clause (clause: clause) : int =
  List.fold_left
    (fun acc literal ->
      (acc + (hash_of_literal literal)) * 131
    )
    0
    clause


(* computation of the hash value of a Func term.

   131 is known to work quite well by ... someone Cesare Tinelli knows.
   tests seem to support it. *)
let compute_hash (predicate_symbol: symbol) (arguments: term array) : int =

  let compute_hash' (old_value: int) (new_value: int) =
    (old_value + new_value) * 131
  in

    Array.fold_left 
      (fun old_hash term ->
	 compute_hash' old_hash (hash_of_term term)
      )
      (compute_hash' 0 (Symbol.id predicate_symbol))
      arguments








(*** comparison ***)

let rec term_equal (term1: term) (term2: term) : bool =
  term1 == term2
  ||
  begin
    match term1, term2 with
      | Func func1, Func func2 ->
	  not (func1.in_db && func2.in_db)
	  &&
	  Symbol.equal func1.symbol func2.symbol
	  &&
	  Tools.array_for_all2
	    (fun term1 term2 ->
	       term_equal term1 term2
	    )
	    func1.subterms
	    func2.subterms

      | _ ->
	  false
  end


let literal_equal (literal1: literal) (literal2: literal) : bool =
  literal1 == literal2
  ||
  begin
    not (literal1.literal_in_db && literal2.literal_in_db)
    &&
    (literal1.sign == literal2.sign)
    &&
    (term_equal literal1.atom literal2.atom)
  end


let clause_equal (clause1: clause) (clause2: clause) : bool =
  Tools.lists_unordered_equal literal_equal clause1 clause2

let clause_approx_equal (clause1: clause) (clause2: clause) : bool =
  Tools.lists_equal literal_equal clause1 clause2








(*** database types ***)


(* term database *)
module TermTableBase =
  Weak.Make (
    struct
      type t = term

      let rec are_terms_equal_at (index: int) (terms1: term array) (terms2: term array) : bool =
	if index >= Array.length terms1 then
	  true
	else begin
	  (* subterms can be compared using pointer equality,
	     as a term is constructed bottom up with this database *)
	  if terms1.(index) == terms2.(index) then
	    are_terms_equal_at (index + 1) terms1 terms2
	  else
	    false
	end


      (* term_equal can not be used, as it uses object identity 
	 which is assured exactly by this module *)
      let equal (term1: term) (term2: term) : bool =
	match term1, term2 with
	  | Var var1, Var var2 ->
	      Var.equal var1 var2

	  | Const const1, Const const2 ->
	      Symbol.equal const1 const2

	  | Func func1, Func func2 ->
	      (Symbol.equal func1.symbol func2.symbol)
	      &&
	      (are_terms_equal_at 0 func1.subterms func2.subterms)

	  | _ ->
	      false

      let hash (term: term) : int =
	hash_of_term term
    end
  )


(* literal database *)
module LiteralTableBase =
  Weak.Make (
    struct
      type t = literal

      let equal (literal1: literal) (literal2: literal) : bool =
	(literal1.sign == literal2.sign)
	&&
	(literal1.atom == literal2.atom)

      let hash (literal: literal) : int =
	hash_of_literal literal

    end
  )




type database = {
  (* contains the terms: consts, vars, functions *)
  terms: TermTableBase.t;
  (* contains the literals *)
  literals: LiteralTableBase.t;
}






(*** globals ***)

(* this global database contains the global terms *)
let global: database =
  {
    terms = TermTableBase.create 2048;
    literals = LiteralTableBase.create 512;
  }




(*** decomposition ***)

let top_symbol_term (term: term) : symbol =
  match term with
    | Var _ -> raise Not_found
    | Const symbol -> symbol
    | Func func -> func.symbol

let top_symbol_literal (literal: literal) : symbol =
  top_symbol_term literal.atom


let is_term_var (term: term) : bool =
  match term with
    | Var _ -> true
    | _ -> false

let is_literal_var (literal: literal) : bool =
  is_term_var literal.atom



let rec do_vars_of_term (acc: var list) (term: term) : var list =
  match term with
    | Var var ->
	(* store each variable only once *)
	if List.exists
	  (fun acc_var ->
	     Var.equal acc_var var
	  )
	  acc
	then
	  acc
	else
	  var :: acc

    | Const _ ->
	acc

    | Func func ->
	if func.ground then
	  acc
	else
	  Array.fold_left do_vars_of_term acc func.subterms

let vars_of_term (term: term) : var list =
  do_vars_of_term [] term

let vars_of_literal (literal: literal) : var list =
  vars_of_term literal.atom

let vars_of_clause (clause: clause) : var list =
  List.fold_left
    (fun acc literal ->
      do_vars_of_term acc literal.atom
    )
    []
    clause


let rec term_contains_var (term: term) (var: var) : bool =
  match term with
    | Var var' ->
	Var.equal var var'
	  
    | Const _ ->
	false
	  
    | Func func ->
	Tools.array_exists
	  (fun term' -> term_contains_var term' var)
	  func.subterms

let literal_contains_var literal var =
  term_contains_var literal.atom var



let rec term_contains_term (term: term) (contained: term) : bool =
  term_equal contained term
  ||
  match term with
    | Var _
    | Const _ ->
	false
	  
    | Func func ->
	Tools.array_exists
	  (fun term' -> term_contains_term term' contained)
	  func.subterms

let literal_contains_term literal term =
  term_contains_term literal.atom term



let rec is_term_ground (term: term) : bool =
  match term with
    | Var _ ->
	false

    | Const _ ->
	true

    | Func func ->
	func.ground

let is_literal_ground literal =
  is_term_ground literal.atom









(*** string representation ***)

let rec term_to_string ?(pretty:bool = true) (term: term) : string =
  match term with
    | Var var ->
	Var.to_string var

    | Const const ->
	if Symbol.is_fd_element const then
	  "'" ^ Symbol.to_string ~pretty:pretty const
	else
	  Symbol.to_string ~pretty:pretty const

    | Func func when Array.length func.subterms = 0 ->
	(* a constant in disguise *)
	let const = func.symbol in
	if Symbol.is_fd_element const then
	  "'" ^ Symbol.to_string ~pretty:pretty const
	else
	  Symbol.to_string ~pretty:pretty const

    | Func func ->
	if pretty && Symbol.equal Symbol.equality func.symbol then begin
	  "(" ^ term_to_string ~pretty:pretty func.subterms.(0)
	    ^ " = "
	    ^ term_to_string ~pretty:pretty  func.subterms.(1) ^ ")"
	end

	(* finite model: r(a, ...) ---> r_a(...) *)
	else if pretty
	  &&
	  Symbol.is_fd_relation func.symbol
	then begin
	  let constant, subterms =
	    match Array.to_list func.subterms with
	      | []
	      | _ :: [] ->
		  failwith ("Term.term_to_string: fd relation of arity < 2: " ^ Symbol.to_string func.symbol);

	      | head :: tail ->
		  head, tail
	  in
	  let index =
	    if is_term_var constant then
	      "r_" ^ string_of_int (Symbol.arity func.symbol - 1)
	    else
	      "r_" ^ Symbol.to_string ~pretty:pretty (top_symbol_term constant)
	  in
	    index
	    ^ "("
	    ^ String.concat ", " (List.map (term_to_string ~pretty:pretty) subterms)
	    ^ ")"
	end

	else begin
	  Symbol.to_string ~pretty:pretty func.symbol
	  ^ "("
	  ^ String.concat ", " (List.map (term_to_string ~pretty:pretty) (Array.to_list func.subterms))
	  ^ ")"
	end





(*** term sort ***)

let get_term_sort (term: term) : Symbol.sort =
  match term with
    | Const symbol
    | Func { symbol = symbol } ->
	Symbol.sort symbol

    | _ ->
	failwith "Term.get_term_sort"

let get_literal_sort (literal: literal) : Symbol.sort =
  get_term_sort literal.atom


let is_connection_literal (literal: literal) : bool =
  try
    Symbol.is_connection (top_symbol_literal literal)
  with
    | Not_found ->
	false

let is_fd_relation (literal: literal) : bool =
  try
    Symbol.is_fd_relation (top_symbol_literal literal)
  with
    | Not_found ->
	false

let is_fd_size_marker (literal: literal) : bool =
  try
    Symbol.is_fd_size_marker (top_symbol_literal literal)
  with
    | Not_found ->
	false

let is_fd_element (term: term) : bool =
  try
    Symbol.is_fd_element (top_symbol_term term)
  with
    | Not_found ->
	false

let is_fd_constraint literal =
  match literal.atom with
    | Const _
    | Var _ ->
	false
	  
    | Func func ->
	Symbol.equal func.symbol Symbol.equality
	||
	Symbol.equal func.symbol Symbol.diff
	||
	Symbol.is_fd_permutable func.symbol

let rec is_input_term (term: term) : bool =
  match term with
    | Var _ ->
	true

    | Const symbol ->
	Symbol.is_input symbol

    | Func func ->
	Symbol.is_input func.symbol
	&&
	Tools.array_for_all is_input_term func.subterms

let is_input_literal (literal: literal) : bool =
  is_input_term literal.atom


let rec is_fd_term (term: term) : bool =
  match term with
    | Var _ ->
	true

    | Const symbol ->
	Symbol.is_input symbol
	||
	Symbol.is_fd_symbol symbol
	||
	Symbol.is_fd_element symbol

    | Func func ->
	(
	  Symbol.is_input func.symbol
	  ||
	  Symbol.is_fd_relation func.symbol
	  ||
	  (Symbol.equal Symbol.diff func.symbol)
	)
	&&
	Tools.array_for_all is_fd_term func.subterms



let is_fd_literal (literal: literal) : bool =
  is_fd_term literal.atom


let rec is_skolem_free_term (term: term) : bool =
  match term with
    | Var _ ->
	true

    | Const const ->
	not (Symbol.is_skolem const)

    | Func func ->
	not (Symbol.is_skolem func.symbol)
	&&
	Tools.array_for_all is_skolem_free_term func.subterms

let rec is_skolem_free_literal (literal: literal) : bool =
  is_skolem_free_term literal.atom





(*** Hash tables ***)


(* now export tables for further use.
   here equality can be checked by pointer identity. *)

module TermTable =
  Hashtbl.Make (
    struct
      type t = term

      let equal = term_equal
      let hash = hash_of_term
    end
  )


(* access by literal *)
module LiteralTable =
  Hashtbl.Make (
    struct
      type t = literal
	  
      let equal = literal_equal
      let hash = hash_of_literal
    end
  )

module ClauseTable =
  Hashtbl.Make (
    struct
      type t = clause
	  
      let equal = clause_equal
      let hash = hash_of_clause
    end
  )


module ClauseApproxTable =
  Hashtbl.Make (
    struct
      type t = clause
	  
      let equal = clause_approx_equal
      let hash = hash_of_clause
    end
  )

(* access by literal type, i.e. sign and predicate symbol
   but not the concrete literal. *)
module LiteralTypeTable =
  Hashtbl.Make (
    struct
      type t = literal

      let equal (literal1: literal) (literal2: literal) : bool =
	(literal1.sign == literal2.sign)
	&&
	match literal1.atom, literal2.atom with
	  | Func func1(*(symbol1, _, _)*), Func func2(*(symbol2, _, _)*) ->
	      (* same predicate symbol *)
	      Symbol.equal func1.symbol func2.symbol
		
	  | _ ->
	      (* or for var/const same term *)
	      literal1.atom == literal2.atom

      let hash (literal: literal) : int =
	match literal.atom with
	  | Const symbol
	  | Func { symbol = symbol } (*(symbol, _, _)*) ->
	      Symbol.id symbol

	  | Var var ->
	      Var.hash_of_var var
    end
  )








(*** creation ***)



(* these functions try to find a requested term in the database
   and to return the old instance.

   otherwise a fresh instance is created, stored, and returned. *)


let request_term ?(insert_db:bool = true) (term: term) : term =
  try
    TermTableBase.find global.terms term
  with
    | Not_found ->
	if insert_db then begin
	  TermTableBase.add global.terms term;
	end;
 	term


let request_var (var: var) : term =
  let new_term =
    Var var
  in
    request_term new_term

let request_const (symbol: symbol) : term =
  if Const.debug then begin
    if Symbol.arity symbol != 0 then begin
      failwith ("Term.request_const on non-constant symbol: " ^ Symbol.to_string symbol);
    end
  end;

  let new_term =
    Const symbol
  in
    request_term new_term


let request_func ?(insert_db:bool = true) ((symbol, terms): symbol * term array) : term =
  if Const.debug then begin
    if (Symbol.arity symbol != Array.length terms) then begin
      print_newline ();
      print_endline (Symbol.to_string symbol);
      print_endline (string_of_int (Symbol.arity symbol));
      Array.iter (fun term -> print_endline (term_to_string term)) terms;
      failwith "request_func: arity mismatch";
    end;
  end;

  let new_term =
    Func (*(symbol, terms, compute_hash symbol terms)*)
      {
	symbol = symbol;
	subterms = terms;
	ground = Tools.array_for_all is_term_ground terms;
	hash = compute_hash symbol terms;
	in_db = insert_db;
      }
  in
   request_term ~insert_db:insert_db new_term

let request_literal ?(insert_db:bool = true) (sign: bool) (term: term) : literal =
  let new_literal = {
    sign = sign;
    atom = term;
    literal_in_db = insert_db;
  }
  in
    try
      LiteralTableBase.find global.literals new_literal
    with
      | Not_found ->
	  if insert_db then begin
	    LiteralTableBase.add global.literals new_literal;
	  end;
 	  new_literal



let rec insert_term term =
  match term with
    | Func func ->
	if func.in_db then
	  term

	else begin
	  let subterms =
	    Array.map insert_term func.subterms
	  in
	    request_func (func.symbol, subterms)
	end

    | _ ->
	term

let insert_literal literal =
  if literal.literal_in_db then
    literal

  else begin
    let term =
      insert_term literal.atom
    in
      request_literal literal.sign term
  end





(*** string representation ***)





let literal_to_string ?(pretty:bool = true) (literal: literal) : string =
  (* replace diff by -equal *)
  let literal =
    match literal.atom with
      | Func func when pretty && Symbol.equal Symbol.diff func.symbol ->
	  request_literal
	    (not literal.sign)
	    (request_func (Symbol.equality, func.subterms))
      | _ ->
	  literal
  in

  let prefix =
    if literal.sign then
      "+" (* "" *)
    else
      "-" (* "~" *)
  in
  let term =
    term_to_string ~pretty:pretty literal.atom
  in
    prefix ^ term



let clause_to_string ?(pretty:bool = true) (clause: clause) : string =
  "[" ^ String.concat ", " (List.map (literal_to_string ~pretty:pretty) clause) ^ "]"









(*** Term Modification ***)


let request_negated_literal ?(insert_db:bool = true) (literal: literal) : literal =
  request_literal ~insert_db:insert_db (not literal.sign) literal.atom


let replace_term_in_term (term: term) (old_term: term) (new_term: term) : term =

  let rec replace_in_term' (term: term) : term =
    if term_equal term old_term then
      new_term
    else
      match term with
	| Var _
	| Const _ ->
	    term
	    
	| Func func ->
	    let new_terms =
	      Array.map
		(fun term ->
		   replace_in_term' term
		)
		func.subterms
	    in
	      request_func (func.symbol, new_terms)
  in
    if term_equal old_term new_term then
      term
    else
      replace_in_term' term
		

let replace_term_in_literal literal old_term new_term =
  request_literal
    literal.sign
    (replace_term_in_term literal.atom old_term new_term)




let rec replace_terms_in_term (term: term) (mapping: (term * term) list) : term =

  let rec replace_in_term' (term: term) : term =
    try
      let (_, new_term) =
	List.find
	  (fun (old_term, _) -> term_equal term old_term)
	  mapping
      in
	new_term
    with
      | Not_found ->
	  begin
	    match term with
	      | Var _
	      | Const _ ->
		  term
		    
	      | Func func ->
		  let new_terms =
		    Array.map
		      (fun term ->
			 replace_in_term' term
		      )
		      func.subterms
		  in
		    request_func (func.symbol, new_terms)
	  end
  in
    replace_in_term' term

let replace_terms_in_literal literal map =
  request_literal
    literal.sign
    (replace_terms_in_term literal.atom map)



let rec replace_vars_in_term (term: term) (func: var -> term -> term) : term =
  match term with
    | Var var ->
	func var term
	  
    | Const _ ->
	term
	  
    | Func func' ->
	let new_terms =
	  Array.map
	    (fun term ->
	       replace_vars_in_term term func
	    )
	    func'.subterms
	in
	  request_func (func'.symbol, new_terms)
		

let replace_vars_in_literal literal func =
  request_literal
    literal.sign
    (replace_vars_in_term literal.atom func)




let rec remove_duplicates clause =
  match clause with
    | [] ->
	[]

    | literal :: tail ->
	if
	  List.exists
	    (fun literal' -> literal_equal literal literal')
	    tail
	then
	  tail
	else
	  literal :: remove_duplicates tail








(* does also normalize the remaining variables,
   i.e. removes 'gaps' created by skolemizing some variables *)
let rec request_skolemized_term' (term: term)
    ((var_skolemization, par_normalization) as acc: ((var * term) list) * ((var * term) list))
    : ((var * term) list) * ((var * term) list) =

    match term with
      | Var var ->
	  if Var.is_universal var then begin
	    if List.exists (fun (var', _) -> Var.equal var var') var_skolemization then
	      acc
	    else
	      let fresh_const =
		request_const (Symbol.create_skolem ())
	      in
		((var, fresh_const) :: var_skolemization), par_normalization
	  end

	  else begin
	    if List.exists (fun (var', _) -> Var.equal var var') par_normalization then
	      acc
	    else
	      let fresh_par =
		(* the next id can be deduced from the number of normalized pars *)
		Var.clone_renumbered var (List.length par_normalization)
	      in
	      let fresh_term =
		request_var fresh_par
	      in
		var_skolemization, ((var, fresh_term) :: par_normalization)
	  end

      | Const _ ->
	  acc

      | Func func ->
	  Array.fold_left
	    (fun acc' term' ->
	      request_skolemized_term' term' acc'
	    )
	    acc
	    func.subterms

let request_skolemized_term (term: term) : term =
  let var_skolemization, par_normalization =
    request_skolemized_term' term ([], [])
  in
    match var_skolemization with
      | [] ->
	  (* no universal variables in term *)
	  term

      | _ ->
	  let mapping =
	    List.map
	      (fun (var, term) ->
		request_var var, term
	      )
	      (par_normalization @ var_skolemization)
	  in
	    replace_terms_in_term term mapping


let request_skolemized_literal (literal: literal) : literal =
  request_literal literal.sign (request_skolemized_term literal.atom)


let request_skolemized_clause (clause: clause) : clause =
  let var_skolemization, par_normalization =
    List.fold_left
      (fun acc literal ->
	request_skolemized_term' literal.atom acc
      )
      ([], [])
      clause
  in
    match var_skolemization with
      | [] ->
	  (* no universal variables in term *)
	  clause

      | _ ->
	  let mapping =
	    List.map
	      (fun (var, term) ->
		request_var var, term
	      )
	      (par_normalization @ var_skolemization)
	  in
	    List.map
	      (fun literal ->
		replace_terms_in_literal literal mapping
	      )
	      clause





let rec request_pure_term' ~(universal: bool) (term: term) (mapping: (var * term) list) : (var * term) list  =
  match term with
    | Var var ->
	if Var.is_universal var != universal then begin
	  if List.exists (fun (var', _) -> Var.equal var var') mapping then
	    mapping
	  else
	    let fresh_var =
	      if universal then
		Var.create_universal (List.length mapping)
	      else
		Var.create_parametric (List.length mapping)
	    in
	    let fresh_term =
	      request_var fresh_var
	    in
	      (var, fresh_term) :: mapping
	end

	else
	  (* ensure that no parameter is mapped to this variable
	     by creating a fresh variable with the same id *)
	  (var, term) :: mapping

      | Const _ ->
	  mapping

      | Func func ->
	  Array.fold_left
	    (fun mapping' term' ->
	      request_pure_term' ~universal:universal term' mapping'
	    )
	    mapping
	    func.subterms

let request_pure_term ~(universal: bool) (term: term) : term =
  let mapping =
    request_pure_term' ~universal:universal term []
  in
    match mapping with
      | [] ->
	  (* no parametric variables in term *)
	  term

      | _ ->
	  let mapping =
	    List.map
	      (fun (var, term) ->
		request_var var, term
	      )
	      mapping
	  in
	    replace_terms_in_term term mapping

let request_pure_literal ~(universal: bool) (literal: literal) : literal =
  request_literal literal.sign (request_pure_term ~universal:universal literal.atom)

let request_pure_clause ~(universal: bool) (clause: clause) : clause =
  let mapping =
    List.fold_left
      (fun acc literal ->
	request_pure_term' ~universal:universal literal.atom acc
      )
      []
      clause
  in
    match mapping with
      | [] ->
	  clause

      | _ ->
	  let mapping =
	    List.map
	      (fun (var, term) ->
		request_var var, term
	      )
	      mapping
	  in
	    List.map
	      (fun literal ->
		replace_terms_in_literal literal mapping
	      )
	      clause











(*** special literals ***)


(*** creation ***)

let true_literal : literal =
  request_literal true (request_const (Symbol.create_predicate "__true__" 0))

let false_literal : literal =
  request_negated_literal true_literal
(*  request_literal true (request_const (Symbol.create_predicate "false" 0))*)

let assert_literal : literal =
  request_literal true (request_const (Symbol.create_predicate "__assert__" 0))

let init_literal : literal =
  request_literal true (request_const (Symbol.create_predicate "__init__" 0))

let null_term : term =
  request_const (Symbol.create_function "__null_term__" 0)

let null_literal : literal =
  request_literal true null_term

let v_par =
  Var.create_parametric 0

let v_term =
  let v_name =
    "__default_v_constant__"
  in
    request_const (Symbol.create_function v_name 0)

let minus_v : literal =
  let term =
    request_func (
      Symbol.create_predicate
		    "__-v__" 1,
		    [| request_var v_par |]
    )
  in
    request_literal false term

let plus_v : literal =
  let term =
    request_func (
      Symbol.create_predicate
			   "__+v__" 1,
			   [| request_var v_par |]
    )
  in
    request_literal true term








(*** schema terms ***)

(* is this a schema term? *)
let is_schema_term (term: term) : bool =
  let terms =
    match term with
      | Func func -> func.subterms
      | _ -> [| |]
  in
    try
      ignore (
	Array.fold_left
	  (fun acc term ->
	     match term with
	       | Var var ->
		   (* check that this variable has not been used before.*)
		   if List.exists (fun old_var -> Var.equal var old_var) acc then
		     raise Exit
		   else
		     var :: acc
			 
	       | _ ->
		   (* must be a variable *)
		   raise Exit
	  )
	  []
	  terms
      );
      true
    with
      | Exit ->
	  false

(* create the schema term for a symbol. *)
let create_schema_term (predicate: symbol) : term =
  if Symbol.arity predicate = 0 then
    request_const predicate

  else
    (* create the most general term,
       i.e. a term with a fresh parameter at each position *)
    let terms =
      Array.make (Symbol.arity predicate) null_term
    in
      for i = 0 to Symbol.arity predicate - 1 do
      terms.(i) <- request_var (Var.create_universal i)
      done;
      request_func (predicate, terms)


(* create the schema term for a term. *)
let create_schema_term_from_term (term: term) : term =
  match term with
    | Const _ ->
	(* consts are most general *)
	term

    | Func func ->
	create_schema_term func.symbol

    | Var _ ->
	failwith "Context.create_schema"





(*** clause properties ***)


let is_definit (clause: literal list) : bool =

  let rec is_definit' (clause: literal list) (pos_count: bool) : bool =
    match clause with
      | [] ->
	  pos_count
	    
      | literal :: tail ->
	  if literal.sign then begin
	    if pos_count then
	      false
	    else
	      is_definit' tail true
	  end
	  else
	    is_definit' tail pos_count
  in
    is_definit' clause false

let is_Horn (clause: literal list) : bool =

  let rec is_horn' (clause: literal list) (pos_count: bool) : bool =
    match clause with
      | [] ->
	  true
	    
      | literal :: tail ->
	  if literal.sign then begin
	    if pos_count then
	      false
	    else
	      is_horn' tail true
	  end
	  else
	    is_horn' tail pos_count
  in
    is_horn' clause false


let are_Horn (clauses: clause list) : bool =
  List.for_all
    (fun clause ->
       is_Horn clause
    )
    clauses




let is_BS (clause: clause) : bool =
  List.for_all
    (fun literal ->
       match literal.atom with
	 | Var _ ->
	     failwith "Term.is_BS"
	       
	 | Const _ ->
	     true
	       
	 | Func func ->
	     Tools.array_for_all
	       (fun term ->
		  match term with
		    | Var _
		    | Const _ ->
			true
			  
		    | Func _ ->
			false
	       )
	       func.subterms
    )
    clause

let are_BS (clauses: clause list) : bool =
  List.for_all is_BS clauses



let contains_empty_clause (clauses: clause list) : bool =
  List.exists
    (List.for_all (literal_equal false_literal))
    clauses


let contains_equality (clauses: clause list) : bool =
  List.exists
    (fun clause ->
       List.exists
	 (fun literal ->
	    match literal.atom with
	      | Func func ->
		  Symbol.equal Symbol.equality func.symbol
		    
	      | _ ->
		  false
       )
       clause
    )
    clauses







(*** Variants ***)



let are_terms_variants (_term1: term) (_term2: term) : bool =

  (* acc keeps track of the bijective mapping
     from the variables of _term1 to the variables of _term2 *)
  let rec do_variants (acc: (var * var) list) (term1: term) (term2: term) :
    (var * var) list =
    
    match term1, term2 with
      | Var var1, Var var2 when
	  Var.is_universal var1 == Var.is_universal var2 ->
	  Tools.mapping_extend acc Var.equal Var.equal var1 var2
	      
      | Const symbol1, Const symbol2 when
	  Symbol.equal symbol1 symbol2 ->
	  acc
	      
      | Func func1, Func func2 when
	  Symbol.equal func1.symbol func2.symbol ->
	  Tools.array_fold2 do_variants acc func1.subterms func2.subterms
	      
      | _ ->
	  raise Exit
  in	    
    (term_equal _term1 _term2)
    ||
    (
      try
	ignore (do_variants [] _term1 _term2);
	true
      with
	| Exit ->
	    false
    )


let are_literals_variants (literal1: literal) (literal2: literal) : bool =
  (literal1.sign == literal2.sign)
  &&
  (are_terms_variants literal1.atom literal2.atom)



let are_terms_skolem_variants (_term1: term) (_term2: term) : bool =

  let var_mapping =
    ref []
  in
  let skolem_mapping =
    ref []
  in

  (* acc keeps track of the bijective mapping
     from the variables of _term1 to the variables of _term2 *)
  let rec do_variants (term1: term) (term2: term) : unit =    
    match term1, term2 with
      | Var var1, Var var2 when
	  Var.is_universal var1 == Var.is_universal var2 ->
	  var_mapping := Tools.mapping_extend !var_mapping Var.equal Var.equal var1 var2
	      
      | Const symbol1, Const symbol2 when
	  Symbol.equal symbol1 symbol2 ->
	  ()
	      
      | Const symbol1, Const symbol2 when
	  Symbol.is_skolem symbol1
	  &&
	  Symbol.is_skolem symbol2
	  ->
	  skolem_mapping := Tools.mapping_extend !skolem_mapping Symbol.equal Symbol.equal symbol1 symbol2

      | Func func1, Func func2 when
	  Symbol.equal func1.symbol func2.symbol ->
	  Tools.array_iter2 do_variants func1.subterms func2.subterms
	      
      | _ ->
	  raise Exit
  in	    
    (term_equal _term1 _term2)
    ||
    (
      try
	ignore (do_variants _term1 _term2);
	true
      with
	| Exit ->
	    (*
	    List.iter
	      (fun (x, y) ->
		 print_endline (Symbol.to_string x ^ " - " ^ Symbol.to_string y)
	      )
	      !skolem_mapping;
	    *)
	    false
    )


let are_literals_skolem_variants (literal1: literal) (literal2: literal) : bool =
  (literal1.sign == literal2.sign)
  &&
  (are_terms_skolem_variants literal1.atom literal2.atom)












(* remove variants from a literal set *)
let remove_variants (literals: literal list) =
  (* extend an existing variant mapping to two more terms.
     invariant: variable with lower id are bound to variables with higher id.
     avoids cycles in renaming.
  *)
  let rec are_terms_variants mapping (term1: term) (term2: term) =
    match term1, term2 with
      | Var var1, Var var2 ->
	  let cmp =
	    Var.compare var1 var2
	  in
	    if cmp = 0 then
	      mapping
	    else if cmp < 0 then
	      (var1, var2) :: mapping
	    else
	      (var2, var1) :: mapping
	      
      | Const symbol1, Const symbol2 when
	  Symbol.equal symbol1 symbol2 ->

	  mapping
	    
      | Func func1, Func func2 when
	  Symbol.equal func1.symbol func2.symbol ->
	  
	  Tools.array_fold2 are_terms_variants mapping func1.subterms func2.subterms
	    
      | _ ->
	  raise Exit
  in

  (* find a renaming making all variants identical *)
  let rec find_renaming mapping literals =
    match literals with
      | [] ->
	  failwith "Lemma: find_renaming"
	  
      | _ :: [] ->
	  mapping
	  
      | l1 :: tail ->
	  (* search for a variant for l1 in tail *)
	  begin
	    let rec find_renaming' mapping tail' =
	      match tail' with
		| [] ->
		    (* no variant found *)
		    find_renaming mapping tail
		    
		| l2 :: tail' ->
		    let mapping' =
		      (* ignore constraint literals *)
		      if is_fd_constraint l2 then
			None

		      else
			try
			  Some (are_terms_variants mapping l1.atom l2.atom)
			with
			  | Exit ->
			      None
		    in
		      (* variant found *)
		      match mapping' with
			| Some mapping ->
			    find_renaming mapping tail

			| None ->
			    find_renaming' mapping tail'
	    in
	      find_renaming' mapping tail	    

	  end
  in
  let mapping =
    find_renaming [] literals
  in

  let rec dereference mapping var =
    try
      let var' =
	Var.VarTable.find mapping var
      in
	dereference mapping var'
    with
      | Not_found ->
	  var
  in

    match mapping with
      | [] ->
	  (* no variants *)
	  literals

      | _ ->
	  begin
	    (* apply the renaming *)
	    let mapping' =
	      Var.VarTable.create 61
	    in
	      List.iter
		(fun (var1, var2) ->
		   Var.VarTable.add mapping' var1 var2
		)
		mapping;

	      let mapping =
		Var.VarTable.fold
		  (fun var1 var2 acc ->
		     let var2' =
		       dereference mapping' var2
		     in
		       (request_var var1, request_var var2') :: acc
		  )
		  mapping'
		  []
	      in
		
		(* apply unifier - have to use replace to avoid independent normalization *)
		List.map
		  (fun literal ->
		     replace_terms_in_literal literal mapping
		  )
		  literals
	  end
			     


      
(* remove duplicates *)
let remove_duplicates (literals: literal list) : literal list =
  let seen =
    LiteralTable.create 63
  in
    List.find_all
      (fun literal ->
	 if LiteralTable.mem seen literal then
	   false
	     
	 else begin
	   LiteralTable.add seen literal ();
	   true
	 end
      )
      literals









(* t = t \/ C *)
let is_tautology_1 (clause: clause) : bool =
  List.exists
    (fun literal ->
       literal.sign
       &&
       match literal.atom with
	 | Func func when
	     Symbol.equal Symbol.equality func.symbol
	     &&
	     term_equal func.subterms.(0) func.subterms.(1)
	     ->
	     true

	 | _ ->
	     false
		    
    )
    clause

(* p \/ -p \/ C *)
let is_tautology_2 (clause: clause) : bool =
  List.exists
    (fun literal ->
      List.exists
	(fun literal' ->
	  literal.sign != literal'.sign
	  &&
	  term_equal literal.atom literal'.atom
	)
	clause
    )
    clause

let is_tautology (clause: clause) : bool =
  is_tautology_1 clause
  ||
  is_tautology_2 clause










(*** Ordering ***)


let rec compare_terms x y =
  if term_equal x y then
    0
  else
    match x, y with
	(* var first *)
      | Var u, Var v ->
	  Var.compare u v
	    
      | Var _, _ ->
	  -1
	    
      | _, Var _ ->
	  1

      (* const second *)
      | Const a, Const b ->
	  Symbol.compare a b
	    
      | Const _, _ ->
	  -1
	    
      | _, Const _ ->
	  1
	    
      (* function term last *)
      | Func func1, Func func2 ->
	  let cmp =
	    Symbol.compare func1.symbol func2.symbol
	  in
	    if cmp != 0 then
	      cmp
	    else
	      let rec compare_terms' index =
		if index >= Array.length func1.subterms then
		  0
		else
		  let cmp' =
		    compare_terms func1.subterms.(index) func2.subterms.(index)
		  in
		    if cmp' != 0 then
		      cmp'
		    else
		      compare_terms' (index + 1)
	      in
		compare_terms' 0

	

(* pos before neg *)
let compare_literals x y =
  let cmp =
    compare
      (Symbol.name (top_symbol_literal x))
      (Symbol.name (top_symbol_literal y))
  in
    if cmp <> 0 then
      cmp
    else if x.sign == y.sign then
      compare_terms x.atom y.atom
    else if x.sign then
      -1
    else
      1

let sort_clause clause =
  List.sort compare_literals clause




(*** tptp ***)

(* get the i.th finite domain element in tptp format: "1" *)
let tptp_get_domain_element (i: int) : term =
(*  request_const (Symbol.create_skolem ~name:("\"" ^ string_of_int i ^ "\"") 0)*)
  let rec tptp_get_domain_element' name =
    try
      let e = Symbol.create_skolem ~name:(Some name) () in
	request_const e
    with
      | Symbol.OVERLOADING ->
	  tptp_get_domain_element' ("e" ^ name)
  in

  let name = "e" ^ string_of_int i in
    tptp_get_domain_element' name


let rec tptp_replace_domain_elements (term: term) : term =
  match term with
    | Var _ ->
	term

    | Const const ->
	if Symbol.is_fd_element const then
	  tptp_get_domain_element (int_of_string (Symbol.to_string ~pretty:true const))
	else
	  term

    | Func func ->
	let subterms =
	  Array.map tptp_replace_domain_elements func.subterms
	in
	  request_func (func.symbol, subterms)




(* replaces all variables in a term by constants,
   so that instead of Darwin's _0 the output is a tptp conform X0. *)
let tptp_replace_var (var: var) : term =
  let const =
    Symbol.create_function ("X" ^ string_of_int (Var.id_of_var var)) 0
  in
    request_const const

let rec tptp_replace_vars (term: term) : term =
  match term with
    | Var var ->
	tptp_replace_var var

    | Const _ ->
	term

    | Func func ->
	let subterms =
	  Array.map tptp_replace_vars func.subterms
	in
	  request_func (func.symbol, subterms)

(* tptp can't take symbols starting with '_' *)
let rec tptp_replace_internal_symbols (term: term) : term =
  match term with
    | Var _ ->
	term

    | Const symbol ->
	if (String.get (Symbol.to_string ~pretty:false symbol) 0) = '_' then
	  request_const (
	      Symbol.create_skolem
		~arity:(Symbol.arity symbol)
		~name:(Some ("int" ^ (Symbol.name symbol)))
		()
	  )
	else
	  term

    | Func func ->
	let symbol =
	  if (String.get (Symbol.to_string ~pretty:false func.symbol) 0) = '_' then
	    Symbol.create_skolem
	      ~arity:(Symbol.arity func.symbol)
	      ~name:(Some ("int" ^ (Symbol.name func.symbol)))
	      ()
	  else
	    func.symbol
	in
	let subterms =
	  Array.map tptp_replace_internal_symbols func.subterms
	in
	  request_func (symbol, subterms)



(* replaces a finite domain relation with the original function symbol *)
let tptp_replace_relation (term: term) =
  match term with
    | Func func when Symbol.is_fd_relation func.symbol ->
	let symbol =
	  match func.subterms.(0) with
	    | Const symbol ->
		Symbol.get_symbol_from_fd_symbol symbol
	    | _ ->
		failwith ("Context.tptp_replace_relation : " ^ term_to_string term);
	in
	let result =
	  func.subterms.(Array.length func.subterms - 1)
	in
	let arguments =
	  Array.sub func.subterms 0 (Array.length func.subterms - 2)
	in
	let function_term =
	  request_func (symbol, arguments)
	in
	  request_func (Symbol.equality, [| function_term; result |])

    | _ ->
	term


(* performs all transformations necessary to output a term as a tptp term *)
let to_tptp_term (term: term) : term =
  tptp_replace_relation (tptp_replace_domain_elements (tptp_replace_vars term))

let tptp_clause_to_tptp_string (label: string) (clause: clause) : string =
  let clause =
    List.map
      (fun literal ->
	let sign =
	  if literal.sign then
	    "  "
	  else
	    "~ "
	in
	let term =
	  tptp_replace_internal_symbols (tptp_replace_domain_elements (tptp_replace_vars literal.atom))
	in

	(* have to treat '=' special *)
	let term_repr =
	  match term with
	    | Func func when Symbol.equal Symbol.equality func.symbol ->
		term_to_string ~pretty:false func.subterms.(0)
		^ " = "
		^ term_to_string ~pretty:false  func.subterms.(1)

	    | _ ->
		term_to_string ~pretty:false term
	in
	  sign ^ term_repr ^ "\n"
      )
      clause;
  in
    "cnf(" ^ label ^ ",axiom,(\n"
    ^ "      " ^ String.concat "    | " clause
    ^ ")).\n"
  
