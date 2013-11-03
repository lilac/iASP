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



(*** types ***)

type var = Var.var
type symbol = Symbol.symbol
type term = Term.term
type clause = Term.clause
type counter = Counter.counter


(* used during sort inference, which is based on union-find. *)
type sort = {
  (* a unique id per sort *)
  mutable sort: int;

  (* the current representative of this sorts' equivalence class.
     None, if this is the representative *)
  mutable repr: sort option;

  (* if this is the class representative,
     then this is the number of class elements. *)
  mutable size: int;
}

(* the sort of a function/predicate symbol:

   for an n-ary function symbol f:
   f: S0 -> S1 -> ... Sn -> Sresult

   for an n-ary predicate symbol p:
   p: S0 -> S1 -> ... Sn -> BOOL
*)
type composed_sort = sort array

module SymbolTable = Symbol.SymbolTable
type symbol_sorts = composed_sort SymbolTable.t
module VarTable = Var.VarTable
type var_sorts = composed_sort VarTable.t

type sorts = {
  (* mapping from a symbol to its sort *)
  symbol_sorts: symbol_sorts;

  (* the sorts, i.e. the normalized class representatives
     after sort inference *)
  sorts: sort list;

  (* total number of sorts *)
  number_of_sorts: int;

  (* constant symbols of the signature *)
  mutable constants: symbol list;
  (* function symbols of the signature *)
  functions: symbol list;
  (* predicate symbols of the signature *)
  predicates: symbol list;

  (* constant symbols partitioned by sorts *)
  mutable constants_partition: symbol list array;
  (* size of biggest partition in constants_partition *)
  mutable max_constant_partition_size: int;

  (* like constants_partition indexed by sort.sort.
     contains a unary function with result sort.sort
     if there is such a function, None otherwise. *)
  unary_functions: (symbol option) array;

  (* permutation symbol -> represented sort *)
  perm_symbols: sort SymbolTable.t;

  mutable cliques: (symbol list * int) array;
  mutable max_clique_size: int;
}

module SortTable =
  Hashtbl.Make (
    struct
      type t = sort

      let equal = (==)
      let hash sort = sort.sort
    end
  )


(* literal ill-sorted as it generalizes over different symbols,
   e.g. as in the totality axioms r_1(x, y, 1) \/ r_1(x, y, 2).
   should only happen for finite domain axioms and lemmas *)
exception RELATIONAL_FUNCTION

(*** accessors ***)

let var_sort = {
  sort = 0;
  repr = None;
  size = 1;
}

let get_sorts (sorts: sorts) : sort list =
  sorts.sorts

let constants_partition (sorts: sorts) =
  sorts.constants_partition

let max_constant_partition_size (sorts: sorts) : int =
  sorts.max_constant_partition_size

let get_constants_for_sort (sorts: sorts) (sort: sort) : symbol list =
  sorts.constants_partition.(sort.sort)

let get_unary_function_for_sort (sorts: sorts) (sort: sort) : symbol option =
  sorts.unary_functions.(sort.sort)




let sort_equal x y =
  x.sort == y.sort

let sort_to_string sort =
  "'" ^ string_of_int sort.sort



(*** union-find ***)

(* get equivalence class of a sort *)
let rec get_repr (sort: sort) : sort =
  match sort.repr with
    | None ->
	sort

    | Some repr ->
	begin
	  let repr' =
	    get_repr repr
	  in
	    (* path compression *)
	    if repr' != repr then
	      sort.repr <- Some repr';
		
	    repr'
	end


(* unify equivalence classes *)
let unify (a: sort) (b: sort) : unit =
  let a = get_repr a
  and b = get_repr b
  in
    if a == b then
      ()

    (* add bigger equivalence class to smaller *)
    else if a.size >= b.size then begin
      a.repr <- Some b;
      b.size <- a.size + b.size;
    end

    else begin
      b.repr <- Some a;
      a.size <- a.size + b.size;
    end







(*** register sorts ***)


(* retrieves/creates the sort of a symbol *)
let register_symbol (sorts: symbol_sorts) (function_occurrences: int SymbolTable.t)
    (sort_counter: counter) (symbol: symbol) : composed_sort =
  if Symbol.is_function symbol && Symbol.arity symbol == 1 then begin
    let count =
      try
	SymbolTable.find function_occurrences symbol
      with
	| Not_found ->
	    0
    in
      SymbolTable.replace function_occurrences symbol (count + 1)
  end;

  try
    SymbolTable.find sorts symbol
  with
    | Not_found ->
	(* function symbols have a result sort, predicate symbols don't *)
	let size =
	  if Symbol.is_predicate symbol then
	    Symbol.arity symbol
	  else
	    Symbol.arity symbol + 1
	in
	let composed_sort =
	  Array.make size { sort = -1; repr = None; size = 1 }
	in
	  for i = 0 to Array.length composed_sort - 1 do
	    composed_sort.(i) <- { sort = Counter.next sort_counter; repr = None; size = 1 };
	  done;
	  SymbolTable.add sorts symbol composed_sort;

	  composed_sort

(* retrieves/creates the sort of a variable *)
let register_var (var_sorts: var_sorts) (sort_counter: counter) (var: var) : composed_sort =
  try
    VarTable.find var_sorts var
  with
    | Not_found ->
	let composed_sort =
	  [| { sort = Counter.next sort_counter; repr = None; size = 1 } |]
	in
	  VarTable.add var_sorts var composed_sort;

	  composed_sort

(* retrieves/creates the sort of the top symbol of a term *)
let register_term (sorts: symbol_sorts) (var_sorts: var_sorts) (function_occurrences: int SymbolTable.t)
    (sort_counter: counter) (term: term) : composed_sort =
  match term with
    | Term.Var var ->
	register_var var_sorts sort_counter var

    | Term.Const symbol
    | Term.Func { Term.symbol = symbol } ->
	register_symbol sorts function_occurrences sort_counter symbol


(* returns the result sort of a non-constant function symbol *)
let result_sort (composed_sort: composed_sort) : sort =
  if Array.length composed_sort == 0 then begin
    failwith ("Sort_inference: result_sort: ");
  end;
  composed_sort.(Array.length composed_sort - 1)


(* sorts sorts by arity and name *)
let sort_symbols (s1: symbol) (s2: symbol) =
  let cmp =
    compare (Symbol.arity s1) (Symbol.arity s2)
  in
    if cmp <> 0 then
      cmp
    else begin
      (* put skolem symbols last *)
      if Symbol.is_skolem s1 && not (Symbol.is_skolem s2) then
        1
      else if not (Symbol.is_skolem s1) && Symbol.is_skolem s2 then
	-1
      else
	compare (Symbol.name s1) (Symbol.name s2)
    end



(*** print ***)

let sort_to_string (sort: sort) : string =
  "'" ^ (string_of_int ((get_repr sort).sort))

let composed_sort_to_string (composed_sort) : string =
  String.concat " -> " (Array.to_list (Array.map sort_to_string composed_sort))


let print_symbols (sorts: sorts) (symbols: symbol list)  (title: string) : unit =
  print_endline (title ^ ": ");
  List.iter
    (fun symbol ->
       try
	 let composed_sort =
	   SymbolTable.find sorts.symbol_sorts symbol
	 in
	   print_endline (Symbol.name symbol ^ " : " ^ composed_sort_to_string composed_sort)
       with
	 | Not_found ->
	     failwith "Sort_inference.print_symbols"
    )
    symbols;
  print_newline ()

let print_sorts (sorts: sorts) : unit =
  print_endline ("Sorts: " ^ string_of_int sorts.number_of_sorts);
  print_newline ();

  print_symbols sorts sorts.constants "Constants";
  print_symbols sorts sorts.functions "Functions";
  print_symbols sorts sorts.predicates "Predicates"


let print = print_sorts





(*** isomporhism ***)

let get_permutable sort : symbol =
  Symbol.create_fd_permutable (get_repr sort).sort

let exists_permutable sorts sort =
  (* without static symmetry reduction any element is permutable *)
  if not Const.fd_static_symmetry_reduction then
    true

  else begin
    (* static symmetry on unary functions and a unary function of this sort? *)
    let exists_unary_function =
      Const.fd_static_symmetry_reduction_unary
      &&
      get_unary_function_for_sort sorts sort != None
    in
      (* yes, so all elements will be used in symmetry reduction axioms *)
      if exists_unary_function then
	false
	  
      else
	true
  end

let get_min_permutable sorts sort =
  (* all permutable *)
  if not Const.fd_static_symmetry_reduction then
    1

  (* just check that the element comes after the constants,
     as the first domain elements will be used
     for static symmetry reduction on constants. *)
  else if exists_permutable sorts sort then
    List.length (get_constants_for_sort sorts sort) + 1

  (* none at all *)
  else
    raise Not_found


let is_permutable_ sorts sort element =
  exists_permutable sorts sort
  && 
  element >= get_min_permutable sorts sort


let is_permutable sorts sort symbol =
  if not (Symbol.is_fd_element symbol) then
    failwith "Sort_inference.is_permutable";

  let element =
    int_of_string (Symbol.to_string ~pretty:true symbol)
  in
    is_permutable_ sorts sort element




(*** clique ***)

(* find the max clique of disequalities between constants of  a sort *)
let rec find_max_clique
    (* as computed by compute_connections *)
    (connections: (symbol list ref) SymbolTable.t)
    (* the largest currently  known clique and its size *)
    (max_clique: symbol list) (max_clique_size: int)
    (* the current partially and still to be extended clique and its size *)
    (clique: symbol list) (clique_size: int)
    (* the constants of this sort not already in the clique *)
    (symbols: symbol list) (symbols_size: int)
    : (symbol list * int) =
    match symbols with
      | [] ->
	  if clique_size < max_clique_size then
	    (max_clique, max_clique_size)
	  else
	    (clique, clique_size)

      | head :: tail ->
	  (* the current clique can not be extended to one
	     bigger than a previously found one. *)
	  if clique_size + symbols_size <= max_clique_size then
	    (max_clique, max_clique_size)

	  else begin
	    let connected_to =
	      try
		!(SymbolTable.find connections head)
	      with
		| Not_found ->
		    []
	    in
	      (* this symbol is not connected to more symbols
		 than a the size of an already found clique,
		 so ignore it *)
	      if List.length connected_to < max_clique_size then
		(max_clique, max_clique_size)

	      else begin
		let max_clique, max_clique_size =
		  (* try to extend the clique with this symbol *)
		  if (
		    List.for_all
		      (fun symbol ->
			 List.exists
			   (fun symbol' -> Symbol.equal symbol symbol')
			   connected_to
		      )
		      clique
		  )
		  then
		    find_max_clique
		      connections
		      max_clique max_clique_size
		      (head :: clique) (clique_size + 1)
		      tail (symbols_size - 1)
		      
		  (* if this symbol is not connected to all symbols in the current clique
		     it can not be used to extend this clique. *)
		  else
		    max_clique, max_clique_size
		in
		  (* try to extend the clique, but not with this symbol *)
		  find_max_clique
		    connections
		    max_clique max_clique_size
		    clique clique_size
		    tail (symbols_size - 1)
	      end
	  end



(* for each sort there is a hash table from a constant symbol
   to a list of constant symbols (the constant's links),
   and if a != b occurs in clauses,
   then b is added to the links of a, and vice versa. *)
let compute_connections sorts clauses : (((symbol list) ref) SymbolTable.t) option array =
  let sort_connections =
    Array.make (sorts.number_of_sorts + 1) None
  in
    (* find all connections *)
    List.iter
      (fun clause ->
	List.iter
	  (fun literal ->
	    if not literal.Term.sign then begin
		match literal.Term.atom with
		  | Term.Func func when
			Symbol.equal Symbol.equality func.Term.symbol ->
		      begin
			match func.Term.subterms.(0), func.Term.subterms.(1) with
			  | Term.Const a, Term.Const b ->
			      (* found a != b *)
			      let composed_sort =
				try
				  SymbolTable.find sorts.symbol_sorts a
				with
				  | Not_found ->
				      failwith ("Sort_inference.compute_clique: " ^ Term.literal_to_string literal)
			      in
			      let sort =
				get_repr (result_sort composed_sort)
			      in


			      let connections : (symbol list ref) SymbolTable.t =
				match sort_connections.(sort.sort) with
				  | None ->
				      let connections =
					SymbolTable.create 512
				      in
					sort_connections.(sort.sort) <- Some connections;
					connections
				  | Some connections ->
				      connections
			      in
			      let links =
				try
				  SymbolTable.find connections a
				with
				  | Not_found ->
				      let links =
					ref []
				      in
					SymbolTable.add connections a links;
					links
			      in
				links := b :: !links;

			      let links =
				try
				  SymbolTable.find connections b
				with
				  | Not_found ->
				      let links =
					ref []
				      in
					SymbolTable.add connections b links;
					links
			      in
				links := a :: !links;



			  | _ -> ()
		      end

		  | _ -> ()
	      end
	  )
	  clause
      )
      clauses;

    sort_connections



let compute_clique sorts una clauses : unit =
  let sort_connections =
    compute_connections sorts clauses
  in
  let max_clique_size =
    ref 0
  in
    (* find the max clique for each sort *)
    Array.iteri
      (fun sort_id connections ->
	 let constants =
	   sorts.constants_partition.(sort_id)
	 in
	 let clique, size =
	   if una then
	     constants, (List.length constants)
	   else
	     match connections with
	       | None ->
		   [], 0
		     
	       | Some connections ->
		   find_max_clique
		     connections
		     [] 0
		     [] 0
		     constants (List.length constants)
	 in
	   if size >= 2 then begin
	     (*print_endline ("CLIQUE: " ^ String.concat ", " (List.map Symbol.to_string clique));*)
	     max_clique_size := max !max_clique_size size;
	     sorts.cliques.(sort_id) <- (List.sort sort_symbols clique, size);
	   end
      )
      sort_connections;

    (* the size of the max clique over all sorts *)
    sorts.max_clique_size <- !max_clique_size;

    (* extract the max cliques for each sort *)
    Array.iteri
      (fun sort_id constants ->
	let clique, clique_size =
	  sorts.cliques.(sort_id)
	in
	  if clique_size > 1 then begin
	      let constants' =
		if List.length constants = clique_size then
		  clique
		else
		  clique @ (
		    List.fold_left
		      (fun acc constant ->
			if List.exists (fun constant' -> Symbol.equal constant constant') clique then
			  acc
			else
			  constant :: acc
		      )
		      []
		      (List.rev constants)
		  )
	      in
		sorts.constants_partition.(sort_id) <- constants';
	    end;
      )
      sorts.constants_partition



let get_max_clique (sorts: sorts) : int =
  sorts.max_clique_size

let get_max_clique_for_sort (sorts: sorts) sort : int =
  snd (sorts.cliques.(sort.sort))




(*** infer ***)


(* partition the constant symbols into their sort equivalence classes,
   and return the size of the biggest partition *)
let partition_constants symbol_sorts constants number_of_sorts : (symbol list) array * int =
  let constants_partition =
    Array.make (number_of_sorts + 1) []
  in
    List.iter
      (fun symbol ->
	try
	  let composed_sort =
	    SymbolTable.find symbol_sorts symbol
	  in
	     let sort =
	       get_repr (result_sort composed_sort)
	     in
	       if sort.sort < 0 || sort.sort >= Array.length constants_partition then
		 failwith ("Sort_inference.partition_constants: sort out of bound: " ^ string_of_int sort.sort);
	       
	       constants_partition.(sort.sort) <- symbol :: constants_partition.(sort.sort)
	with
	  | Not_found ->
	      failwith "Sort_inference.partition_constants: 2"
      )
      constants;
    
      (* sort the symbols of each partition *)
      for i = 0 to Array.length constants_partition - 1 do
	constants_partition.(i) <- List.sort sort_symbols constants_partition.(i);
      done;

      (* put domains with few constants first -
	 doesn't work with e.g. get_constants_for_sort *)
(*      Array.sort
	(fun x y ->
	   compare (List.length x) (List.length y)
	)
	constants_partition;*)

      let max_size =
	Array.fold_left
	  (fun acc symbols ->
	    Tools.max_int (List.length symbols) acc
	  )
	  0
	  constants_partition
      in
	constants_partition, max_size


let find_unary_functions symbol_sorts functions function_occurrences number_of_sorts : symbol option array =
  let unary_functions =
    Array.make (number_of_sorts + 1) None
  in
    List.iter
      (fun symbol ->
	if Symbol.arity symbol == 1 then begin
	    try
	      let composed_sort =
		SymbolTable.find symbol_sorts symbol
	      in
	      let sort =
		get_repr (result_sort composed_sort)
	      in
		if sort.sort < 0 || sort.sort >= Array.length unary_functions then
		  failwith ("Sort_inference.unary_functions: sort out of bound: " ^ string_of_int sort.sort);
	       
		match unary_functions.(sort.sort) with
		  | None ->
		      unary_functions.(sort.sort) <- Some symbol

		  | Some old_symbol ->
		      let old_counter =
			try
			  SymbolTable.find function_occurrences old_symbol
			with
			  | Not_found ->
			      failwith "Sort_inference.find_unary_functions 1"
		      in
		      let new_counter =
			try
			  SymbolTable.find function_occurrences symbol
			with
			  | Not_found ->
			      failwith "Sort_inference.find_unary_functions 2"
		      in
			if old_counter < new_counter then
			  unary_functions.(sort.sort) <- Some symbol
	    with
	      | Not_found ->
		  failwith "Sort_inference.unary_functions: 3"
	  end
      )
      functions;

    unary_functions


(* infer sorts for a term.
   - [sort] is the sort of the term position at which term occurs,
     i.e. the term's result sort must be the same as [sort] *)
let rec infer_term (sorts: symbol_sorts) (var_sorts: var_sorts) 
    (function_occurrences: int SymbolTable.t)
    (sort_counter: counter) (sort: sort) (term: term) : unit =
  match term with
    | Term.Var var ->
	unify sort (result_sort (register_var var_sorts sort_counter var))

    | Term.Const symbol ->
	unify sort (result_sort (register_symbol sorts function_occurrences sort_counter symbol))

    | Term.Func func ->
	let composed_sort =
	  register_symbol sorts function_occurrences sort_counter func.Term.symbol
	in
	  unify sort (result_sort composed_sort);
	  (* register all subterms, and infer their sorts *)
	  Array.iteri
	    (fun i term ->
	       infer_term sorts var_sorts function_occurrences sort_counter composed_sort.(i) term
	    )
	    func.Term.subterms
	
	

(* infer sorts for a clause *)
let infer_clause (sorts: symbol_sorts) (function_occurrences: int SymbolTable.t)
    (sort_counter: counter) (clause: clause) : unit =
  (* variable sorts are local to a clause *)
  let var_sorts =
    VarTable.create 64
  in
    List.iter
      (fun literal ->
	 match literal.Term.atom with
	   | Term.Var _ ->
	       failwith ("Sort_inference: infer_clause on variable: " ^ Term.literal_to_string literal);

	   | Term.Const symbol ->
	       (* predicate with arity 0 - nothing to infer *)
	       ignore (register_symbol sorts function_occurrences sort_counter symbol : composed_sort);

	   | Term.Func func ->
	       (* equality predicate *)
	       if
		 Symbol.equal Symbol.equality func.Term.symbol
		 ||
		 Symbol.equal Symbol.diff func.Term.symbol
	       then begin
		 (* register both subterms, unify their result sort, and infer their sorts *)
		 let left_sort = register_term sorts var_sorts function_occurrences
		   sort_counter func.Term.subterms.(0)
		 and right_sort = register_term sorts var_sorts function_occurrences
		   sort_counter func.Term.subterms.(1)
		 in
		   unify (result_sort left_sort) (result_sort right_sort);
		   infer_term sorts var_sorts function_occurrences 
		     sort_counter (result_sort left_sort) func.Term.subterms.(0);
		   infer_term sorts var_sorts function_occurrences
		     sort_counter (result_sort right_sort) func.Term.subterms.(1);
	       end

	       (* relational function of flattened term *)
	       else if Symbol.is_fd_relation func.Term.symbol then begin
		 match func.Term.subterms.(0) with
		   | Term.Var _ ->
		       raise RELATIONAL_FUNCTION;
		       (*failwith ("Sort_inference.infer_clause: Generalized Relation: " ^ Term.literal_to_string literal)*)
			 
		   | Term.Const symbol -> 
		       let symbol =
			 Symbol.get_symbol_from_fd_symbol symbol
		       in
			 (* predicate with arity 0 - nothing to infer *)
			 if Symbol.arity symbol == 0 then begin
			   let composed_sort =
			     register_symbol sorts function_occurrences sort_counter symbol
			   in
			     infer_term sorts var_sorts function_occurrences
			       sort_counter composed_sort.(0) func.Term.subterms.(1)
			 end
			     
			 else begin
			   let composed_sort =
			     register_symbol sorts function_occurrences sort_counter symbol
			   in
			     for i = 1 to Array.length func.Term.subterms - 1 do
			       infer_term sorts var_sorts function_occurrences
				 sort_counter composed_sort.(i - 1) func.Term.subterms.(i)
			     done
			 end
			   
		   | Term.Func _ ->
		       failwith ("Sort_inference.infer_clause: non-flat functional relation: " ^ Term.literal_to_string literal)
	       end

	       (* predicate with arguments *)
	       else begin
		 let composed_sort =
		   register_symbol sorts function_occurrences sort_counter func.Term.symbol
		 in
		   (* register all subterms, and infer their sorts *)
		   Array.iteri
		     (fun i term ->
			infer_term sorts var_sorts function_occurrences
			  sort_counter composed_sort.(i) term
		     )
		     func.Term.subterms
	       end
      )
      clause


(* infer sorts for clauses *)
let infer ~(print:bool) (clauses: clause list) : sorts =
  let symbol_sorts =
    SymbolTable.create 256
  in
  let function_occurrences =
    SymbolTable.create 256
  in
  (* first used id is 1, as 0 is used for unsorted variables *)
  let sort_counter =
    Counter.create_with 0
  in
    (* infer the sorts for each symbol *)
    List.iter
      (fun clause -> infer_clause symbol_sorts function_occurrences sort_counter clause)
      clauses;

    (* for each found symbol, check if it is a constant, function, or predicate.
       also check which sorts are left after unifying sorts. *)
    let constants, functions, predicates, used_sorts =
      SymbolTable.fold
	(fun symbol composed_sort (constants, functions, predicates, used_sorts) ->
	   (* add sorts used by this symbol, which did not occur before *)
	   let used_sorts =
	     Array.fold_left
	       (fun acc sort ->
		 Tools.list_add (==) acc (get_repr sort)
	       )
	       used_sorts
	       composed_sort
	   in
	     (* a predicate symbol *)
	     if Symbol.is_predicate symbol then
	       (constants, functions, symbol :: predicates, used_sorts)
		 
	     (* a function symbol *)
	     else if Array.length composed_sort > 1 then
	       (constants, symbol :: functions, predicates, used_sorts)

	     (* a constant symbol *)
	     else
	       (symbol :: constants, functions, predicates, used_sorts)
	)
	symbol_sorts
	([], [], [], [])
    in

    (* normalize used sorts to use the numbers from 1 .. number_of_used_sorts *)
    let rec normalize counter used_sorts =
      match used_sorts with
	| [] ->
	    ()

	| head :: tail ->
	    head.sort <- counter;
	    normalize (counter + 1) tail
    in
      normalize 1 used_sorts;

    (* sort the symbols *)
    let constants = List.sort sort_symbols constants
    and functions = List.sort sort_symbols functions
    and predicates = List.sort sort_symbols predicates
    and number_of_sorts = List.length used_sorts
    in

    let constants_partition, max_size =
      partition_constants symbol_sorts constants number_of_sorts
    in
    let unary_functions =
      find_unary_functions symbol_sorts functions function_occurrences number_of_sorts
    in

    let perm_symbols =
      SymbolTable.create number_of_sorts
    in
      List.iter
	(fun sort ->
	   SymbolTable.add perm_symbols (get_permutable sort) sort
	)
	used_sorts;

    let sorts = {
      symbol_sorts = symbol_sorts;
      number_of_sorts = number_of_sorts;
      sorts = used_sorts;

      constants = constants;
      functions = functions;
      predicates = predicates;

      constants_partition = constants_partition;
      max_constant_partition_size = max_size;

      unary_functions = unary_functions;
      perm_symbols = perm_symbols;

      cliques = Array.make (number_of_sorts + 1) ([], 0);
      max_clique_size = 0;
    }
    in
(*      if Const.fd_compute_cliques then
	compute_clique sorts clauses;*)
     
      if print then begin
	print_sorts sorts
      end;

      sorts



let add_constant (sorts: sorts) (constant: symbol) (existing: symbol) : unit =
  if SymbolTable.mem sorts.symbol_sorts constant then
    failwith ("Sort_inference.add_constant: symbol already known: " ^ Symbol.to_string constant);

  let sort =
    try
      result_sort (SymbolTable.find sorts.symbol_sorts existing)
    with
      | Not_found ->
	  failwith ("Sort_inference.add_constant: symbol is unkown: " ^ Symbol.to_string existing);
  in 
  let composed_sort =
    Array.make 1 { sort = -1; repr = Some sort; size = 1 }
  in
    SymbolTable.add sorts.symbol_sorts constant composed_sort;

    let constants =
      List.sort sort_symbols (constant :: sorts.constants)
    in
    let constants_partition, max_size =
      partition_constants sorts.symbol_sorts constants sorts.number_of_sorts
    in
      sorts.constants <- constants;
      sorts.constants_partition <- constants_partition;
      sorts.max_constant_partition_size <- max_size


let get_argument_sort (sorts: sorts) (symbol: symbol) (index: int) : sort =
  if Symbol.is_fd_permutable symbol then begin
    try
      SymbolTable.find sorts.perm_symbols symbol
    with
      | Not_found ->
	  failwith ("Sort_inference.get_argument_sort: unknown permutable: "
		    ^ Symbol.to_string symbol);
  end

  else begin
    let composed_sort =
      try
	SymbolTable.find sorts.symbol_sorts symbol
      with
	| Not_found ->
	    failwith ("Sort_inference.get_argument_sort: unknown symbol: "
		      ^ Symbol.to_string symbol)
    in

    if Const.debug && index < 0 then
      failwith ("Sort_inference.get_argument_sort: index < 0");

    if Const.debug && index >= Array.length composed_sort then
      failwith ("Sort_inference.get_argument_sort: index too large: "
		^ Symbol.to_string symbol ^ " at " ^ string_of_int index);

    get_repr composed_sort.(index)
  end






(* for a predicate, return the predicate symbol,
   for a 'relational function', return the 'relationalized' function symbol *)
let get_function_symbol_of_func func =
  if Symbol.is_fd_relation func.Term.symbol then
    match func.Term.subterms.(0) with
      | Term.Const symbol ->
	  Symbol.get_symbol_from_fd_symbol symbol
	    
      | _ ->
	  failwith ("Sort_inference.get_function_symbol_of_func: generalized relation: "
		    ^ Term.term_to_string (Term.request_func (func.Term.symbol, func.Term.subterms)))
  else
    func.Term.symbol


exception Found of sort

(* raises Found sort, if sort can be determined *)
let get_var_sort sorts clause var : sort option =
  try
    (* equivalence classes over variables, till the sort of var is determined. *)
    let equ_classes =
      VarTable.create 32
    in
    let rec get_repr var =
      try
	let var' =
	  VarTable.find equ_classes var
	in
	  get_repr var'
      with
	| Not_found ->
	    var
    in

    (* mapping from var to its sort *)
    let var_sorts =
      VarTable.create 32
    in
    let set var' sort =
      let var' =
	get_repr var'
      in
      if not Const.debug then begin
	if Var.equal var var' then
	  raise (Found sort);
      end;
      
      try
	let sort' =
	  VarTable.find var_sorts var'
	in
	  if Const.debug && sort != sort' then begin
	    print_endline (Term.clause_to_string clause);
	    print_endline (Var.to_string var');
	    print_endline (sort_to_string sort);
	    print_endline (sort_to_string sort');
	    failwith "Sort_inference.get_sort_of_var.set";
	  end
      with
	| Not_found ->
	    VarTable.add var_sorts var' sort
    in
      
    let merge var0 var1 =
      let var0, var1 =
	get_repr var0, get_repr var1
      in

	if not (Var.equal var0 var1) then begin
	  (* order by id, larger points to smaller,
	     but also var smaller than anything else *)
	  let var0, var1 =
	    if Var.equal var0 var then
	      var0, var1
	    else if Var.equal var1 var then
	      var1, var0
	    else if Var.id_of_var var0 < Var.id_of_var var1 then
	      var0, var1
	    else
	      var1, var0
	  in
	    (* merge equivalence classes *)
	    VarTable.add equ_classes var1 var0;
	    
	    (* set sort of merged class if known *)
	    try
	      let sort =
		set var0 (VarTable.find var_sorts var1)
	      in
		sort
	    with
	      | Not_found ->
		  ()	  
	end
    in
      
      List.iter
	(fun literal ->
	   match literal.Term.atom with
	     | Term.Var _ -> failwith "Sort_inference.get_sort_of_var: var"
	     | Term.Const _ -> ()
	     | Term.Func func ->
		 if
		   Symbol.equal Symbol.equality func.Term.symbol
		   ||
		   Symbol.equal Symbol.diff func.Term.symbol
		 then begin
		   match func.Term.subterms.(0), func.Term.subterms.(1) with
		     | Term.Var var0, Term.Var var1 ->
			 (* merge equivalence classes *)
			 merge var0 var1
			   
		     | _ ->
			 (* ignore equalities between variables and domain elements *)
			 ()		       
		 end
		   
		 else begin
		   let function_symbol =
		     get_function_symbol_of_func func
		   in
		     Array.iteri
		       (fun i subterm ->
			  match subterm with
			    | Term.Var var' ->
				let sort =
				  (* function symbol disguised as relation symbol *)
				  if not (Symbol.equal function_symbol func.Term.symbol) then
				    get_argument_sort sorts function_symbol (i - 1)
				  (* relation symbol *)
				  else
				    get_argument_sort sorts function_symbol i
				in
				  set var' sort
				    
			    | Term.Const _ -> ()
			    | Term.Func _ -> failwith "Sort_inference.get_sort_of_var: func"
		       )
		       func.Term.subterms
		 end
	)
	clause;

      if Const.debug then begin
	try
	  Some (VarTable.find var_sorts var)
	with
	  | Not_found ->
	      None
      end

      else
	None
  with
    | Found sort ->
	Some sort
