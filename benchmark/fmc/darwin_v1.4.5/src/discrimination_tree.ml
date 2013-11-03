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

(* a basic implementation of a discrimination tree term indexing tree.
   Reference: Peter Graf, Term Indexing, Springer-Verlag, New York, NY, 1995

   Copyright (C) 2004 John Wheeler 
   
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)

(* This implementation is based on code found in the file
   dtIndex.ml contained in the source code for KRHyper, copyright
   2003, by Universitaet Koblenz-Landau.  The file dtIndex.ml
   included the following copyright notice:

   * Copyright (C) 2003 Universitaet Koblenz-Landau
   * 
   * This file is part of KRHyper.
   * 
   * KRHyper is free software; you can redistribute it and/or modify it under the
   * terms of the GNU General Public License as published by the Free Software
   * Foundation; either version 2 of the License, or (at your option) any later
   * version.
   * 
   * KRHyper is distributed in the hope that it will be useful, but WITHOUT ANY
   * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   * details.
   * 
   * You should have received a copy of the GNU General Public License along with
   * KRHyper; if not, write to the Free Software Foundation, Inc., 59 Temple
   * Place, Suite 330, Boston, MA 02111-1307 USA
   * 
   * Contributor(s): Christoph Wernhard <wernhard@uni-koblenz.de>.
*)

(* The current author (John Wheer) has adapted the basic implementation of discrimination tree
   term indexing found in the dtIndex.ml file for use in the darwin program and 
   extended the implementation to handle both universal and paremetric variables.  
   This adaptation and extension retained the basic outlines of implementing functions, 
   but rewrote both the call interfaces to match the darwin term indexing 
   interface (see term_index.ml* for details) and the code that handles the term and
   data items that are stored in the darwin term index.  Approximately 70% 
   of the code in the file dtIndex.ml has been modified or rewritten and approximately 
   300 lines of code of additional code were created to implement extensions to 
   address universal and parametric variables.
*)

(* History:
   the code was originally taken from KRHyper by Christoph Wernhard,
   then integrated into Darwin by John Wheer,
   and then modifyied by Alexander Fuchs:
   transformation into a class, coding style harmonization,
   some performance enhancements, mostly compression of tails,
   implementation of the productivity check
*)




(*** types ***)

type symbol = Symbol.symbol
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type 'a data = 'a Term_indexing.data
type 'a iterator = 'a Term_indexing.iterator

exception ITERATOR_EMPTY = Term_indexing.ITERATOR_EMPTY

(* abstraction of term position within the discrimination tree. *)
type hkey =
  | Variable 
  | Parameter
  | Symbol of symbol 

(* a flattened term, i.a. the term representation within the discrimination tree. *)
type fterm =
    hkey list







(*** hkey functions ***)


let hkey_equal (k1: hkey) (k2: hkey) : bool =
  match k1, k2 with
    | Variable, Variable ->
	true
    | Parameter, Parameter ->
	true
    | Symbol s1, Symbol s2 ->
	Symbol.equal s1 s2
    | _ ->
	false
	  
let hkey_hash (k1: hkey) : int =
  match k1 with
    | Variable ->
	0
    | Parameter ->
	1
    | Symbol s ->
	2 + Symbol.id s


(* mapping from hkeys *)
module Table =
  Hashtbl.Make (
    struct
      type t = hkey
      let equal = hkey_equal
      let hash = hkey_hash
    end
  )


(* stored entry, term and data *)
type 'data entry = {
  term: term;
  data: 'data;
}

(* leaf node - containing data *)      
type 'data leaf = {
  (* all entries abstracting to the same fterm *)
  mutable entries: 'data entry list;

  (* abstraction of the term entries,
     used as a prefilter for productivity related tests. *)
  fterm: fterm;
}

(* splitting towards subnodes via their hkey. *)
type 'data inner_node = {
  mutable next: 'data node Table.t;
}

(* encapsulates the prefix towards a subnode *)
and 'data node = {
  (* the common prefix of all subnodes of variant *)
  mutable prefix: fterm;

  (* the subnode *)
  mutable variant: 'data variant;
}

(* a tree node or a leaf *)
and 'data variant =
  | DtInnerNode of 'data inner_node
  | DtLeafNode of 'data leaf

      
(* a discrimination tree *)
type 'data tree = {
  (* the discrimination tree *)
  mutable root: 'data node;
  
  (* its size *)
  mutable size: int;

  (* ugly: this simulates raise (FOUND 'data), see exception FOUND *)
  mutable found: 'data option;
}


(* a check function done on a leaf node, see function create_leaf_check *)
type ('data, 'a) leaf_check = 'data leaf -> 'a list -> 'a list



(*** functions ***)


(*** representation ***)
let hkey_to_string hkey : string =
  match hkey with
      Symbol sym ->
	(Symbol.name sym) ^ "/" ^ (string_of_int (Symbol.arity sym))
    | Variable ->
	"*U/0"
    | Parameter ->
	"*P/0"


let rec fterm_to_string x : string =
    match x with
      | [] ->
	  ""

      | h :: t ->
	  hkey_to_string h ^ " " ^  fterm_to_string t


let leaf_to_string (leaf: 'data leaf) (depth: int) : string =
  let depth_prefix =
    String.make (2 * depth) ' ' ^ string_of_int depth ^ ": "
  in
    String.concat ""
      (List.map
	 (fun entry ->
	    depth_prefix ^ Term.term_to_string entry.term ^ "\n"
	 )
	 leaf.entries
      )

let rec node_to_string (node: 'data node) (depth: int) : string = 
  let depth_prefix =
    String.make (2 * depth) ' ' ^ string_of_int depth ^ ": "
  in
    depth_prefix ^ 
    "prefix : " ^ fterm_to_string node.prefix ^ "\n" ^
  match node.variant with
    | DtInnerNode table ->
	Table.fold
	  (fun hkey node acc ->
	     depth_prefix ^ 
	       "hkey : " ^ (hkey_to_string hkey) ^ "\n" ^
	       (node_to_string node (depth + 1)) ^ 
	       acc
	  )
	  table.next
	  ""

    | DtLeafNode leaf ->
	leaf_to_string leaf depth


let tree_to_string (tree: 'data tree) : string =
  node_to_string tree.root 0


	


(*** creation ***)

let create_inner_node () = {
  next = Table.create 8;
}

let create_node () = {
  prefix = [];
  variant = DtInnerNode (create_inner_node ());
}

let create_entry (term: term) (data: 'data) : 'data entry =
  {
    term = term;
    data = data;
  }

let create_tree () = {
  root = create_node ();
  size = 0;
  found = None;
}


let clear (tree: 'data tree) : unit =
  tree.root <- create_node ();
  tree.size <- 0


(*** data access ***)
let get_size (tree: 'data tree) : int =
  tree.size

let inc_size (tree: 'data tree) : unit =
  tree.size <- tree.size + 1

let dec_size (tree: 'data tree) : unit =
  tree.size <- tree.size - 1



(*** hkey ***)

let hkey_arity (hkey: hkey) : int =
  match hkey with
    | Symbol s ->
	Symbol.arity s
    | _ ->
	0

let term_hkey (term: Term.term ): hkey =
  match term with
    | Term.Var var ->
	if Var.is_universal var then
	  Variable
	else
	  Parameter
    | Term.Const const ->
	Symbol const
    | Term.Func func(*(symbol, _, _)*) ->
	Symbol func.Term.symbol






(*** flatt terms *)

let flatten_term (_term: term) : fterm =

  let rec flatten_term' (term: term) (acc: fterm) : fterm =
    match term with
      | Term.Var var ->
	  if Var.is_universal var then
	    Variable :: acc
	  else
	    Parameter :: acc
	      
	| Term.Const symbol ->
	    Symbol symbol :: acc

	| Term.Func func(*(symbol, terms, _)*) ->
	    (* flatten subterms from right to left *)
	    let new_acc =
	      Array.fold_right
		(fun term acc ->
		   flatten_term' term acc
		)
		func.Term.subterms
		acc
	    in
	      Symbol func.Term.symbol :: new_acc
  in
  let flattened =
    flatten_term' _term []
  in
    match flattened with
      | _propositional_atom :: [] ->
	  (* a propositional predicate, keep it -
	     though this term index is not the adequate data structure... *)
	  flattened

      | _ :: tail ->
	  (* as each index is for one predicate symbol only,
	     we can just drop this (first order) symbol from all terms *)
	  tail

      | [] ->
	  failwith "Discrimination_tree.flatten_term"



    
(* skip some terms in fterm. *)
let rec skip_terms_fterm (fterm: fterm) (skip: int) : fterm =
  if skip = 0 then
    fterm

  else
    match fterm with 
      | hkey :: tail ->
	  (* one symbol skipped, but also skip the subterm corresponding to it,
	     e.g. if f is skipped in f(a) then a must be skipped as well. *)
	  skip_terms_fterm tail (skip - 1 +  (hkey_arity hkey))

      | _ ->
	  failwith "Discrimination_tree.skip_terms_fterm: Bad flattened term structure."

let rec skip_terms_fterm_partial (fterm: fterm) (skip: int) : fterm * int =
  if skip = 0 then
    fterm, 0

  else
    match fterm with 
      | hkey :: tail ->
	  (* one symbol skipped, but also skip the subterm corresponding to it,
	     e.g. if f is skipped in f(a) then a must be skipped as well. *)
	  skip_terms_fterm_partial tail (skip - 1 +  (hkey_arity hkey))

      | [] ->
	  [], skip






(*** fterm comparisons based on (abstracted) unification ***)

(* removes prefix from fterm, raises Exit if not a proper prefix *)
let rec shorten_fterm (prefix: fterm) (fterm: fterm) : fterm =
  match prefix, fterm with
    | [], _ ->
	fterm

    | xh :: xt, yh :: yt when
	hkey_equal xh yh ->
	shorten_fterm xt yt

    | _ ->
	raise Exit

(* find common prefix *)
let find_prefix (_x: fterm) (_y: fterm) : fterm * fterm * fterm =

  let rec find_prefix' (prefix: fterm) (x: fterm) (y: fterm) : fterm * fterm * fterm =
    match x, y with
      | xh :: xt, yh :: yt when
	  hkey_equal xh yh ->
	  find_prefix' (xh :: prefix) xt yt
	    
      | _ ->
	  List.rev prefix, x, y
  in
    find_prefix' [] _x _y

(* strictly identical *)
let rec fterm_equal (x: fterm) (y: fterm) : bool =
  x == y
  ||
  match x, y with
    | [], [] ->
	true

    | xh :: xt, yh :: yt when
	hkey_equal xh yh ->
	fterm_equal xt yt

    | _ ->
	false



(* x is more general than y,
   i.e. the term abstracted to x can be instantiated to the term abstracted to y *)
let rec fterm_equal_generalization ~(p_preserving:bool) (x: fterm) (y: fterm) : bool =
  match x, y with
    | [], [] ->
	true

    | xh :: xt, yh :: yt ->
	(* equal, so compare tails *)
	if hkey_equal xh yh then begin
	  fterm_equal_generalization ~p_preserving:p_preserving xt yt
	end
	  
	(* xh more general, so skip instance in y and compare rest *)
	else if
	  xh = Variable
	  ||
	  (
	    not p_preserving
	    &&
	    xh = Parameter
	  )
	then begin
	  fterm_equal_generalization ~p_preserving:p_preserving xt (skip_terms_fterm y 1)
	end

	else begin
	  false
	end

    | _ ->
	print_endline (fterm_to_string x);
	print_endline (fterm_to_string y);
	failwith "Discrimination_tree.fterm_equal_generalization: Bad flattened term structure."




(*** returning fast when finding _one_ entry ***)

(* an element matching the request has been found in the index.
   unfortunately, just returning it with (FOUND 'data)
   is not possible due to the polymorphic exception.

   therefore, stick it into a tree variable,
   raise the exception, and retrieve it afterwards.
*)
exception FOUND

let set_found (tree: 'data tree) (data: 'data) : unit =
  tree.found <- Some data

let get_found (tree: 'data tree) : 'data =
  match tree.found with
    | None ->
	failwith "Discrimination_tree.get_found"
	  
    | Some data ->
	tree.found <- None;
	data

	  




(*** add an entry to a tree ***)


(* a new entry shares as much of its prefix with existing entries
   as possible.

   e.g. if previously the tree contained
   f(a, b) and f(b, a)
   it contained two leafs with the prefix [f] and the tails [a, b] resp. [b, a].
   now if f(a, a) is added,
   a new inner node with the prefix [f, a] is created,
   f(a, b) is stored below it in a leaf with tail [b],
   and f(a, a) is stored below it in a leaf with tail [b].
*)

(* no_duplicates: see term_indexing
   don't add the same term twice (even if it carries different data). *)
let rec add_term ~(no_duplicates: bool) (_data: 'data data)
    (entry: 'data entry) (new_term: fterm) (fterm: fterm) (node: 'data inner_node) : bool =
  match fterm with
    | [] ->
	failwith "discrimination_tree:add_term: Bad index structure 1."
	  
    | hkey :: tail ->
	begin
	  try 
	    let next =
	      Table.find node.next hkey
	    in
	      begin
		(* check if node compression matches remaining fterm *)
		try
		  let tail =
		    shorten_fterm next.prefix tail
		  in
		    (* ok, compatible prefix, continue *)
		    match next.variant with
		      | DtLeafNode entries ->
			  (* already at the leaf node *)
			  if Const.debug && tail != [] then
			    failwith "add_term";

			  if no_duplicates then begin
			    (* entry does already exist *)
			    if
			      List.exists
				(fun entry' ->
				   Term.term_equal entry.term entry'.term
				)
				entries.entries
			    then
			      false

			    (* first entry under this term *)
			    else begin
			      entries.entries <- entry :: entries.entries;
			      true
			    end
			  end
			    
			  else begin
			    entries.entries <- entry :: entries.entries;
			    true
			  end
			    
		      | DtInnerNode table1 ->
			  (* continue traversal *)
			  add_term ~no_duplicates:no_duplicates _data entry new_term tail table1

		with
		  | Exit ->
		      (* incompatability in the remainder of the path to this node.
			 so, split into a common node with two subnodes *)
		      let common_prefix, prefix', fterm' =
			find_prefix next.prefix tail
		      in

		      (* create a new node with the content of the current node,
			 but a shortened prefix *)
		      let new_next = {
			prefix = List.tl prefix';
			variant = next.variant;
		      }
		      in

		      (* create a new leaf node for the new entry *)
		      let new_leaf_node = {
			prefix = List.tl fterm';
			variant = DtLeafNode {
			  entries = [ entry ];
			  fterm = new_term;
			}
		      }
		      in

		      (* transform the previous node to a common prefix node *)
		      let common_inner_node =
			create_inner_node ()
		      in
			Table.add common_inner_node.next (List.hd prefix') new_next;
			Table.add common_inner_node.next (List.hd fterm') new_leaf_node;

			next.prefix <- common_prefix;
			next.variant <- DtInnerNode common_inner_node;
			true
	      end
	  with
	    | Not_found ->
		(* hkey, does not exist, so insert a new leaf node *)
		let new_leaf = {
		  prefix = tail;
		  variant = DtLeafNode {
		    entries = [ entry ];
		    fterm = new_term;
		  }
		}
		in
		  Table.add node.next hkey new_leaf;
		  true
	end




(*** remove ***)


(* remove entry from a leaf.
   raise Not_found if the leaf does not contain a corresponding entry. *)
let remove_leaf ~(equal: bool)
    (* the entry to remove *)
    (term: term) (data: 'data option) (_data: 'data data)
    (leaf: 'data leaf) : unit =

  (* returns the list of remaining entries, without the removed one *)
  let rec list_remove_variant' (entries: 'data entry list) : 'data entry list =
    match entries with
      | [] ->
	  (* no suitable entry found *)
	  raise Not_found
	    
      | entry :: tail ->
	  if
	    (equal && Term.term_equal entry.term term)
	    ||
	    (not equal && Term.are_terms_variants entry.term term)
	  then begin
	    match data with
	      | Some termdata when 
		  not (_data#is_equal entry.data termdata) ->
		  (* continue search ... *)
		  entry :: list_remove_variant' tail
		    
	      | _ ->
		    (* success, remove entry *)
		  tail
	  end
	    
	  else
	    (* continue search ... *)
	    entry :: list_remove_variant' tail
  in
    (* remove a matching entry *)
    leaf.entries <- list_remove_variant' leaf.entries



(* raises Not_found if no matching entry is found.
   returns true if all entries of the subnode have been removed. *)
let rec remove_node ~(equal: bool) (term: term) (fterm: fterm) (data: 'data option) (_data: 'data data) (node: 'data node) : bool =
  try
    let fterm =
      shorten_fterm node.prefix fterm
    in
      match node.variant with
	| DtInnerNode table ->
	    (* node, so continue traversal *)
	    begin
	      match fterm with
		| [] ->
		    raise (Failure "Discrimination_tree.remove_node: Bad index structure.")
		      
		| hkey :: fterm1 ->
		    begin
		      try
			let empty =
			  remove_node ~equal:equal term fterm1 data _data (Table.find table.next hkey)
			in
			  (* the hkey node is empty now, so remove it *) 
			  if empty then begin
			    Table.remove table.next hkey;
			    (* is this node now empty as well? *)
			    Table.length table.next == 0;
			  end
			    
			  else begin
			    false
			  end
		      with
			| Not_found ->
			    raise Not_found
		    end
	    end
	      
	| DtLeafNode leaf ->
	    if fterm != [] then
	      failwith "remove_node";

	    (* leaf, so try to remove *)
	    remove_leaf ~equal:equal term data _data leaf;
	    (* no entries left? *)
	    (leaf.entries = [])

 with
    | Exit ->
	(* prefix does not match *)
	raise Not_found









let remove_variant ~(equal:bool) (tree: 'data tree) (term: term) (data: 'data option) (_data: 'data data) : bool =
  let fterm =
    flatten_term term
  in
    try
      ignore (remove_node ~equal:equal term fterm data _data tree.root : bool);
      dec_size tree;
      true
    with
      | Not_found ->
	  false








(*** tree traversal ***)


let rec iter (func: term -> 'data -> unit) (node: 'data node) : unit =
  match node.variant with
    | DtInnerNode table ->
	Table.iter 
	  (fun _ node' ->
	     iter func node')
	  table.next

    | DtLeafNode entries ->
	List.iter
	  (fun entry -> func entry.term entry.data )
	  entries.entries
	

let rec fold (func: 'a -> term -> 'data -> 'a) (acc: 'a) (node: 'data node) : 'a =
  match node.variant with
    | DtInnerNode table ->
	Table.fold
	  (fun _ node acc ->
	     fold func acc node
	  )
	  table.next
	  acc

    | DtLeafNode entries ->
	List.fold_left
	  (fun acc entry ->
	     func acc entry.term entry.data
	  )
	  acc
	  entries.entries
	




(*** leaf check predicates for retrieval functions ***)

(* check if a leaf node contains searched for entries *)
let create_leaf_check
    (* find all or only the first suitable entry? *)
    ?(find_all: bool = true)
    (* search happens within this index *)
    (tree: 'data tree)
    (* check on the leaf fterm *)
    (check_fterm: fterm -> bool)
    (* check on each entry's term *)
    (check_term: term -> bool)

    (* the leaf to check *)
    (leaf: 'data leaf)
    (* the currently found results *)
    (acc: 'data list)
    : 'data list =

  if not (check_fterm leaf.fterm) then begin
    acc
  end
    
  (* add all suitable entries to the results *)
  else begin
    List.fold_left
      (fun acc entry ->
	 if check_term entry.term then begin
	   if find_all then
	     entry.data :: acc
	   else begin
	     set_found tree entry.data;
	     raise FOUND;
	   end
	 end
	 else
	   acc
      )
      acc
      leaf.entries
  end

let create_leaf_check_subst
    (* find all or only the first suitable entry? *)
(*    ?(find_all: bool = true)*)
    (* search happens within this index *)
    (_tree: 'data tree)
    (* check on the leaf fterm *)
    (check_fterm: fterm -> bool)
    (* check on each entry's term *)
    (check_term: term -> Subst.subst)

    (* the leaf to check *)
    (leaf: 'data leaf)
    (* the currently found results *)
    (acc: ('data * Subst.subst) list)
    : ('data * Subst.subst) list =

  if not (check_fterm leaf.fterm) then begin
    acc
  end
    
  (* add all suitable entries to the results *)
  else begin
    List.fold_left
      (fun acc entry ->
	 try
	   let subst =
	     check_term entry.term
	   in
(*	     if find_all then*)
	       (entry.data, subst) :: acc
(*	     else begin
	       failwith "Discrimation_tree.create_leaf_check_subst";
	       (*
	       set_found tree entry.data;
	       raise FOUND;
	       *)
	     end*)
	 with
	   | Unification.UNIFICATION_FAIL  ->
	       acc
      )
      acc
      leaf.entries
  end



(*** tree traversal based on fterms ***)

(* descend into the variant of the given hkey.
   tail is the rest of the fterm towards the potential leaf of the query tree. *)
let descend_variant ~(p_preserving: bool)
    (func: p_preserving:bool -> ('data, 'a) leaf_check -> 'a list -> 'fterm -> 'data node -> fterm -> 'a list)
    (leaf_check: ('data, 'a) leaf_check)
    (acc: 'a list) 
    (hkey: hkey) (tail: fterm) (table: 'data inner_node)
    : 'a list
    =
  let node =
    try
      Some (Table.find table.next hkey)
    with
      | Not_found ->
	  None
  in
    match node with
      | None ->
	  acc
	    
      | Some node' ->
	  func ~p_preserving:p_preserving leaf_check acc tail node' node'.prefix

(* descend into the generalization of the given hkey.
   tail is the rest of the fterm towards the potential leaf of the query tree,
   where fterm = hkey :: tail. *)
let descend_generalized ~(p_preserving: bool)
    (func: p_preserving:bool -> ('data, 'a) leaf_check -> 'a list -> 'fterm -> 'data node -> fterm -> 'a list)
    (leaf_check: ('data, 'a) leaf_check)
    (acc: 'a list) 
    (hkey: hkey) (fterm: fterm) (table: 'data inner_node)
    : 'a list
    =
  let node =
    try
      Some (Table.find table.next hkey)
    with
      | Not_found ->
	  None
  in
    match node with
      | None ->
	  acc
	    
      | Some node' ->
	  func ~p_preserving:p_preserving leaf_check acc (skip_terms_fterm fterm 1) node' node'.prefix
	  

(* descend into an instance of the given hkey.
   tail is the rest of the fterm towards the potential leaf of the query tree. *)
let rec descend_instance ~(p_preserving:bool)
    (func: p_preserving:bool -> ('data, 'a) leaf_check -> 'a list -> 'fterm -> 'data node -> fterm -> 'a list)
    (leaf_check: ('data, 'a) leaf_check)
    (acc: 'a list) 
    (query_tail: fterm) (table: 'data inner_node) (skip: int)
    : 'a list
    =
  Table.fold
    (fun hkey node acc ->
       let skip' =
	 skip - 1 + (hkey_arity hkey)
       in
	 
       (* now skip also the prefix of this node *)
       let prefix, skip' =
	 skip_terms_fterm_partial node.prefix skip'
       in
	 if skip' = 0 then
	   func ~p_preserving:p_preserving leaf_check acc query_tail node prefix

	 else
	   match node.variant with
	     | DtLeafNode _ ->
		 failwith "Discrimination_tree.descend_instance"
		 
	     | DtInnerNode table' ->
		 descend_instance ~p_preserving:p_preserving func leaf_check acc query_tail table' skip'
    )
    table.next
    acc



(*** variant retrieval ***)


let rec find_variants (leaf_check: ('data, 'data) leaf_check)
    (fterm: fterm) (node: 'data node) : 'data list =

  try
    let fterm =
      shorten_fterm node.prefix fterm
    in
      begin
	match fterm, node.variant with
	  | hkey :: fterm', DtInnerNode table ->
	      begin
		try
		  let node' =
		    Table.find table.next hkey
		  in
		    find_variants leaf_check fterm' node'
		with
		  | Not_found ->
		      []
	      end
		
	  | [], DtLeafNode leaf ->
	      leaf_check leaf []
		
	  | _ ->
	      failwith "Discrimination_tree.find_variants"
      end
  with
    | Exit ->
	[]




(*** generalization retrieval ***)



let rec find_generalizations ~(p_preserving: bool)
    (leaf_check: ('data, 'data) leaf_check) (acc: 'data list)
    (query_fterm: fterm) (node: 'data node) (tree_fterm: fterm) : 'data list =

  (* performance: order so that elements with most variation come first *)
  match query_fterm, tree_fterm, node.variant with
      (* equal keys *)
    | query_key :: query_tail, tree_key :: tree_tail, _ when
	hkey_equal query_key tree_key ->
	(* variant *)
	find_generalizations ~p_preserving:p_preserving
	  leaf_check acc
	  query_tail node tree_tail

    (* query_fterm empty, so check the node itself *)
    | [], [], DtLeafNode leaf ->
	leaf_check leaf acc

    | Symbol _ as hkey :: query_tail, [], DtInnerNode table ->
	let acc =
	  descend_variant ~p_preserving:p_preserving
	    find_generalizations leaf_check acc
	    hkey query_tail table		    
	in
	let acc =
	  descend_generalized ~p_preserving:p_preserving
	    find_generalizations leaf_check acc
	    Variable query_fterm table
	in
	  if p_preserving then
	    acc
	  else
	    descend_generalized ~p_preserving:p_preserving 
	      find_generalizations leaf_check acc
	      Parameter query_fterm table
	      
    | Variable :: query_tail, [], DtInnerNode table ->
	let acc =
	  descend_variant ~p_preserving:p_preserving 
	    find_generalizations leaf_check acc
	    Variable query_tail table
	in
	  if p_preserving then
	    acc
	  else
	    descend_variant ~p_preserving:p_preserving
	      find_generalizations leaf_check acc
	      Parameter query_tail table
	      
    | Parameter :: query_tail, [], DtInnerNode table ->
	let acc =
	  descend_variant ~p_preserving:p_preserving
	    find_generalizations leaf_check acc
	    Variable query_tail table
	in
	  descend_variant ~p_preserving:p_preserving
	    find_generalizations leaf_check acc
	    Parameter query_tail table
	    
    (* symbol or parameter <-> variable *)
    | _ :: _, Variable :: tree_tail, _ ->
 	(* generalization *)
	find_generalizations ~p_preserving:p_preserving
	  leaf_check acc
	  (skip_terms_fterm query_fterm 1) node tree_tail
    
    (* symbol or variable <-> parameter *)
    | _ :: _, Parameter :: tree_tail, _ when not p_preserving ->
	find_generalizations ~p_preserving:p_preserving
	  leaf_check acc
	  (skip_terms_fterm query_fterm 1) node tree_tail

    (* mismatch *)
    | _ :: _, _ :: _, _ ->
	acc

    | [], _ :: _, _ ->
	failwith "Discrimination_tree.find_generalizations 1"

    | _ :: _, [], DtLeafNode _ ->
	failwith "Discrimination_tree.find_generalizations 2"

    | [], [], DtInnerNode _ ->
	failwith "Discrimination_tree.find_generalizations 3"




(*** unifiable retrieval ***)

let rec find_unifiables ~(p_preserving: bool)
    (leaf_check: ('data, 'data) leaf_check) (acc: 'data list)
    (query_fterm: fterm) (node: 'data node) (tree_fterm: fterm) : 'data list =
  (* first handle the tree_term *)
  match query_fterm, tree_fterm, node.variant  with
      (* equal keys *)
    | query_key :: query_tail, tree_key :: tree_tail, _ when
	hkey_equal query_key tree_key ->
	(* variant *)
	find_unifiables ~p_preserving:p_preserving
	  leaf_check acc
	  query_tail node tree_tail


    (* tree_fterm empty, so check the node itself *)
    | [], [] , DtLeafNode leaf ->
	leaf_check leaf acc
		  
    | Symbol _ as hkey :: query_tail, [], DtInnerNode table ->
	let acc =
	  descend_variant ~p_preserving:p_preserving
	    find_unifiables leaf_check acc
	    hkey query_tail table
	in
	let acc =
	  descend_generalized ~p_preserving:p_preserving
	    find_unifiables leaf_check acc
	    Variable query_fterm table
	in
	  if p_preserving then
	    acc
	  else
	    descend_generalized ~p_preserving:p_preserving
	      find_unifiables leaf_check acc
	      Parameter query_fterm table
		
    | Variable :: query_tail, [], DtInnerNode table ->
	descend_instance ~p_preserving:p_preserving
	  find_unifiables leaf_check acc
	  query_tail table 1
	  
    | Parameter :: query_tail, [], DtInnerNode table ->
	if p_preserving then begin
	  let acc =
	    descend_variant ~p_preserving:p_preserving
	      find_unifiables leaf_check acc
	      Variable query_tail table
	  in
	    descend_variant ~p_preserving:p_preserving
	      find_unifiables leaf_check acc
	      Parameter query_tail table
	end
	  
	else begin
	  descend_instance ~p_preserving:p_preserving
	    find_unifiables leaf_check acc
	    query_tail table 1
	end
	  
    (* variable <-> symbol or parameter *)
    | Variable :: query_tail, _ :: _, _ ->
	let tree_fterm', skip =
	  skip_terms_fterm_partial tree_fterm 1
	in
	  if skip = 0 then
	    find_unifiables ~p_preserving:p_preserving
	      leaf_check acc
	      query_tail node tree_fterm'
	  else
	    begin
	      match node.variant with
		| DtInnerNode inner_node ->
		    descend_instance ~p_preserving:p_preserving
		      find_unifiables leaf_check acc
		      query_tail inner_node skip

		| DtLeafNode _ ->
		    failwith "find_unifiables 3"
	    end

    (* symbol or parameter <-> variable *)
    | _ :: _, Variable :: tree_tail, _ ->
	(* generalization *)
	find_unifiables ~p_preserving:p_preserving
	  leaf_check acc
	  (skip_terms_fterm query_fterm 1) node tree_tail

    (* parameter <-> symbol *)
    | Parameter :: query_tail, _ :: _, _ ->
	(* generalization *)
	if p_preserving then
	  acc
	else
	  let tree_fterm', skip =
	    skip_terms_fterm_partial tree_fterm 1
	  in
	    if skip = 0 then
	      find_unifiables ~p_preserving:p_preserving
		leaf_check acc
		query_tail node tree_fterm'
	    else
	    begin
	      match node.variant with
		| DtInnerNode inner_node ->
		    descend_instance ~p_preserving:p_preserving
		      find_unifiables leaf_check acc
		      query_tail inner_node skip

		| DtLeafNode _ ->
		    failwith "find_unifiables 5"
	    end

    (* symbol <-> parameter *)
    | _ :: _, Parameter :: tree_tail, _ ->
	(* generalization *)
	if p_preserving then
	  acc
	else
	  find_unifiables ~p_preserving:p_preserving
	    leaf_check acc
	    (skip_terms_fterm query_fterm 1) node tree_tail
    
    (* mismatch *)
    | _ :: _, _ :: _, _ ->
	acc

    | [], _ :: _, _ ->
	failwith "Discrimination_tree.find_unifiables 1"

    | [], [], DtInnerNode _ ->
	failwith "Discrimination_tree.find_unifiables"
	  
    | _ :: _, [], DtLeafNode _ ->
	failwith "Discrimination_tree.find_unifiables 2"


let rec find_unifiables_subst ~(p_preserving: bool)
    leaf_check (acc: ('data * Subst.subst) list)
    (query_fterm: fterm) (node: 'data node) (tree_fterm: fterm) : ('data * Subst.subst) list =
  (* first handle the tree_term *)
  match query_fterm, tree_fterm, node.variant  with
      (* equal keys *)
    | query_key :: query_tail, tree_key :: tree_tail, _ when
	hkey_equal query_key tree_key ->
	(* variant *)
	find_unifiables_subst ~p_preserving:p_preserving
	  leaf_check acc
	  query_tail node tree_tail


    (* tree_fterm empty, so check the node itself *)
    | [], [] , DtLeafNode leaf ->
	leaf_check leaf acc
		  
    | Symbol _ as hkey :: query_tail, [], DtInnerNode table ->
	let acc =
	  descend_variant ~p_preserving:p_preserving
	    find_unifiables_subst leaf_check acc
	    hkey query_tail table
	in
	let acc =
	  descend_generalized ~p_preserving:p_preserving
	    find_unifiables_subst leaf_check acc
	    Variable query_fterm table
	in
	  if p_preserving then
	    acc
	  else
	    descend_generalized ~p_preserving:p_preserving
	      find_unifiables_subst leaf_check acc
	      Parameter query_fterm table
		
    | Variable :: query_tail, [], DtInnerNode table ->
	descend_instance ~p_preserving:p_preserving
	  find_unifiables_subst leaf_check acc
	  query_tail table 1
	  
    | Parameter :: query_tail, [], DtInnerNode table ->
	if p_preserving then begin
	  let acc =
	    descend_variant ~p_preserving:p_preserving
	      find_unifiables_subst leaf_check acc
	      Variable query_tail table
	  in
	    descend_variant ~p_preserving:p_preserving
	      find_unifiables_subst leaf_check acc
	      Parameter query_tail table
	end
	  
	else begin
	  descend_instance ~p_preserving:p_preserving
	    find_unifiables_subst leaf_check acc
	    query_tail table 1
	end
	  
    (* variable <-> symbol or parameter *)
    | Variable :: query_tail, _ :: _, _ ->
	let tree_fterm', skip =
	  skip_terms_fterm_partial tree_fterm 1
	in
	  if skip = 0 then
	    find_unifiables_subst ~p_preserving:p_preserving
	      leaf_check acc
	      query_tail node tree_fterm'
	  else
	    begin
	      match node.variant with
		| DtInnerNode inner_node ->
		    descend_instance ~p_preserving:p_preserving
		      find_unifiables_subst leaf_check acc
		      query_tail inner_node skip

		| DtLeafNode _ ->
		    failwith "find_unifiables 3"
	    end

    (* symbol or parameter <-> variable *)
    | _ :: _, Variable :: tree_tail, _ ->
	(* generalization *)
	find_unifiables_subst ~p_preserving:p_preserving
	  leaf_check acc
	  (skip_terms_fterm query_fterm 1) node tree_tail

    (* parameter <-> symbol *)
    | Parameter :: query_tail, _ :: _, _ ->
	(* generalization *)
	if p_preserving then
	  acc
	else
	  let tree_fterm', skip =
	    skip_terms_fterm_partial tree_fterm 1
	  in
	    if skip = 0 then
	      find_unifiables_subst ~p_preserving:p_preserving
		leaf_check acc
		query_tail node tree_fterm'
	    else
	    begin
	      match node.variant with
		| DtInnerNode inner_node ->
		    descend_instance ~p_preserving:p_preserving
		      find_unifiables_subst leaf_check acc
		      query_tail inner_node skip

		| DtLeafNode _ ->
		    failwith "find_unifiables 5"
	    end

    (* symbol <-> parameter *)
    | _ :: _, Parameter :: tree_tail, _ ->
	(* generalization *)
	if p_preserving then
	  acc
	else
	  find_unifiables_subst ~p_preserving:p_preserving
	    leaf_check acc
	    (skip_terms_fterm query_fterm 1) node tree_tail
    
    (* mismatch *)
    | _ :: _, _ :: _, _ ->
	acc

    | [], _ :: _, _ ->
	failwith "Discrimination_tree.find_unifiables 1"

    | [], [], DtInnerNode _ ->
	failwith "Discrimination_tree.find_unifiables"
	  
    | _ :: _, [], DtLeafNode _ ->
	failwith "Discrimination_tree.find_unifiables 2"


(*** instance retrieval ***)

let rec find_instances ~(p_preserving: bool)
    (leaf_check: ('data, 'data) leaf_check) (acc: 'data list)
    (query_fterm: fterm) (node: 'data node) (tree_fterm: fterm) : 'data list =

  (* first handle the tree_term *)
  match query_fterm, tree_fterm, node.variant with
      (* equal keys *)
    | query_key :: query_tail, tree_key :: tree_tail, _ when
	hkey_equal query_key tree_key ->
	(* variant *)
	find_instances ~p_preserving:p_preserving
	  leaf_check acc
	  query_tail node tree_tail

    (* query_fterm empty, so check the node itself *)
    | [], [], DtLeafNode leaf ->
	leaf_check leaf acc
		  
    | Symbol _ as hkey :: query_tail, [], DtInnerNode table ->
	descend_variant ~p_preserving:p_preserving
	  find_instances leaf_check acc
	  hkey query_tail table
	  
    | Variable :: query_tail, [], DtInnerNode table ->
	descend_instance ~p_preserving:p_preserving
	  find_instances leaf_check acc
	  query_tail table 1
	  
    | Parameter :: query_tail, [], DtInnerNode table ->
	if p_preserving then begin
	  descend_variant ~p_preserving:p_preserving
	    find_instances leaf_check acc
	    Parameter query_tail table
	end

	else begin
	  descend_instance ~p_preserving:p_preserving
	    find_instances leaf_check acc
	    query_tail table 1
	end
	  
    (* variable <-> symbol or parameter *)
    | Variable :: query_tail, _ :: _, _ ->
	let tree_fterm', skip =
	  skip_terms_fterm_partial tree_fterm 1
	in
	  if skip = 0 then
	    find_instances ~p_preserving:p_preserving
	      leaf_check acc
	      query_tail node tree_fterm'
	  else
	    begin
	      match node.variant with
		| DtInnerNode inner_node ->
		    descend_instance ~p_preserving:p_preserving
		      find_instances leaf_check acc
		      query_tail inner_node skip

		| DtLeafNode _ ->
		    failwith "find_instances 3"
	    end

    (* parameter <-> symbol or parameter *)
    | Parameter :: query_tail, _ :: _, _ ->
	if p_preserving then
	  acc
	else
	  let tree_fterm', skip =
	    skip_terms_fterm_partial tree_fterm 1
	  in
	    if skip = 0 then
	      find_instances ~p_preserving:p_preserving
		leaf_check acc
		query_tail node tree_fterm'
	    else
	    begin
	      match node.variant with
		| DtInnerNode inner_node ->
		    descend_instance ~p_preserving:p_preserving
		      find_instances leaf_check acc
		      query_tail inner_node skip

		| DtLeafNode _ ->
		    failwith "find_instances 4"
	    end

    (* failure *)
    | _ :: _, _ :: _, _ ->
	acc

    | [], _ :: _, _ ->
	failwith "Discrimination_tree.find_instances 1"

    | [], [], DtInnerNode _ ->
	failwith "Discrimination_tree.find_instance 1"

    | _ :: _, [], DtLeafNode _ ->
	failwith "Discrimination_tree.find_instance 2"





(*** shielding ***)


(* find an entry potentially p-instantiating below the producer term *)
let rec find_shielding ~(p_preserving: bool)
    (leaf_check: ('data, 'data) leaf_check) (acc: 'data list)
    (query_fterm: fterm) (node: 'data node) (tree_fterm: fterm) : 'data list =

  (* first handle the tree_term *)
  match query_fterm, tree_fterm, node.variant with
      (* equal keys *)
    | query_key :: query_tail, tree_key :: tree_tail, _ when
	hkey_equal query_key tree_key ->
	(* variant *)
	find_shielding ~p_preserving:p_preserving
	  leaf_check acc
	  query_tail node tree_tail

    (* query_fterm empty, so check the node itself *)
    | [], [], DtLeafNode leaf ->
	leaf_check leaf acc

    | Symbol _ as hkey :: query_tail, [], DtInnerNode table ->
	let acc =
	  descend_variant ~p_preserving:p_preserving
	    find_shielding leaf_check acc
	    hkey query_tail table
	in
	  descend_generalized ~p_preserving:p_preserving
	    find_shielding leaf_check acc
	    Variable query_fterm table
	    
    | Variable :: query_tail, [], DtInnerNode table
    | Parameter :: query_tail, [], DtInnerNode table ->
	descend_instance ~p_preserving:p_preserving
	  find_shielding leaf_check acc
	  query_tail table 1
	  
    | Variable :: query_tail, Parameter :: tree_tail, _
    | Parameter :: query_tail, Variable :: tree_tail, _ ->
	find_shielding ~p_preserving:p_preserving
	  leaf_check acc
	  query_tail node tree_tail

    | Variable :: query_tail, Symbol _ :: _, _
    | Parameter :: query_tail, Symbol _ :: _, _ ->
	let tree_fterm', skip =
	  skip_terms_fterm_partial tree_fterm 1
	in
	  if skip = 0 then
	    find_shielding ~p_preserving:p_preserving
	      leaf_check acc
	      query_tail node tree_fterm'
	  else
	    begin
	      match node.variant with
		| DtInnerNode inner_node ->
		    descend_instance ~p_preserving:p_preserving
		      find_shielding leaf_check acc
		      query_tail inner_node skip

		| DtLeafNode _ ->
		    failwith "find_shielding 3"
	    end

    (* symbol <-> variable *)
    | _ :: _, Variable :: tree_tail, _ ->
 	(* generalization *)
	find_shielding ~p_preserving:p_preserving
	  leaf_check acc
	  (skip_terms_fterm query_fterm 1) node tree_tail


    (* mismatch *)
    | _ :: _, _ :: _, _ ->
	acc

    | [], _ :: _, _ ->
	failwith "Discrimination_tree.find_instances 1"

    | [], [], DtInnerNode _ ->
	failwith "Discrimination_tree.find_shielding"

    | _, [], DtLeafNode _ ->
	failwith "Discrimination_tree.find_shielding 2"





(* class index (database: Term.database)

   essentially this class is just a wrapper around the functions defined above.
*)
class ['data] predicate_index (__productivity: bool) (__data: 'data data) =

object (self)

   
  (**** values ***)

  (* object providing functions on the data type *)
  val _data = __data
  val _productivity = __productivity

  val _tree: 'data tree = create_tree ()

	
  (*** creation ***)
      
  method clear =
    clear _tree
      
  method size =
    get_size _tree
 
      


  (*** add ***)


  method add ?(no_duplicates: bool = false) (term: term) (data: 'data) : unit =
    match _tree.root.variant with
      | DtInnerNode table ->
	  let fterm =
	    flatten_term term
	  in
	  let added =
	    if _productivity then
	      add_term ~no_duplicates:no_duplicates _data (create_entry term data) fterm fterm table
	    else
	      add_term ~no_duplicates:no_duplicates _data (create_entry term data) [] fterm table;
	  in
	    if added then begin
	      inc_size _tree;
	    end;

      | _ ->
	  raise (Failure "discrimination_tree:add: Bad index toplevel type.")



  (*** traversal ***)
      
  method iter (func: term -> 'data -> unit) : unit =
    iter func _tree.root
      
  method fold:
    'a. ('a -> term -> 'data -> 'a) -> 'a -> 'a =
    fun func acc ->
      fold func acc _tree.root


  method remove (term: term) (data: 'data option) : bool =
    if get_size _tree = 0 then
      false
    else
      remove_variant ~equal:true _tree term data _data




  (*** variants ***)

  method find_all_variants (term: term) : 'data list =
    if get_size _tree = 0 then
      []
    else
      find_variants
	(create_leaf_check ~find_all:true
	   _tree
	   (fun _ -> true)
	   (Term.are_terms_variants term)
	)
	(flatten_term term)
	_tree.root

  method find (term: term) : 'data option =
    if get_size _tree = 0 then
      None
    else
      try
	ignore (
	  find_variants
	    (create_leaf_check ~find_all:false
	       _tree
	       (fun _ -> true)
	       (Term.term_equal term)
	    )
	    (flatten_term term)
	    _tree.root
	    : 'data list);
	None
      with FOUND ->
	Some (get_found _tree)



  (*** generalization ***)

  method private find_generalizations' ~(find_all: bool) ~(p_preserving: bool) (term: term) : 'data list =
    find_generalizations ~p_preserving:p_preserving
      (create_leaf_check
	 ~find_all:find_all
	 _tree
	 (fun _ -> true)
	 (Unification.is_term_instance ~p_preserving:p_preserving term)
      )
      []
      (flatten_term term)
      _tree.root
      _tree.root.prefix
	    

  method find_all_generalizations ~(p_preserving: bool) (term: term) : 'data list =  
    if get_size _tree = 0 then
      []
    else
      self#find_generalizations' ~find_all:true ~p_preserving:p_preserving term

  method find_generalization ~(p_preserving: bool) (term: term) : 'data option =  
    if get_size _tree = 0 then
      None
    else
      try
	ignore(
	  self#find_generalizations' ~find_all:false ~p_preserving:p_preserving term
	    : 'data list);
	None
      with FOUND ->
	Some (get_found _tree)
      


  (*** unifiable ***)
	    
  method private find_unifiables' ~(find_all: bool) ~(p_preserving: bool) (term: term) : 'data list =
    find_unifiables ~p_preserving:p_preserving
      (create_leaf_check
	 ~find_all:find_all
	 _tree
	 (fun _ -> true)
	 (Unification.are_terms_unifiable ~p_preserving:p_preserving term)
      )
      []
      (flatten_term term)
      _tree.root
      _tree.root.prefix

  method find_all_unifiable ~(p_preserving: bool) (term: term) : 'data list = 
    if get_size _tree = 0 then
      []
    else
      self#find_unifiables' ~find_all:true ~p_preserving:p_preserving term

  method find_unifiable ~(p_preserving: bool) (term: term) : 'data option = 
    if _tree.size == 0 then
      None
    else
      try
	ignore(
	  self#find_unifiables' ~find_all:false ~p_preserving:p_preserving term
	  : 'data list);
	None
      with FOUND ->
	Some (get_found _tree)

  method find_all_unifiable_subst ~(p_preserving: bool) (term: term) : ('data * Subst.subst) list = 
    if get_size _tree = 0 then
      []
    else
    find_unifiables_subst ~p_preserving:p_preserving
      (create_leaf_check_subst
(*	 ~find_all:true*)
	 _tree
	 (fun _ -> true)
	 (fun tree_term ->
	    (* context literal term first,
	       so that bindings are from context literal vars to clause literal vars.
	       see Subst.remove_universal_context_vars.
	    *)
	    Unification.unify_terms ~recompute:false ~p_preserving:p_preserving
	      term Term_indexing.query_offset tree_term Term_indexing.index_offset)
      )
      []
      (flatten_term term)
      _tree.root
      _tree.root.prefix



  (*** instance ***)

  method private find_instances' ~(find_all: bool) ~(p_preserving: bool) (term: term) : 'data list =
    find_instances ~p_preserving:p_preserving
      (create_leaf_check
	 ~find_all:find_all
	 _tree
	 (fun _ -> true)
	 (fun tree_term ->
	    Unification.is_term_generalization ~p_preserving:p_preserving term tree_term
	 )
      )
      []
      (flatten_term term)
      _tree.root
      _tree.root.prefix


  method find_all_instances ~(p_preserving: bool) (term: term) : 'data list =
    if get_size _tree = 0 then
      []
    else
      self#find_instances' ~find_all:true ~p_preserving:p_preserving term

  method find_instance ~(p_preserving: bool) (term: term) : 'data option = 
    if get_size _tree = 0 then
      None
    else
    try 
      ignore(
	self#find_instances' ~find_all:false ~p_preserving:p_preserving term
	  : 'data list);
      None
    with FOUND ->
      Some (get_found _tree)



  (*** productivity ***)

  (* find an instance of producer that is more general than produced *)
  method find_strongly_shielding (producer: term) (produced: term) : 'data option =
    if not _productivity then
      failwith "Discrimination_tree: productivity not enabled"
    else if get_size _tree = 0 then
      None
    else
      let fterm_produced =
	flatten_term produced
      in
      try 
	ignore(
	  find_instances ~p_preserving:false
	    (create_leaf_check
	       ~find_all:false
	       _tree
	       (fun leaf_fterm -> fterm_equal_generalization ~p_preserving:false leaf_fterm fterm_produced)
	       (fun tree_term ->
		  (Term.term_equal tree_term produced)
		  ||
		    (
		      (* must be a true instance *)
		      not (Term.term_equal producer tree_term)
		      &&
		      Unification.is_term_instance produced tree_term
		      &&
		      Unification.is_term_generalization producer tree_term
		    )
	       )
	    )
	    []
	    (flatten_term producer)
	    _tree.root
	    _tree.root.prefix
	    : 'data list);
	None
      with FOUND ->
	Some (get_found _tree)
	  

  method private shielding_entry_check (producer: term) (produced: term) (tree_term: term) : bool =
    (* produced itself is ok *)
    (Term.term_equal tree_term produced)
    ||
    (
      (* must be a true instance *)
      not (Term.term_equal tree_term producer)
      &&
      (* must be more general then the produced term *)
	Unification.is_term_instance produced tree_term
	(* p-preservingly unifiable from the tree term -
	   this instance of producer and tree term is still more general
	   than produced, as producer and tree term themselves
	   are more general than produced.
	   (at least, I, Alex, think so)
	*)
      &&
      (
	try
	  let subst =
	    Unification.unify_terms ~recompute:false
	      producer Term_indexing.query_offset
	      tree_term Term_indexing.index_offset
	  in
	    Subst.is_p_renaming subst Term_indexing.index_offset
	with
	  | Unification.UNIFICATION_FAIL  ->
	      false
      )
    )

  (* find a term p-preservingly instantiable to an instance of producer,
     which is also more general than produced *)
  method private find_shielding' ~(find_all: bool) (producer: term) (produced: term) : 'data list =
    let fterm_produced =
      flatten_term produced
    in
    find_shielding ~p_preserving:false
      (create_leaf_check
	 ~find_all:find_all
	 _tree
	 (*(fterm_equal_shielding)*)
	 (fun leaf_fterm -> fterm_equal_generalization ~p_preserving:false leaf_fterm fterm_produced)
	 (self#shielding_entry_check producer produced)
      )
      []
      (flatten_term producer)
      _tree.root
      _tree.root.prefix


  method find_all_shielding (producer: term) (produced: term) : 'data list =
    if not _productivity then
      failwith "Discrimination_tree: productivity not enabled"
    else if get_size _tree = 0 then
      []
    else
      self#find_shielding' ~find_all:true producer produced

  method find_shielding (producer: term) (produced: term) : 'data option =
    if not _productivity then
      failwith "Discrimination_tree: productivity not enabled"
    else if get_size _tree = 0 then
      None
    else
      try 
	ignore(
	  self#find_shielding' ~find_all:false producer produced
	    : 'data list);
	None
      with FOUND ->
	Some (get_found _tree)
    



  method get_generalization_iterator ~(p_preserving: bool) (term: term) : 'data iterator =
    let generalizations =
      self#find_all_generalizations ~p_preserving:p_preserving term
    in
      object (_iterator)
	val mutable _elements = generalizations

	method is_empty =
	  match _elements with
	    | [] -> true
	    | _ -> false

	method next =
	  match _elements with
	    | [] ->
		raise ITERATOR_EMPTY

	    | element :: tail ->
		_elements <- tail;
		element
      end


  (* like find_all_shielding, but computing matches one at a time *)
  method get_shielding_iterator (producer: term) (produced: term) : 'data iterator =

    object (iterator)
      val _producer_fterm = flatten_term producer
      val _produced_fterm = flatten_term produced
      
      (* index nodes still to search for *)
      val mutable _nodes: (fterm * 'data node * fterm) list = []
      
      (* currently found potentially shielding entries *)
      val mutable _entries: 'entry list = []

      (* a found shielding entry *)
      val mutable _shielding: 'data option = None

	
      initializer
	(* start with query term and root node node *)
	if not _productivity then
	  failwith "Discrimination_tree: productivity not enabled"
	else if get_size _tree > 0 then
	  iterator#save_node _producer_fterm _tree.root _tree.root.prefix


      (* new node to search forfound *)
      method private save_node (query_ftail: fterm) (node: 'data node) (tree_ftail: fterm) =
	_nodes <- (query_ftail, node, tree_ftail) :: _nodes

      (* matching leaf found, evaluate its entries *)
      method private save_leaf (leaf: 'data leaf) =
	(* nodes more general? *)
	if fterm_equal_generalization ~p_preserving:false leaf.fterm _produced_fterm then begin
	  _entries <- _entries @ leaf.entries;
	end
  
      method is_empty =
	match _shielding with
	  | Some _ ->
	      (* yes, entry found *)
	      false
		
	  | None ->
	      (* don't know, check ... *)
	      begin
		match _entries with
		  | entry :: tail ->
		      (* ...leaves *)
		      _entries <- tail;
		      if self#shielding_entry_check producer produced entry.term then begin
			_shielding <- Some entry.data;
		      end;
		      iterator#is_empty
			
		  | [] ->
		      begin
			match _nodes with
			  | (query_fterm, node, tree_fterm) :: tail ->
			      (* ...nodes *)
			      _nodes <- tail;
			      iterator#advance query_fterm node tree_fterm;
			      iterator#is_empty
			    
			  | [] ->
			      true
		      end
	      end

      method next =
	if iterator#is_empty then
	  raise ITERATOR_EMPTY
	
	else  
	  match _shielding with
	    | None ->
		failwith "Discrimination_tree.get_shielding_iterator.next"
		  
	    | Some shielding ->
		_shielding <- None;
		shielding


      method private advance (query_fterm: fterm) (node: 'data node) (tree_fterm: fterm) : unit =
	(* first handle the tree_term *)
	match query_fterm, tree_fterm, node.variant  with
	  | [], _ :: _, _ ->
	      failwith "Discrimination_tree.get_shielding_iterator.advance 1"

	  (* equal keys *)
	  | query_key :: query_tail, tree_key :: tree_tail, _ when
	      hkey_equal query_key tree_key ->
	      (* variant *)
	      iterator#advance query_tail node tree_tail

	  | Variable :: query_tail, Parameter :: tree_tail, _
	  | Parameter :: query_tail, Variable :: tree_tail, _ ->
	      iterator#advance query_tail node tree_tail

	  | Variable :: query_tail, Symbol _ :: _, _
	  | Parameter :: query_tail, Symbol _ :: _, _ ->
	      let tree_fterm', skip =
		skip_terms_fterm_partial tree_fterm 1
	      in
		if skip = 0 then
		  iterator#advance query_tail node tree_fterm'
		else
		  begin
		    match node.variant with
		      | DtInnerNode inner_node ->
			  iterator#descend_instance query_tail inner_node skip

		      | DtLeafNode _ ->
			  failwith "Discrimination_tree.get_shielding_iterator.advance 2"
		  end

	  (* symbol <-> variable *)
	  | _ :: _, Variable :: tree_tail, _ ->
 	      (* generalization *)
	      iterator#advance (skip_terms_fterm query_fterm 1) node tree_tail
		
		
	  (* mismatch *)
	  | _ :: _, _ :: _, _ ->
	      ()

	  (* query_fterm empty, so check the node itself *)
	  | [], [], DtLeafNode leaf ->
	      iterator#save_leaf leaf
		
	  | Symbol _ as hkey :: query_tail, [], DtInnerNode table ->
	      begin
		try
		  (* for the same symbol don't store it by immediately follow it,
		     for all other cases store the new node to avoid early branching. *)
		  let node' =
		    Table.find table.next hkey
		  in
		    iterator#advance query_tail node' node'.prefix
		with
		  | Not_found ->
		      ()
	      end;
	      begin
		try
		  let node' =
		    Table.find table.next Variable
		  in
		    iterator#save_node (skip_terms_fterm query_fterm 1) node' node'.prefix
		with
		  | Not_found ->
		      ()
	      end
		
	  | Variable :: query_tail, [], DtInnerNode table
	  | Parameter :: query_tail, [], DtInnerNode table ->
	      iterator#descend_instance query_tail table 1
		
	  | [], [], DtInnerNode _ ->
	      failwith "Discrimination_tree.get_shielding_iterator.advance 3"

	  | _, [], DtLeafNode _ ->
	      failwith "Discrimination_tree.get_shielding_iterator.advance 4"


      method private descend_instance (query_tail: fterm) (table: 'data inner_node) (skip: int) : unit =
	Table.iter
	  (fun hkey node ->
	     let skip' =
	       skip - 1 + (hkey_arity hkey)
	     in

	     (* now skip also the prefix of this node *)
	     let prefix, skip' =
	       skip_terms_fterm_partial node.prefix skip'
	     in
	       if skip' = 0 then
		 iterator#save_node query_tail node prefix
	       
	       else
		 match node.variant with
		   | DtLeafNode _ ->
		       failwith "shielding_iterator.descend_instance"
			 
		   | DtInnerNode table' ->
		       iterator#descend_instance query_tail table' skip'
	  )
	  table.next
	  
	  
    end
      
      
  (*** to_string ***)

  method to_string : string =
    tree_to_string _tree
      
end


    
class ['data] index
  (__productivity: bool) (__data: 'data data)
   =
object (_self)
  inherit ['data] Term_indexing.index
    __data
    (new predicate_index __productivity)
end






(*** public interface ***)

let create_predicate_index (productivity: bool) (data: 'data data)  : 'data Term_indexing.predicate_index =
  new predicate_index productivity data


let create_index (productivity: bool) (data: 'data data) : 'data Term_indexing.index =
  new index productivity data



(*** specialized term indexes ***)

(* int index *)
class int_data =
object
  method is_equal (x: int) (y: int) = x == y
  method to_string x = string_of_int x
end

let create_int_index (productivity: bool) : int index =
  new index productivity (new int_data)


(* term index *)
class term_data =
object
  method is_equal x y = Term.term_equal x y
  method to_string x = Term.term_to_string x
end

let create_term_index (productivity: bool) : term index =
  new index productivity (new term_data)


(* literal index *)
class literal_data =
object
  method is_equal x y = Term.literal_equal x y
  method to_string x = Term.literal_to_string x
end

let create_literal_index (productivity: bool) : literal index =
  new index productivity (new literal_data)


(* clause index *)
class clause_data =
object
  method is_equal x y = Term.clause_equal x y
  method to_string x = Term.clause_to_string x
end

let create_clause_index (productivity: bool) : clause index =
  new index productivity (new clause_data)



(* literal and clause index *)
class literal_clause_data =
object
  method is_equal (l1, c1) (l2, c2) =
    Term.literal_equal l1 l2 && Term.clause_equal c1 c2

  method to_string (l, c) =
    Term.literal_to_string l ^ " : " ^ Term.clause_to_string c
end

let create_literal_clause_index (productivity: bool) : (literal * clause) index =
  new index productivity (new literal_clause_data)

