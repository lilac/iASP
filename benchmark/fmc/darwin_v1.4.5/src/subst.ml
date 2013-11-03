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

type symbol = Symbol.symbol
type literal = Term.literal

type var = {
  sv_var: Var.var;
  sv_offset: int;
}

type term = {
  st_term: Term.term;
  st_offset: int;
}

type binding = {
  sb_var: var;
  sb_term: term;
}
    





(*** offsets used in a context unifier substitution *)

(* literals of the input clause *)
let input_literal_offset  =
  0

(* fresh parameters introduced when making the substitution admissible,
   i.e. var/par unmixing *)
let fresh_par_offset =
  1

(* offset of the context literal paired to the index.th input clause literal *)
let context_literal_offset (index: int) =
  2 + index







(*** SUBST: immutable *)


type subst = 
    binding list



(*** equality ***)

let var_equal (var1: var) (var2: var) : bool =
  (var1.sv_offset == var2.sv_offset)
  &&
  (Var.equal var1.sv_var var2.sv_var)
	
let var_equal' (var1: Var.var) (offset1: int) (var2: var) : bool =
  (offset1 == var2.sv_offset)
  &&
  (Var.equal var1 var2.sv_var)

let term_equal (term1: term) (term2: term) : bool =
  (term1.st_offset == term2.st_offset)
  &&
  (Term.term_equal term1.st_term term2.st_term)

let binding_equal (binding1: binding) (binding2: binding) : bool =
  (var_equal binding1.sb_var binding2.sb_var)
  &&
  (term_equal binding1.sb_term binding2.sb_term)

(* for testing only *)
let subst_equal (subst1: subst) (subst2: subst) : bool =
  subst1 == subst2
  ||
  Tools.lists_unordered_equal
    binding_equal
    subst1
    subst2


let binding_compare binding1 binding2 =
  if binding1.sb_var.sv_offset != binding2.sb_var.sv_offset then
    Tools.compare_int binding1.sb_var.sv_offset binding2.sb_var.sv_offset
  else if binding1.sb_term.st_offset != binding2.sb_term.st_offset then
    Tools.compare_int binding1.sb_term.st_offset binding2.sb_term.st_offset
  else if not (Var.equal binding1.sb_var.sv_var binding2.sb_var.sv_var) then
    Var.compare binding1.sb_var.sv_var binding2.sb_var.sv_var
  else
    Term.compare_terms binding1.sb_term.st_term binding2.sb_term.st_term

let rec subst_compare subst1 subst2 =
  match subst1, subst2 with
    | [], [] ->
	0

    | [], _ ->
	-1

    | _, [] ->
	1

    | binding1 :: tail1, binding2 :: tail2 ->
	let binding_cmp =
	  binding_compare binding1 binding2
	in
	  if binding_cmp == 0 then
	    subst_compare tail1 tail2
	  else
	    binding_cmp


module VarTable =
  Hashtbl.Make (
    struct
      type t = var

      let equal = var_equal

      let hash var =
	var.sv_offset * 131 + Var.hash_of_var var.sv_var
    end
  )

type mapping = Term.term VarTable.t
type hash_subst = term VarTable.t


exception SET_FAIL


(*** creation ***)

let empty =
  []


let make_var (var: Var.var) (offset: int) : var =
  {
    sv_var = var;
    sv_offset = offset;
  }

let make_term (term: Term.term) (offset: int) : term =
  {
    st_term = term;
    st_offset = offset;
  }

let make_binding var term = {
  sb_var = var;
  sb_term = term;
}





(*** append to a substitution ***)

let append binding subst =
   binding :: subst




(*** general helper functions ***)

let fold =
  List.fold_left

let iter =
  List.iter

let map =
  List.map

let map' =
  List.map

let exists =
  List.exists

let for_all =
  List.for_all

let length =
  List.length

let is_empty subst =
  subst == empty

let find =
  List.find

let find_all =
  List.find_all

let partition =
  List.partition

let first subst : binding =
  match subst with
    | binding :: _ ->
	binding

    | [] ->
	failwith "Subst.first"


  




(*** string representation ***)

let var_to_string (var: var) : string =
  string_of_int var.sv_offset ^ ": " ^ Var.to_string var.sv_var
	  
let term_to_string (term: term) : string = 
  string_of_int term.st_offset ^ ": " ^ Term.term_to_string term.st_term

let binding_to_string (binding: binding) : string =
  "[ "
  ^ var_to_string binding.sb_var
  ^ " -> "
  ^ term_to_string binding.sb_term
  ^ " ]"

let subst_to_string (subst: subst) : string =
  " { \n"
  ^ String.concat "\n"
    (map' binding_to_string subst)
  ^ " }" 







(*** consistency checks ***)

let check_binding (binding: binding) =
  if Const.debug then
    (* is a var bound to itself? *)
    begin
      if binding.sb_var.sv_offset = binding.sb_term.st_offset then begin
	match binding.sb_term.st_term with
	  | Term.Var var2 when
	      Var.equal binding.sb_var.sv_var var2
	      ->
	      failwith "Subst.append 1";

	  | _ ->
	      ()
	end
    end
      
let check_subst subst =
  if Const.debug then begin
    List.iter check_binding subst
  end






(*** access ***)

let rec get' (subst: subst) (var: Var.var) (offset: int) : term option =
  match subst with
    | [] ->
	None

    | binding :: tail ->
	if 
	  (offset == binding.sb_var.sv_offset)
	  &&
	  (Var.equal var binding.sb_var.sv_var)
	then
	  Some binding.sb_term
	else
	  get' tail var offset

let get (subst: subst) (var: var) : term option =
  get' subst var.sv_var var.sv_offset




let orient (binding: binding) : binding =
  let var, term =
    binding.sb_var, binding.sb_term
  in
    if var.sv_offset < term.st_offset then
      binding

    else begin
      let replace_binding var' =
	  make_binding
	    (make_var var' term.st_offset)
	    (make_term (Term.request_var var.sv_var) var.sv_offset)
      in
        match term.st_term with
	  | Term.Var var' ->
	      if var.sv_offset > term.st_offset then
		replace_binding var'
		  
	      else if Var.is_universal var.sv_var && Var.is_parametric var' then
		binding
		  
	      else if Var.is_parametric var.sv_var && Var.is_universal var' then
		replace_binding var'

	      else if Var.id_of_var var.sv_var <= Var.id_of_var var' then
		binding
		  
	      else
		replace_binding var'

	  | _ ->
	      binding
      end

(* dereference a hash_subst binding *)
let rec dereference' (subst: hash_subst) (bound_term: term) : term option =
  match bound_term.st_term with
    | Term.Var bound_var ->
	begin
	  try
	    let var =
	      make_var bound_var bound_term.st_offset
	    in
	    let bound_term' =
	      VarTable.find subst var
	    in
	      begin
		match dereference' subst bound_term' with
		  | Some bound_term'' ->
		      VarTable.replace subst var bound_term'';
		      Some bound_term''
			
		  | None ->
		      VarTable.replace subst var bound_term';
		      Some bound_term'
	      end
	  with
	    | Not_found ->
		None
	end
	  
    | _ ->
	None

let dereference' (subst: hash_subst) (term: term) : term =
  match dereference' subst term with
    | None ->
	term

    | Some term ->
	term


(* caching var -> bound_vars information
   between calls from get_bound_vars2/3 didn't pay off *)
let rec get_bound_vars' (subst: subst) (vars: var list) (acc: var list) : var list =
  List.fold_left
    (fun acc var ->
       match get subst var with
	 | None ->
	     (* unbound, so return the variable itself *)
	     if List.exists (var_equal var) acc then
	       acc
	     else
	       var :: acc
		   
	 | Some term ->
	     (* unbound var, so return the variables in the bound term *)
	     let term_vars =
	       List.map
		 (fun term_var -> 
		    make_var term_var term.st_offset
		 )
		 (Term.vars_of_term term.st_term)
	     in
	       get_bound_vars'
		 subst
		 term_vars
		 acc
    )
    acc
    vars

let get_bound_vars (subst: subst) (vars: var list) : var list =
  get_bound_vars' subst vars []





let is_p_renaming (subst:subst) (offset: int) : bool =
  
  let rec is_p_renaming' (bindings: subst) (mapping: (var * term) list) : bool =
    match bindings with
      | [] ->
	  true
	    
      | binding :: tail ->
	  (* not an interesting variable *)
	  if binding.sb_var.sv_offset != offset then
	    is_p_renaming' tail mapping
	      
	  (* universal, so ok in any case *)
	  else if Var.is_universal binding.sb_var.sv_var then
	    is_p_renaming' tail mapping
	      
	  (* a parameter bound to an instance of the same term *)
	  else if binding.sb_term.st_offset == offset then
	    false

	  else
	    match binding.sb_term.st_term with
	      | Term.Var bound_var when Var.is_parametric bound_var ->
		  begin
		    try
		      (* extend the injective mapping *)
		      let mapping' =
			Tools.mapping_extend mapping
			  var_equal term_equal
			  binding.sb_var binding.sb_term
		      in
			is_p_renaming' tail mapping'
		    with
		      | Exit ->
			  (* more than one variable bound to the same variable *)
			  false
		  end
		    
	      | _ ->
		  (* bound to a non-parameter *)
		  false
		    
  in
    is_p_renaming' subst []



(*** create a binding and add it to a substitution ***)

(* if term is a variable, return the term variable is bound to *)
let dereference (subst: subst) (term: term) : term =
  match term.st_term with
    | Term.Var var ->
	begin
	  match get' subst var term.st_offset with
	    | None ->
		term
		  
	    | Some bound_term ->
		bound_term
	end

    | _ ->
	term



(* raises SET_FAIL on a cyclic variable dependency.
   caching the already checked terms is  slower than
   the naive approach of computing them again and again *)
let rec occur_check' (subst: subst) (var: var) (term: Term.term) (term_offset: int) : unit =
  match term with
    | Term.Var term_var ->
	if ((var.sv_offset == term_offset) 
	    && 
	    (Var.equal var.sv_var term_var))
	then begin
	  raise SET_FAIL
	end
	else begin
	  match get' subst term_var term_offset with
	    | None -> 
		()

	    | Some term -> 
		occur_check' subst var term.st_term term.st_offset
	end

    | Term.Const _ ->
	()

    | Term.Func func ->
	Array.iter
	  (fun term ->
	     occur_check' subst var term term_offset
	  )
	  func.Term.subterms


(*** Set Modules ***)

(* interface *)
module type T_Preserving = 
sig
  val set: subst -> var -> term -> subst
  val set': subst -> Var.var -> int -> Term.term -> int -> subst
end

(* encapsulates the parameters to be compiled away *)
module type Options =
sig
  val recompute: bool
  val p_preserving: bool
end

(* functor *)
module Make (Options: Options) : T_Preserving =
struct
  

  (* raises SET_FAIL if the substitution can not be extended with var -> term *)
  let check_bindings (subst: subst) (var: var) (term: term) : unit =
    
    (* ensure var is not a parameter bound to a non-parameter *)
    if Options.p_preserving && Var.is_parametric var.sv_var then begin
      match term.st_term with
	| Term.Var bound_var when
	    Var.is_parametric bound_var
	    &&
	    var.sv_offset != term.st_offset
	    ->
	    ()

	| _ ->
	    raise SET_FAIL
    end;

    (* ensure that var is not already bound,
       and that the substitution remains p-preserving if required. *)
    if Options.p_preserving && Var.is_parametric var.sv_var then begin
      iter
	(fun binding ->
	   (* var already bound *)
	   if var_equal var binding.sb_var then
	     raise SET_FAIL
	     
	   (* there is already another parameter bound to the target term *)
	   else if Var.is_parametric binding.sb_var.sv_var && term_equal term binding.sb_term then begin
	     raise SET_FAIL
	   end
	   
	   (* there is already another parameter bound to the source parameter to bind,
	      so both will be bound to the same target term. *)
	   else begin	     
	     match binding.sb_term.st_term with 
	       | Term.Var bound_var ->
		   if
		     (var.sv_offset == binding.sb_term.st_offset)
		     &&
		     (Var.equal var.sv_var bound_var)
		     &&
		     Var.is_parametric binding.sb_var.sv_var
		   then
		     raise SET_FAIL
		       
	       | _ ->
		   ()
	   end
	)
	subst;
    end


  (* some variable in the substitution bound to var? *)
  let rec compression_needed (subst: subst) (var: var) : bool =
    match subst with
      | [] ->
	  false

      | binding :: tail ->
	  if var.sv_offset == binding.sb_term.st_offset then begin
	    match binding.sb_term.st_term with 
	      | Term.Var bound_var when
		  (Var.equal var.sv_var bound_var) ->
		  true
			
	      | _ ->
		  compression_needed tail var
	  end
	  
	  else
	    compression_needed tail var


  (* var is bound to term -
     so all variables already bound to var are rebound to term. *)
  let compress_old_bindings (subst: subst) (var: var) (term: term) : subst =
    map
      (fun binding ->
	 match binding.sb_term.st_term with 
	   | Term.Var bound_var
	       when
		 ((var.sv_offset = binding.sb_term.st_offset)
		  &&
		  (Var.equal var.sv_var bound_var)) ->
	       {
		 sb_var = binding.sb_var; 
		 sb_term = term;
	       }
		 
	   | _ ->
	       binding
      )
      subst
	


  (* binds a variable to a term,
     does occur check and path compression. *)
  let set (subst: subst) (var: var) (term: term) : subst =
    (* occur check for cyclic bindings
       only checking the new variable var suffices as for a cycle,
       as any new cycle must include var.
    *)
    if not Options.recompute then
      occur_check' subst var term.st_term term.st_offset;


    (* path compression: if term is a variable, bind to its binding instead *)
    let new_bound_term =
      dereference subst term
    in

    (* check if extension results in a valid substitution *)
    if not Options.recompute then
      check_bindings subst var new_bound_term;

    (* path compression: replace bindings to var by bindings to its new binding *)
    let compressed_subst =
      if compression_needed subst var then
	compress_old_bindings subst var new_bound_term
      else
	subst
    in
      
    let new_binding = { 
      sb_var = var;
      sb_term = new_bound_term;
    }
    in
      append new_binding compressed_subst

  let set' (subst: subst) (var: Var.var) (var_offset: int) (term: Term.term) (term_offset: int) : subst =
    set subst (make_var var var_offset) (make_term term term_offset)
end
  
  






module PPreserving =
  Make (
    struct
      let recompute = false
      let p_preserving = true
    end
  )

module Preserving =
  Make (
    struct
      let recompute = false
      let p_preserving = false
    end
  )

module RPPreserving =
  Make (
    struct
      let recompute = not Const.debug
      let p_preserving = true
    end
  )

module RPreserving =
  Make (
    struct
      let recompute = not Const.debug
      let p_preserving = false
    end
  )


(* the general set functions -
   are actually just a wrapper to the modules *)
let set ~(recompute: bool) ?(p_preserving:bool = false)
  (subst: subst) (var: var) (term: term) : subst =

  if not recompute && p_preserving then
    PPreserving.set subst var term

  else if not recompute && not p_preserving then
    Preserving.set subst var term
  
  else if recompute && p_preserving then
    RPPreserving.set subst var term

  else (*if recompute && not p_preserving then*)
    RPreserving.set subst var term

let set' ~(recompute: bool) ?(p_preserving:bool = false)
  (subst: subst) (var: Var.var) (var_offset: int) (term: Term.term) (term_offset: int) : subst =
  set ~recompute:recompute ~p_preserving:p_preserving
    subst (make_var var var_offset) (make_term term term_offset)
















(*** apply ***)

    
let rec consult_mapping (var: Var.var) (offset: int) (mapping: (var * Term.term) list)
    (mapping': (var * Term.term) list) :
    Term.term * ((var * Term.term) list) =
  match mapping with
    | [] ->
	let new_var =
	  (* the next id can be deduced from the number of remapped vars *)
	  Var.clone_renumbered var (List.length mapping')
	in
	let new_term =
	  Term.request_var new_var
	in
	  new_term, (make_var var offset, new_term) :: mapping'
	    
    | (var', bound) :: tail ->
	if var_equal' var offset var' then
	  bound, mapping'
	else
	  consult_mapping var offset tail mapping'


(* applies a substitution to a term and normalizes it.
   returns the normalization mapping. *)
let rec apply_to_term' ~(insert_db: bool) ~(normalize: bool)
    (subst: subst) (term: Term.term) (offset: int)
    (mapping: (var * Term.term) list) : Term.term * ((var * Term.term) list) =
  
  match term with
    | Term.Var var ->
	begin
	  match get' subst var offset with
	    | None ->
		(* keep unbound variable *)
		if not normalize then
		  term, mapping

		(* normalize unbound variable *)
		else
		  consult_mapping var offset mapping mapping

	    | Some term -> 
		(* replace bound variable *)
		apply_to_term' ~insert_db:insert_db ~normalize:normalize
		  subst term.st_term term.st_offset mapping
	end

    | Term.Const _ -> 
	term, mapping

    | Term.Func func ->
	let new_terms =
	  Array.copy func.Term.subterms
	in

	let rec do_at (i : int) mapping =
	  if i >= Array.length new_terms then
	    mapping
	  else
	    let new_term, mapping' =
	      apply_to_term' ~insert_db:insert_db ~normalize:normalize
		subst func.Term.subterms.(i) offset mapping
	    in
	      new_terms.(i) <- new_term;
	      do_at (i + 1) mapping'
	in
	  
	(* side effect on new_terms *)
	let mapping' =
	  do_at 0 mapping
	in
	let term' =
	  Term.request_func ~insert_db:insert_db (func.Term.symbol, new_terms)
	in
	  term', mapping'


let rec apply_to_literal' ~(insert_db: bool) ~(normalize: bool)
    (subst: subst) (literal: Term.literal) (offset: int)
    (mapping: (var * Term.term) list) : Term.literal * ((var * Term.term) list) =
  let term', mapping' =
    apply_to_term' ~insert_db:insert_db ~normalize:normalize
      subst literal.Term.atom offset mapping
  in
    Term.request_literal literal.Term.sign term',
    mapping'

let apply_to_literals' (subst: subst) (literals: (literal * int) list)
    (mapping: (var * Term.term) list) : Term.literal list * ((var * Term.term) list) =
  let (mapping', literals') =
    List.fold_left
      (fun (mapping, literals') (literal, offset) ->
	(* just normalize with empty substitution *)
	let literal', mapping' =
	  apply_to_literal' ~insert_db:true ~normalize:true subst literal offset mapping
	in
	  (mapping', literal' :: literals')
      )
      (mapping, [])
      literals
  in
    List.rev literals', mapping'


let apply_to_term  ?(insert_db: bool = true) ?(normalize: bool = true)
    (subst: subst) (term: Term.term) (offset: int) : Term.term =
  fst (apply_to_term' ~insert_db:insert_db ~normalize:normalize subst term offset [])

let apply_to_literal ?(insert_db: bool = true) ?(normalize: bool = true)
    (subst: subst) (literal: Term.literal) (offset: int) : Term.literal =
  let term =
    apply_to_term ~insert_db:insert_db ~normalize:normalize subst literal.Term.atom offset
  in
    Term.request_literal ~insert_db:insert_db literal.Term.sign term

let apply_to_clause (subst: subst) (clause: Term.clause) (offset: int) : Term.clause =
  fst (apply_to_literals' subst (List.map (fun literal -> (literal, offset)) clause) [])

let apply_to_literals (subst: subst) (literals: (literal * int) list) : literal list =
  fst (apply_to_literals' subst literals [])

let apply_to_literals_groups (subst: subst) (groups: (literal * int) list list) : literal list list =
  let (_, groups') =
    List.fold_left
      (fun (mapping, groups') literals ->
	(* just normalize with empty substitution *)
	let literals', mapping' =
	  apply_to_literals' subst literals mapping
	in
	  (mapping', literals' :: groups')
      )
      ([], [])
      groups
  in
    List.rev groups'





let normalize_term (term: Term.term) : Term.term  =
  apply_to_term ~insert_db:true ~normalize:true
    empty term 0

let normalize_literal (literal: Term.literal) : Term.literal  =
  apply_to_literal ~insert_db:true ~normalize:true
    empty literal 0

let normalize_clause (clause: Term.clause) : Term.clause  =
  apply_to_clause empty clause 0




(* like apply_to_term' for a hash_subst *)
let rec apply_to_term'' ?(insert_db:bool = true) (subst: hash_subst) (term: Term.term) (offset: int)
  (mapping: mapping) : Term.term =
  
  match term with
    | Term.Var var ->
	begin
	  try
	    (* replace var with binding *)
	    let term =
	      dereference' subst (VarTable.find subst (make_var var offset))
	    in
	      apply_to_term'' ~insert_db:insert_db subst term.st_term term.st_offset mapping

	  with
	    | Not_found ->
		(* normalize var *)
		begin
		  try
		    (* replace with existing mapping *)
		    VarTable.find mapping (make_var var offset)
		  with
		    | Not_found ->
			(* create new mapping *)
			let new_var =
			  (* the next id can be deduced from the number of remapped vars *)
			  Var.clone_renumbered var (VarTable.length mapping)
			in
			let new_term =
			  Term.request_var new_var
			in
			  VarTable.add mapping (make_var var offset) new_term;
			  new_term
		end
	end

    | Term.Const _ -> 
	term

    | Term.Func func(*(symbol, terms, _)*) ->
	let new_terms =
	  Array.copy func.Term.subterms
	in

	let rec do_at (i : int) =
	  if i >= Array.length new_terms then
	    ()
	  else
	    let new_term =
	      apply_to_term'' ~insert_db:insert_db subst func.Term.subterms.(i) offset mapping
	    in
	      new_terms.(i) <- new_term;
	      do_at (i + 1)
	in	  
	  (* side effect on new_terms *)
	  do_at 0;
	  Term.request_func ~insert_db:insert_db (func.Term.symbol, new_terms)



let apply_to_literals'' (subst: hash_subst) (literals: (literal * int) list) (mapping: mapping)  : literal list =
  let applied =
    List.fold_left
      (fun acc (literal, offset) ->
	let new_term =
	  apply_to_term'' subst literal.Term.atom offset mapping
	in
	let new_literal =
	  Term.request_literal literal.Term.sign new_term
	in
	  new_literal :: acc
      )
      []
      literals
  in
    List.rev applied


let apply_to_literal_groups' (subst: hash_subst) (groups: (literal * int) list list) : literal list list =
  let mapping =
    VarTable.create 1024
  in
  let applied =
    List.fold_left
      (fun acc group ->
	apply_to_literals'' subst group mapping :: acc
      )
      []
      groups
  in
    List.rev applied





let replace_binding_offset (binding: binding) (old_offset: int) (new_offset: int) : binding =
  if
    binding.sb_var.sv_offset = old_offset
    &&
    binding.sb_term.st_offset = old_offset
  then
    { 
      sb_var = {
	sv_var = binding.sb_var.sv_var;
	sv_offset = new_offset;
      };
      sb_term = {
	st_term = binding.sb_term.st_term;
	st_offset = new_offset;
      };
    }
  else if
    binding.sb_var.sv_offset = old_offset
  then
    { binding with
	sb_var = {
	  sv_var = binding.sb_var.sv_var;
	  sv_offset = new_offset;
	};
    }
    
  else if
    binding.sb_term.st_offset = old_offset
  then
    { binding with
	sb_term = {
	  st_term = binding.sb_term.st_term;
	  st_offset = new_offset;
	};
    }
    
  else
    binding


let replace_offset (subst: subst) (old_offset: int) (new_offset: int) : subst =
  map
    (fun binding -> replace_binding_offset binding old_offset new_offset)
    subst


let replace_in_bound_terms (subst: subst) (old_term: Term.term) (new_term: Term.term) : subst =
  if Term.term_equal old_term new_term then
    subst
  else
    map
      (fun binding ->
	 let new_bound_term =
	   Term.replace_term_in_term binding.sb_term.st_term old_term new_term
	 in
	   if Term.term_equal binding.sb_term.st_term new_bound_term then begin
	     binding
	   end

	   else begin
	     { binding with
		 sb_term = {
		   binding.sb_term with
		     st_term = new_bound_term
		 };
	     }
	   end
      )
      subst

let rec reverse_bindings (subst: subst) (vars_to_reverse: var list) (vars_to_keep: var list): subst =
  match vars_to_reverse with
    | [] ->
	subst
	  
    | var_to_reverse :: tail ->
	let rec find' list =
	  match list with
	    | [] ->
		(* the var to reverse is not bound, so ignore it *)
		reverse_bindings subst tail vars_to_keep
		  
	    | binding :: tail' ->
		if var_equal var_to_reverse binding.sb_var then
		  let binding_to_reverse =
		    binding
		  in
		    match binding_to_reverse.sb_term.st_term with
		      | Term.Var bound_par ->
			  if
			    (* the var is not bound to a par but to a var, so ignore it *)
			    (Var.is_universal bound_par)
			    ||
			    (* this parameter may not be reversed *)
			    (List.exists 
			       (var_equal (make_var bound_par binding_to_reverse.sb_term.st_offset))
			       vars_to_keep)
			  then
			    reverse_bindings subst tail vars_to_keep
			  else begin
			    if 
			      List.exists
				(fun binding ->
				   (* ignore the binding to reverse *)
				   (not (binding == binding_to_reverse))
				   &&
				   (* does this binding bind to the parameter to reverse? *)
				   term_equal binding.sb_term binding_to_reverse.sb_term
				   &&
				   (* and is this binding a parameter which may not be reversed? *)
				   List.exists (var_equal binding.sb_var) vars_to_keep
				)
				subst
			    then
			      (* reversing this var -> par would also reverse a par which may not be reversed *)
			      reverse_bindings subst tail vars_to_keep
			    else begin
			      (* ok, now the binding can be reversed *)
			      let new_bound_term =
				make_term (Term.request_var var_to_reverse.sv_var) var_to_reverse.sv_offset
			      in
				
			      let reversed_subst =
				map
				  (fun binding ->
				     (* replace the old binding by its reversed binding *)
				     if binding == binding_to_reverse then
				       {
					 sb_var = make_var bound_par binding_to_reverse.sb_term.st_offset;
					 sb_term = new_bound_term;
				       }
					      
				     (* replace a binding to the reversed var *)
				     else if term_equal binding.sb_term binding_to_reverse.sb_term then
				       { binding with
					   sb_term = new_bound_term;
				       }
					 
				     else
				       binding
				  )
				  subst
			      in
				check_subst reversed_subst;
				reverse_bindings reversed_subst tail vars_to_keep
			    end
			  end
			    
		      | _ ->
			  (* not bound to a variable *)
			  reverse_bindings subst tail vars_to_keep
			    
		else
		  find' tail'
		    
	in
	  find' subst

(* ensure that universal renamings go from context to clause vars,
   and not vice versa *)
let normalize_var_renamings (subst: subst) : subst =
  (* find all mappings from universal clause to context vars *)
  let renamings =
  fold
    (fun renamings binding ->
       if
	 (* from clause literal *)
	 binding.sb_var.sv_offset == input_literal_offset
	 &&
	 (* to context literal *)
	 binding.sb_term.st_offset != input_literal_offset
	 &&
	 (* from a universal variable *)
	 Var.is_universal binding.sb_var.sv_var
	 
	 then
	   match binding.sb_term.st_term with
	       (* to a universal variable *)
	    | Term.Var var when Var.is_universal var ->
		(* which is not bound to by any other variable *)
		if
		  for_all
		    (fun binding' ->
		      binding_equal binding binding'
		      ||
	              not (term_equal binding.sb_term binding'.sb_term)
		    )
		    subst
		then
		  (binding.sb_var, var, binding.sb_term.st_offset)
		  :: renamings
		else
		  renamings
		  
	    | _ ->
		renamings
       else
	 renamings
    )
    []
    subst
  in

  (* swap the found renamings *)
  let renamed =
    List.map
      (fun (var, bound_var, bound_offset) ->
	 make_binding
	   (make_var bound_var bound_offset)
	   (make_term (Term.request_var var.sv_var) input_literal_offset)
      )
      renamings
  in
    begin
      match renamings with
	| [] ->
	    subst

	| _ ->
	    fold
	      (fun acc binding ->
		 (* remove renamings *)
		 if
		   List.exists
		     (fun (from, _, _) -> var_equal from binding.sb_var)
		     renamings
		 then
		   acc

		 (* apply renaming *)
		 else begin
		   match binding.sb_term.st_term with
		     | Term.Var bound_var ->
			 begin
			   try
			     let from, _, _ =
			       List.find
				 (fun (_, to_var, to_offset) ->
				    binding.sb_term.st_offset == to_offset
				    &&
				    Var.equal bound_var to_var
				 )
				 renamings
			     in
			     let reversed =
			       make_binding
				 binding.sb_var
				 (make_term
				    (Term.request_var from.sv_var)
				    from.sv_offset
				 )
			     in
			       reversed :: acc
			   with
			     | Not_found ->
				 binding :: acc
			 end

		     | _ ->
			 binding :: acc
		 end
	      )
	      renamed
	      subst
    end

let remove_context_var_renamings (subst: subst) : subst =
  let subst =
    normalize_var_renamings subst
  in
  List.find_all
    (fun binding ->
       (* keep all clause literal variables -
          this are used to actually instantiate the remainder literals
	  (Selection_assert, Selection_split, Context_unifier_check) *)
       binding.sb_var.sv_offset == input_literal_offset
       ||
       (
	 (* keep if ... *)
	 List.exists
	   (fun binding' ->
	      (* ... another variable *)
	      binding' != binding
	      &&
	      (* ... is bound to a context term *)
	      binding'.sb_term.st_offset == binding.sb_var.sv_offset
              &&
	      (* ... containing this variable *)
	      Term.term_contains_var binding'.sb_term.st_term binding.sb_var.sv_var
	   )
	   subst
       )
    )
    subst
