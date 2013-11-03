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



(*** types ***)

type term = Term.term
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst


(* number of variables and parameters of a term *)
type pureness = {
  vars: int;
  pars: int;
}




(*** pureness ***)


let is_universal (pureness: pureness) : bool =
  pureness.pars = 0

let is_mixed (pureness: pureness) : bool =
  pureness.vars > 0 && pureness.pars > 0

let is_parametric (pureness: pureness) : bool =
  pureness.pars > 0

(* at least one parameter contained? *)
let rec is_term_parametric (term: term) : bool =
  match term with
    | Term.Var var ->
	Var.is_parametric var

    | Term.Const _ ->
	false

    | Term.Func func (*(_, terms, _)*) ->
	Tools.array_exists is_term_parametric func.Term.subterms


let is_propositional (literal: literal) =
  match literal.Term.atom with
    | Term.Const _ ->
	true

    | Term.Var _
    | Term.Func _ ->
	false

let cmp_pureness (first: pureness) (second: pureness) : int =
  if is_universal first == is_universal second then
    0
  else if is_universal first then
    -1
  else
    1


let cmp_universality (first: pureness) (second: pureness) : int =
  (* as few parameters as possible *)
  if first.pars < second.pars then
    -1
  else if first.pars > second.pars then
    1
  else 
    (* as many variables as possible -
       this check is supposed to be done for terms of equal term weight only,
       so this prefers smaller/shallower predicates/terms *)
    compare second.vars first.vars



let get_pureness (term: term) : pureness =
  let vars = ref 0
  and pars = ref 0
  in
  let rec do_get_pureness (term: term) : unit =
    match term with
      | Term.Func func(*(_, terms, _)*) ->
	  Array.iter
            do_get_pureness
            func.Term.subterms

      | Term.Var var ->
          if Var.is_universal var then
	    vars := !vars + 1
          else
	    pars := !pars + 1
	    
      | Term.Const _ ->
	  ()
  in

    do_get_pureness term;
    {
      vars = !vars;
      pars = !pars;
    }





(*** depth ***)

let rec depth_of_term (term: term) (depth: int) : int =
  match term with
    | Term.Var _ ->
	depth
	  
    | Term.Const _ ->
	depth
	  
    | Term.Func func(*(_, terms, _)*) ->
	let current_depth =
	  depth + 1
	in
	  Array.fold_left 
	    (fun acc term ->
	       Tools.max_int acc (depth_of_term term current_depth)
	    )
	    current_depth
	    func.Term.subterms
    

let depth_of_literal (literal: literal) =
  depth_of_term literal.Term.atom 0

let depth_of_term (term: term) =
  depth_of_term term 0


let rec depth_of_term_subst (subst: subst) (term: term) (offset: int) (depth: int) : int =
  match term with
    | Term.Var var ->
	begin
	  match Subst.get' subst var offset with
	    | None ->
		depth
		  
	    | Some bound_term ->
		depth_of_term_subst subst bound_term.Subst.st_term bound_term.Subst.st_offset depth
	end
	  
    | Term.Const _ ->
	depth

    | Term.Func func(*(_, terms, _)*) ->
	let current_depth =
	  depth + 1
	in
	  (Array.fold_left 
	     (fun acc term ->
		Tools.max_int acc (depth_of_term_subst subst term offset current_depth)
	     )
	     current_depth
	     func.Term.subterms
	  )

let depth_of_literal_subst (literal: literal) (subst: subst) (offset: int) =
  depth_of_term_subst subst literal.Term.atom offset 0











(*** weight ***)


let rec weight_of_term (term: term) (weight: int) : int =
  match term with
    | Term.Var _var ->
	weight
	  
    | Term.Const _ ->
	weight + 1
	  
    | Term.Func func(*(_, terms, _)*) ->
	Array.fold_left
	  (fun acc term ->
	     weight_of_term term acc
	  )
	  (weight + 1)
	  func.Term.subterms

let weight_of_literal (literal: literal) : int =
  weight_of_term literal.Term.atom 0



let rec weight_of_term_subst (subst: subst) (term: term) (offset: int) (weight: int) : int =
  match term with
    | Term.Var var ->
	begin
	  match Subst.get' subst var offset with
	    | None ->
		weight
		  
	    | Some bound_term ->
		weight_of_term_subst subst bound_term.Subst.st_term bound_term.Subst.st_offset weight
	end
	  
    | Term.Const _ ->
	weight + 1

    | Term.Func func(*(_, terms, _)*) ->
	Array.fold_left 
	  (fun acc term ->
	     weight_of_term_subst subst term offset acc
	  )
	  (weight + 1)
	  func.Term.subterms
	
 	    

let weight_of_literal_subst (literal: literal) (subst: subst) (offset: int) =
  weight_of_term_subst subst literal.Term.atom offset 0

let weight_of_clause (clause: clause) : int =
  List.fold_left
    (fun weight literal -> weight + weight_of_literal literal)
    0
    clause






let rec bounded_depth_of_term (term: term) (depth: int) (bound: int) : int =
  match term with
    | Term.Var _ ->
	depth
	  
    | Term.Const _ ->
	depth
	  
    | Term.Func func ->
	if depth >= bound then
	  raise Exit;

	let current_depth =
	  depth + 1
	in
	  Array.fold_left 
	    (fun acc term ->
	       Tools.max_int acc (bounded_depth_of_term term current_depth bound)
	    )
	    current_depth
	    func.Term.subterms
    

let bounded_depth_of_literal (literal: literal) (bound: int) : int =
  try
    bounded_depth_of_term literal.Term.atom 0 bound
  with
    | Exit ->
	bound



let rec bounded_depth_of_term_subst (subst: subst) (term: term) (offset: int) (depth: int) (bound: int) : int =
  match term with
    | Term.Var var ->
	begin
	  match Subst.get' subst var offset with
	    | None ->
		depth
		  
	    | Some bound_term ->
		bounded_depth_of_term_subst subst
		  bound_term.Subst.st_term bound_term.Subst.st_offset depth bound
	end
	  
    | Term.Const _ ->
	depth

    | Term.Func func ->
	if depth >= bound then
	  raise Exit;

	let current_depth =
	  depth + 1
	in
	  (Array.fold_left 
	     (fun acc term ->
		Tools.max_int acc
		  (bounded_depth_of_term_subst subst term offset current_depth bound)
	     )
	     current_depth
	     func.Term.subterms
	  )

let bounded_depth_of_literal_subst (literal: literal) (subst: subst) (offset: int) (bound: int) : int =
  try
    bounded_depth_of_term_subst subst literal.Term.atom offset 0 bound;
  with
    | Exit ->
	bound







let rec bounded_weight_of_term (term: term) (weight: int) (bound: int) : int =
  match term with
    | Term.Var _var ->
	weight
	  
    | Term.Const _ ->
	if weight >= bound - 1 then
	  raise Exit;
	weight + 1
	  
    | Term.Func func ->
	if weight >= bound - 1 then
	  raise Exit;

	Array.fold_left
	  (fun acc term ->
	     bounded_weight_of_term term acc bound
	  )
	  (weight + 1)
	  func.Term.subterms

let bounded_weight_of_literal (literal: literal) (bound: int) : int =
  try
    bounded_weight_of_term literal.Term.atom 0 bound
  with
    | Exit ->
	bound



let rec bounded_weight_of_term_subst (subst: subst) (term: term) (offset: int)
    (weight: int) (bound: int) : int =
  match term with
    | Term.Var var ->
	begin
	  match Subst.get' subst var offset with
	    | None ->
		weight
		  
	    | Some bound_term ->
		bounded_weight_of_term_subst subst bound_term.Subst.st_term bound_term.Subst.st_offset
		  weight bound
	end
	  
    | Term.Const _ ->
	if weight >= bound -1 then
	  raise Exit;

	weight + 1

    | Term.Func func ->
	if weight >= bound -1 then
	  raise Exit;

	Array.fold_left 
	  (fun acc term ->
	     bounded_weight_of_term_subst subst term offset acc bound
	  )
	  (weight + 1)
	  func.Term.subterms
	
 	    

let bounded_weight_of_literal_subst (literal: literal) (subst: subst) (offset: int) (bound: int) : int =
  try
    bounded_weight_of_term_subst subst literal.Term.atom offset 0 bound;
  with
    | Exit ->
	bound
