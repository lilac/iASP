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


(*** Types ***)

type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type binding = Subst.binding
type subst = Subst.subst
type subst_var = Subst.var

exception UNIFICATION_FAIL (*of subst*)



(* Like in Subst it pays off performance wise
   to precompile p_preserving and i_preserving in specialized modules *)
module type Unification = 
sig
  val match_terms: term -> int -> term -> int -> subst
  val unify_terms: subst -> term -> int -> term -> int -> subst
  val match_substs: int -> subst -> subst -> subst
  val unify_substs: subst -> subst -> subst
end


module type Options =
sig
  val recompute: bool
  val p_preserving: bool
end


(* functor *)
module Make (Options: Options) : Unification =
struct

  (*** Matching ***)
  let subst_set subst var term =
    if not Options.recompute && Options.p_preserving then
      Subst.PPreserving.set subst var term

    else if not Options.recompute && not Options.p_preserving then
      Subst.Preserving.set subst var term

    else if Options.recompute && Options.p_preserving then
      Subst.RPPreserving.set subst var term

    else (* if Options.recompute && not Options.p_preserving then *)
      Subst.RPreserving.set subst var term


  let subst_set_ subst var offset1 term2 offset2 =
    if not Options.recompute && Options.p_preserving then
      Subst.PPreserving.set' subst var offset1 term2 offset2

    else if not Options.recompute && not Options.p_preserving then
      Subst.Preserving.set' subst var offset1 term2 offset2

    else if Options.recompute && Options.p_preserving then
      Subst.RPPreserving.set' subst var offset1 term2 offset2

    else (* if Options.recompute && not Options.p_preserving then *)
      Subst.RPreserving.set' subst var offset1 term2 offset2

  let rec do_match_terms (subst: subst) (term1: term) (offset1: int) (term2: term) (offset2: int) : subst =
    
    if (offset1 == offset2) && (Term.term_equal term1 term2) then
      subst
    else
      match term1, term2 with
	| Term.Var var, _ ->
	    begin
	      match Subst.get' subst var offset1 with
		| Some bound_term ->
		    do_match_terms
		      subst
		      bound_term.Subst.st_term		      bound_term.Subst.st_offset
		      term2
		      offset2
		      
		| None ->
		    (* term1 is part of the term to match and may thus not be bound. *)
		    if offset1 == offset2 then
		      raise (UNIFICATION_FAIL (*subst*))
			
		    (* var can be bound if p preserving doesn't apply *)
		    else
		      subst_set_ subst var offset1 term2 offset2
	  end
	      
      | Term.Const symbol1, Term.Const symbol2 when 
	  Symbol.equal symbol1 symbol2 ->
	  
	  subst
	      
      | Term.Func func1, Term.Func func2 when
	  Symbol.equal func1.Term.symbol func2.Term.symbol ->
	  if func1.Term.ground && func2.Term.ground then begin
	    if Term.term_equal term1 term2 then
	      subst
	    else
	      raise UNIFICATION_FAIL
	  end
	  else
	    Tools.array_fold2
	      (fun subst term1 term2 ->
		 do_match_terms
		   subst term1 offset1 term2 offset2
	      )
	      subst
	      func1.Term.subterms
	      func2.Term.subterms
	      
      | _ -> 
	  raise UNIFICATION_FAIL


  let match_terms (term1: term) (offset1: int) (term2: term) (offset2: int) : subst =
    try
      do_match_terms
	Subst.empty term1 offset1 term2 offset2
    with
      | Subst.SET_FAIL ->
	  raise UNIFICATION_FAIL







  (*** Unification ***)
	  
	  
  let rec do_unify_terms (subst: subst)
      (term1: term) (offset1: int) (term2: term) (offset2: int) : subst =

    if (offset1 == offset2) && (Term.term_equal term1 term2) then 
      subst
    else
      begin
	match term1, term2 with
	  | Term.Var var1, Term.Var var2 ->
	      begin
		(* dereference bound variables *)
		match Subst.get' subst var1 offset1, Subst.get' subst var2 offset2 with
		  | Some bound_term1, Some bound_term2 ->
		      do_unify_terms
			subst
			bound_term1.Subst.st_term
			bound_term1.Subst.st_offset
			bound_term2.Subst.st_term
			bound_term2.Subst.st_offset
		      
		  | Some bound_term1, None ->
		      do_unify_terms
			subst
			bound_term1.Subst.st_term
			bound_term1.Subst.st_offset
			term2
			offset2
			
		  | None, Some bound_term2 ->
		      do_unify_terms
			subst
			term1
			offset1
			bound_term2.Subst.st_term
			bound_term2.Subst.st_offset
			
		  | None, None ->
		      (* try to bind a universal variable *)
		      if Var.is_universal var1 then
			subst_set_ subst var1 offset1 term2 offset2
			
		      (* bind the other variable, unversal or not *)
		      else
			subst_set_ subst var2 offset2 term1 offset1
	      end
		
	  | Term.Var var1, _ ->
	      begin
		(* dereference bound variable *)
		match Subst.get' subst var1 offset1 with
		  | Some bound_term1 ->
		      do_unify_terms
			subst
			bound_term1.Subst.st_term
			bound_term1.Subst.st_offset
			term2
			offset2
		      
		  | None ->
		      subst_set_ subst var1 offset1 term2 offset2
	      end
		
	  | _, Term.Var var2 ->
	      begin
		(* dereference bound variable *)
		match Subst.get' subst var2 offset2 with
		  | Some bound_term2 ->
		      do_unify_terms
			subst
			term1
			offset1
			bound_term2.Subst.st_term
			bound_term2.Subst.st_offset
		      
		  | None ->		    
		      subst_set_ subst var2 offset2 term1 offset1
	      end
		
	  | Term.Const symbol1, Term.Const symbol2 when
	      Symbol.equal symbol1 symbol2 ->
	      
	      subst
		
		
	  | Term.Func func1, Term.Func func2 when
	      Symbol.equal func1.Term.symbol func2.Term.symbol ->

	      if func1.Term.ground && func2.Term.ground then begin
		if Term.term_equal term1 term2 then
		  subst
		else
		  raise UNIFICATION_FAIL
	      end
	      else
	      
	      Tools.array_fold2
		(fun subst term1 term2 ->
		   do_unify_terms
		     subst
		     term1
		     offset1
		     term2
		     offset2
		)
		subst
		func1.Term.subterms
		func2.Term.subterms
		
	  | _ ->
	      raise UNIFICATION_FAIL
      end
    

  let unify_terms
      (subst: subst) (term1: term) (offset1: int) (term2: term) (offset2: int) : subst =
    try  
      do_unify_terms subst term1 offset1 term2 offset2
    with
      | Subst.SET_FAIL (*subst*) ->
	  raise (UNIFICATION_FAIL (*subst*))







  (*** Merging ***)
	    
  (* does subst contain binding reversed? *)	
  let is_included_reversed (subst: subst) (binding: binding) : bool =
    match binding.Subst.sb_term.Subst.st_term with
      | Term.Var term_var ->
	  Subst.exists
	    (fun current ->
	       (current.Subst.sb_var.Subst.sv_offset == binding.Subst.sb_term.Subst.st_offset)
	       &&
	       (Var.equal current.Subst.sb_var.Subst.sv_var term_var)
	       &&  
	       (current.Subst.sb_term.Subst.st_offset == binding.Subst.sb_var.Subst.sv_offset)
	       &&
		 (match current.Subst.sb_term.Subst.st_term with
		    | Term.Var binding_bound_var when
			Var.equal binding_bound_var binding.Subst.sb_var.Subst.sv_var ->
		     	true
			  
		    | _ ->
			false
		 )
	    )
	    subst
	    
      | _ ->
	  false
          

  let do_unify_substs (acc: subst) (binding: binding) : subst =
    
    let var, term =
      binding.Subst.sb_var, binding.Subst.sb_term
    in
      match Subst.get acc var with
	| None ->
	    if is_included_reversed acc binding then
	      acc
	    else
	      (* var is unbound till now, so just keep its binding *)
	      subst_set acc var term
	      
	| Some acc_term ->
	    (* var is already bound, so unify the existing bindings *)
	    do_unify_terms
	      acc
	      term.Subst.st_term term.Subst.st_offset
	      acc_term.Subst.st_term acc_term.Subst.st_offset

  let unify_substs (first: subst) (second: subst) : subst =
    if Subst.is_empty first then
      second
    else if Subst.is_empty second then
      first
    else
      begin
        try
	  let subst =
	    Subst.fold
	      (fun acc binding ->
		 do_unify_substs acc binding
	      )
	      first
	      second
	  in
	    subst
        with
          | Subst.SET_FAIL ->
	      raise (UNIFICATION_FAIL)
      end
	    




  let do_match_substs (more_general_offset: int) (acc: subst) (subst: subst) : subst =
    Subst.fold
      (fun (acc: subst) binding ->
	 let var, term =
	   binding.Subst.sb_var, binding.Subst.sb_term
	 in
	   match Subst.get acc var with
	     | None ->
		 (* var is unbound till now, so just keep its binding *)
		 subst_set acc var term
	       
	     | Some acc_term ->
		 (* var is already bound, so unify the existing bindings *)
		 if
		   term.Subst.st_offset == more_general_offset
		   &&
		   acc_term.Subst.st_offset == more_general_offset
		 then begin
		   (* unify *)
		   do_unify_terms
		     acc
		     term.Subst.st_term term.Subst.st_offset
		     acc_term.Subst.st_term acc_term.Subst.st_offset
		 end

		 else if term.Subst.st_offset == more_general_offset then begin
		   (* match term to acc *)
		   do_match_terms
		     acc
		     term.Subst.st_term term.Subst.st_offset
		     acc_term.Subst.st_term acc_term.Subst.st_offset
		 end
		   
		 else if acc_term.Subst.st_offset == more_general_offset then begin
		   (* match acc to term *)
		   do_match_terms
		     acc
		     acc_term.Subst.st_term acc_term.Subst.st_offset
		     term.Subst.st_term term.Subst.st_offset
		 end
		   
		 else begin
		   (* both terms part of the term to match -
		      no variables can be bound *)
		   if
		     acc_term.Subst.st_offset == term.Subst.st_offset
		     &&
		     Term.term_equal term.Subst.st_term acc_term.Subst.st_term
		   then
		     acc
		   else
		     raise (UNIFICATION_FAIL )
		 end
		   
      )
      acc
      subst


  let match_substs (more_general_offset: int) (first: subst) (second: subst) : subst =
    if Subst.is_empty first then
      second

    else if Subst.is_empty second then
      first

    else
      begin
        try
          do_match_substs more_general_offset first second
        with
          | Subst.SET_FAIL  ->
	      raise (UNIFICATION_FAIL )
      end
end

  
module UnificationPPreserving =
  Make (
    struct
      let recompute = false
      let p_preserving = true
    end
  )

module Unification_Preserving =
  Make (
    struct
      let recompute = false
      let p_preserving = false
    end
  )

module UnificationRPPreserving =
  Make (
    struct
      let recompute = not Const.debug
      let p_preserving = true
    end
  )

module Unification_RPreserving =
  Make (
    struct
      let recompute = not Const.debug
      let p_preserving = false
    end
  )



(* the general wrapper functions for the spezialiced modules *)
let match_terms ~(recompute: bool) ?(p_preserving: bool = false)
  (term1: term) (offset1: int) (term2: term) (offset2: int) : subst =

  if not recompute && p_preserving then
    UnificationPPreserving.match_terms term1 offset1 term2 offset2

  else if not recompute && not p_preserving then
    Unification_Preserving.match_terms term1 offset1 term2 offset2

  else if recompute && p_preserving then
    UnificationRPPreserving.match_terms term1 offset1 term2 offset2

  else
    Unification_RPreserving.match_terms term1 offset1 term2 offset2

let match_literals ~(recompute: bool) ?(p_preserving: bool = false)
  (literal1: literal) (offset1: int) (literal2: literal) (offset2: int) : subst =

  if (literal1.Term.sign == literal2.Term.sign) then
    match_terms
      ~recompute:recompute ~p_preserving:p_preserving
      literal1.Term.atom offset1 literal2.Term.atom offset2
  else
    raise (UNIFICATION_FAIL )


let is_term_generalization ?(p_preserving: bool = false)
    (general: term) (instance: term) : bool =
  try
    ignore (
      match_terms ~recompute:false ~p_preserving:p_preserving
        general 0 instance 1 : subst
    );
    true
  with
    | UNIFICATION_FAIL ->
        false

let is_term_instance ?(p_preserving: bool = false)
    (instance: term) (general: term) : bool =
  is_term_generalization ~p_preserving:p_preserving general instance
	  
let is_literal_instance ?(p_preserving: bool = false)
    (instance: literal) (general: literal) : bool =
  instance.Term.sign = general.Term.sign
  &&
  is_term_instance ~p_preserving:p_preserving general.Term.atom instance.Term.atom

let is_literal_generalization ?(p_preserving: bool = false)
    (general: literal) (instance: literal) : bool =
  is_literal_instance ~p_preserving:p_preserving instance general



let unify_terms_ ~(recompute: bool) ?(p_preserving: bool = false)
  (subst: subst) (term1: term) (offset1: int) (term2: term) (offset2: int) : subst =

  if not recompute && p_preserving then
    UnificationPPreserving.unify_terms subst term1 offset1 term2 offset2

  else if not recompute && not p_preserving then
    Unification_Preserving.unify_terms subst term1 offset1 term2 offset2

  else if recompute && p_preserving then
    UnificationRPPreserving.unify_terms subst term1 offset1 term2 offset2

  else (* if recompute && not p_preserving then *)
    Unification_RPreserving.unify_terms subst term1 offset1 term2 offset2

let unify_terms ~(recompute:bool) ?(p_preserving: bool = false) =
  unify_terms_ ~recompute:recompute ~p_preserving:p_preserving Subst.empty
  

let unify_literals ~(recompute:bool) ?(p_preserving: bool = false)
  (literal1: literal) (offset1: int) (literal2: literal) (offset2: int) : subst =

  if literal1.Term.sign == literal2.Term.sign then
    unify_terms ~recompute:recompute ~p_preserving:p_preserving
      literal1.Term.atom offset1 literal2.Term.atom offset2
  else
    raise (UNIFICATION_FAIL )

let unify_literals_ ~(recompute:bool) ?(p_preserving: bool = false)
  (subst: subst) (literal1: literal) (offset1: int) (literal2: literal) (offset2: int) : subst =

  if literal1.Term.sign == literal2.Term.sign then
    unify_terms_ ~recompute:recompute ~p_preserving:p_preserving
      subst literal1.Term.atom offset1 literal2.Term.atom offset2
  else
    raise (UNIFICATION_FAIL )

let are_terms_unifiable ?(p_preserving: bool = false)
    (x: term) (y: term) : bool =
  try
    ignore (
      unify_terms ~recompute:false ~p_preserving:p_preserving
        x 0 y 1 : subst
    );
    true
  with
    | UNIFICATION_FAIL  ->
        false




let unify_constraints ~(recompute: bool) (constraints : ((literal * int) * (literal * int)) list) : subst =
  let unify_terms =
    if recompute then
      Unification_RPreserving.unify_terms
    else
      Unification_Preserving.unify_terms
  in
    List.fold_left
      (fun subst ((literal1, offset1), (literal2, offset2)) ->
	 unify_terms
	   subst
	   literal1.Term.atom offset1
	   literal2.Term.atom offset2
      )
      Subst.empty
      constraints



let unify_constraints' ~(recompute: bool) (constraints : ((literal * int) * (literal * int)) list) : Subst.hash_subst =
  let unifier =
    Subst.VarTable.create 1024
  in
  let unify_terms =
    if recompute then
      Unification_RPreserving.unify_terms Subst.empty
    else
      Unification_Preserving.unify_terms Subst.empty
  in

  let rec unify' to_unify =
    match to_unify with
      | [] ->
	  ()

      | binding :: tail ->
	  (* orientation avoids cycles *)
	  let binding =
	    Subst.orient binding
	  in
	  begin
	    try
	      let bound_term =
		Subst.VarTable.find unifier binding.Subst.sb_var
	      in
	      (* dereference *)
	      let bind_term = Subst.dereference' unifier binding.Subst.sb_term
	      and bound_term = Subst.dereference' unifier bound_term
	      in
	      let unifier' =
		unify_terms
		  bound_term.Subst.st_term bound_term.Subst.st_offset
		  bind_term.Subst.st_term bind_term.Subst.st_offset
	      in
		unify' unifier';
	    with
	      | Not_found ->
		  (* var still unbound, so just add binding *)
		  Subst.VarTable.add unifier binding.Subst.sb_var binding.Subst.sb_term;
	  end;
	  unify' tail		
  in
    (* unify the constraints *)
    List.iter
      (fun ((literal1, offset1), (literal2, offset2)) ->
	 let t1 = Subst.dereference' unifier (Subst.make_term literal1.Term.atom offset1)
	 and t2 = Subst.dereference' unifier (Subst.make_term literal2.Term.atom offset2)
	 in
	 let unifier' =
	   unify_terms
	     t1.Subst.st_term t1.Subst.st_offset
	     t2.Subst.st_term t2.Subst.st_offset
	 in
	   (* integrate the unifiers into the common unifier *)
	   unify' unifier'
      )
      constraints;

    unifier




let unify_substs ~(recompute: bool) ?(p_preserving: bool = false)
  (first: subst) (second: subst) : subst =

  if not recompute && p_preserving then
    UnificationPPreserving.unify_substs first second

  else if not recompute && not p_preserving then
    Unification_Preserving.unify_substs first second

  else if recompute && p_preserving then
    UnificationRPPreserving.unify_substs first second

  else (*if recompute && not p_preserving then*)
    Unification_RPreserving.unify_substs first second





  let unify_substs_shallow (first: subst) (second: subst) : unit =
    Subst.iter
      (fun binding ->
	 if binding.Subst.sb_var.Subst.sv_offset != Subst.input_literal_offset then
	   ()

	 else begin
	   match Subst.get first binding.Subst.sb_var with
	     | Some bound_term ->
		 begin
		   match binding.Subst.sb_term.Subst.st_term, bound_term.Subst.st_term with
		     | Term.Var _, _
		     | _, Term.Var _ ->
			 (* a variable is unifiable with any term *)
      			 ()
			   
		     | Term.Const s1, Term.Const s2
		     | Term.Func { Term.symbol = s1  } , Term.Func { Term.symbol = s2 } when
			 Symbol.equal s1 s2 ->
			 ()

		     | _ ->
			 raise (UNIFICATION_FAIL)
		 end
		     
	     | _ ->
		 ()
	 end
      )
      second


let match_substs ~(recompute:bool) ?(p_preserving: bool = false)
  (more_general_offset: int) (first: subst) (second: subst) : subst =

  if not recompute && p_preserving then
    UnificationPPreserving.match_substs more_general_offset first second

  else if not recompute && not p_preserving then
    Unification_Preserving.match_substs more_general_offset first second

  else if recompute && p_preserving then
    UnificationRPPreserving.match_substs more_general_offset first second

  else
    Unification_RPreserving.match_substs more_general_offset first second

let merge_substs (first: subst) (second: subst) : subst =
  if Subst.is_empty first then
    second
      
  else if Subst.is_empty second then
    first
      
  else
    begin
      Subst.fold
	(fun acc binding ->
	   let var, term =
	     binding.Subst.sb_var, binding.Subst.sb_term
	   in
	     match Subst.get acc var with
	       | None ->
		   (* var is unbound till now, so just keep its binding *)
		   Subst.append binding acc
		     
	       | Some acc_term ->
		   (* this binding does already exists *)
		   if Subst.term_equal term acc_term then begin
		     acc
		   end

		   (* same variable bound to different terms *)
		   else
		     raise (UNIFICATION_FAIL )
			 
	)
	first
	second
    end








(*** subsumption ***)  


let subsumes (_subsuming: clause) (_subsumed: clause) : bool =
  let unifiers =
    List.map
      (fun general ->
	 let unifiers =
	   List.fold_left
	     (fun acc instance ->
		try
		  let subst =
		    match_literals
		    ~recompute:false
		      general 0
		      instance 1
		  in
		  subst :: acc
		with
		  | (UNIFICATION_FAIL ) ->
		      acc
	     )
	     []
	     _subsumed
	 in
	   general, unifiers
      )
      _subsuming
  in
  let unifiers =
    List.sort
      (fun (_, x) (_, y) -> Tools.compare_int (List.length x) (List.length y))
      unifiers
  in

  let rec subsumes' unifiers merged =
    match unifiers with
      | [] ->
	  raise Exit

      | (_general, head) :: tail ->
	  List.iter
	    (fun subst ->
	       try
		 let merged' =
		   merge_substs merged subst
		 in
		   subsumes' tail merged'
	       with
		 | (UNIFICATION_FAIL ) ->
		     ()
	    )
	    head
  in
    try
      subsumes' unifiers Subst.empty;
      false
    with
      | Exit ->
	  true

    

(* algorithm taken from
   Gottlob, Fermueller. Removing redundancy from a clause.

   iteratively check for each clause literal:
   if the current condensed clause C (starting with the original clause)
   is subsumed by C' = C \ {L}, then C' is the new condensed clause.
*)
let condense (clause: clause) : clause =

  let rec condense' (condensed: clause) (to_process: literal list) : clause =
    match to_process with
      | [] ->
	  (* all clause literals checked, so this is the condensed literal *)
	  condensed

      | head :: tail ->
	  print_endline (Term.literal_to_string head);
	  (* a ground literal must be part of the condensed clause -
	     assuming no literal exists more than once in a clause. *)
	  if Term.is_term_ground head.Term.atom then begin
	    condense' condensed tail
	  end
	    
	  (* cheack precheck:
	     head must be more general than another clause literal. *)
	  else if
	    List.for_all
	      (fun x ->
		 (Term.literal_equal head x)
		 ||
		 begin
		   try
		     ignore (match_literals ~recompute:false head 0 x 1 : subst);
		     false
		   with
		     | UNIFICATION_FAIL  ->
			 true
		 end
	      )
	      condensed
	  then begin
	    condense' condensed tail
	  end


	  else begin
	    (* the potentially condensed clause *)
	    let condensed' =
	      Tools.list_remove_first
		(Term.literal_equal head)
		condensed
	    in
	      if subsumes condensed condensed' then
		(* yes, condensed *)
		condense' condensed' tail
	      else
		(* no, keep previous clause *)
		condense' condensed tail
	  end
  in
  (*condense' clause clause*)
  let ground, non_ground =
    List.partition Term.is_literal_ground clause
  in
  let condensed =
    condense' non_ground non_ground
  in
    ground @ condensed
