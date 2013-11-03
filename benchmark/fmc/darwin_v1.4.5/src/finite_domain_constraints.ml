type var = Var.var
type symbol = Symbol.symbol
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type sort = Sort_inference.sort
type sorts = Sort_inference.sorts
type finite_domain = Finite_domain.finite_domain


let print_debug = false


(*** first the static constraits ***)


(* the constraints on a constraint variable.

   a constraint variable of a clause is a variable that occurs
   in a constraint literal, i.e. equality or domain restriction *)
type var_constraint = {
  (* the constraint variable *)
  v_var: var;

  (* its sort *)
  v_sort: sort;

  (* the min domain element: only restricted by permutable restriction *)
  mutable v_domain_min : int;

  (* the max domain element: the current domain size *)
  mutable v_domain_max : int;

  (* .(i) = true -> domain element i removed from domain.
     .(0) has no meaning *)
  mutable v_blocked: bool array;

  (* the disequalities to other constraint variables.

     that is, if x = y is in the clause,
     then the clause is satisfied if both are instantiated to the same domain element.
     So, in order to compute a context unifier,
     that is in order to satisfy the constraints,
     x and y have to be instantiated to different domain elements.

     so, links represent disequality constraints between constraint variables.

     Need direction to vars better and worse in var ordering
     for propagation of constraints like x = 1. *)
  mutable v_links: var_constraint list;




  (* to avoid memory allocation some element who should be var_search are here,
     so this are wrt. to a context unifier *)

  (* the remaining domain elements
     after applying the constraints of the context unifier. *)
  (*vs_domain: int array;*)

  (* only the first n elements in vs_domain are valid domain elements *)
  mutable vs_domain_size: int;

  (* v_blocked extended to given substitution
     
     if vs_blocked.(0) is true the domain has been changed
     in the last search and has to be retracted. *)
  mutable vs_blocked: bool array;


  (* used in search *)

  (* mapping to the first element in vs_domain that is usable
     at the i.th level of the current search branch.

     that is, vs_first_element.(0) is 0.
     when the 0.st constrained variable is set to a value,
     then constraint propagation is applied to all linked constrained variables,
     and vs_first_element.(0 + 1) is set to the first still usable domain element,
     starting searching for it from vs_first_element.(0).
 
     This is done for the i.th variable instead of 0. *)
  mutable vs_first_element: int array;

  (* if this constraint variable is part of the explored search branch,
     then its current value is vs_domain.(vs_current_index) *)
  mutable vs_current_index: int;
}

(* for each sort there are a number of constraint variables,
   which are stored here. *)
type sort_constraint = {
  (* sort of the constraints *)
  s_sort: sort;

  (* the constraint variables,
     ordered to be traversed when searching for a solution:
     first, by increasing domain size then by decreasing connectivity *)
  s_vars: var_constraint list;
}

(* the constraints of a clause *)
type constraints = {
  (* the current finite domain representation,
     provides access to domain elements and domain size. *)
  finite_domain: finite_domain;

  (* the original clause *)
  clause: clause;

  (* the unconstrained part of the clause *)
  unconstrained: clause;

  (* the constrained part of the clause *)
  constrained: clause;

  (* the constraints of the clause *)
  sort_constraints: sort_constraint list;

  (* for faster access, mapping from constraint variables
     in clause to var_constraint representation (by var id) *)
  var_to_var_constraint: var_constraint option array;

  (* constraints unsatisfiable (probably only in current domain size) *)
  unsatisfiable: bool;
  
  (* some constraint variable are forced to have a specific value in any solution.
     these fixed mappings are stored here. *)
  base_subst: subst;
}





(*** now the search setup for a context unifier of the unconstrained clause part ***)


(* the search setup for a constraint variable *)
type var_search = {
  (* static setup *)

  (* the constraint variable *)
  vs_var: var_constraint;

  (* the links of the constraint variable
     reduced to the constraint variables remaining for the search. *)
  mutable vs_links: var_search list;
}


(* the constrained variables of the search.
   some variables are removed because of the additional constraints of the context unifier,
   i.e. when it is mapped to a constant or another variable. *)
type sort_search = {
  ss_vars: var_search array;
}


(* the search setup for a context unifier and a constrained clause *)
type search = {
  (* the constraints *)
  h_constraints: constraints;

  (* the context unifier for the unconstrained clause part *)
  h_subst: subst;

  (* the instantiation of constraint variables in subst *)
  h_mappings: Subst.term option array;

  (* sort_constraints specialized for the context unifier *)
  h_sort_search: sort_search list;

  (* flag showing that the constraints are (trivially) unsolvable *)
  h_unsat: bool;
}


(* raised when constraints are unsolvable with any context unifier *)
exception UNSATISFIABLE

(* raised when constraints are unsolvable wrt. to current context unifier *)
exception NO_SOLUTION

(* raised when only one solution is search for, and the first solution is found *)
exception FIRST_SOLUTION of subst

module VarTable = Var.VarTable
module SortTable = Sort_inference.SortTable


let is_unsatisfiable constraints =
  constraints.unsatisfiable

let base_subst constraints =
  constraints.base_subst

(* pair (i, j) - variable with id i is bound to domain element with id j *)
type solution =
    int array


let subst_to_solution subst : solution =
  let solution =
  Subst.fold
    (fun acc binding ->
       if Const.debug then begin
	 if not (
	   binding.Subst.sb_var.Subst.sv_offset == Subst.input_literal_offset
	   &&
	   binding.Subst.sb_term.Subst.st_offset == Subst.input_literal_offset
	 )
	 then
	   failwith "Finite_domain_constraints.subst_to_solution 1"
       end;

       let var =
	 Var.id_of_var binding.Subst.sb_var.Subst.sv_var
       in
       let element =
	 Finite_domain.get_id_of_domain_element binding.Subst.sb_term.Subst.st_term
       in
	 var :: element :: acc
    )
    []
    subst
  in
    Array.of_list solution


let solution_to_subst solution =
  let rec solution_to_subst' subst i =
    if i >= Array.length solution then
      subst
    else begin
      let var =
	Var.create_universal solution.(i)
      and element =
	Finite_domain.get_domain_element solution.(i + 1)
      in
      let binding =
	Subst.make_binding
	  (Subst.make_var var Subst.input_literal_offset)
	  (Subst.make_term element Subst.input_literal_offset)
      in
      let subst' =
	Subst.append binding subst
      in
	solution_to_subst' subst' (i + 2)
    end
  in
    solution_to_subst' Subst.empty 0  

let solution_compare solution0 solution1 =
  if Const.debug then begin
    if Array.length solution0 != Array.length solution1 then
      failwith "Finite_domain_constraints.solution_compare"
  end;

  if Array.length solution0 == 0 then
    0
  else
    let rec solution_compare' i =
      if i >= Array.length solution0 then
	0
      else if solution0.(i) < solution1.(i) then
	-1
      else if solution0.(i) > solution1.(i) then
	1
      else
	solution_compare' (i + 1)
    in
      solution_compare' 0


(*** to string ***)

let blocked_to_list blocked =
  let list = ref []
  in
    for i = 1 to Array.length blocked - 1 do
      if blocked.(i) then
	list := i :: !list;
    done;
    List.rev (!list)

let var_constraint_to_string indent var_constraint =
  (indent ^ "var_constraint: "
   ^ Var.to_string var_constraint.v_var
   ^ " : " ^ Sort_inference.sort_to_string var_constraint.v_sort
   ^ "  " ^ string_of_int var_constraint.v_domain_min
   ^ ".." ^ string_of_int var_constraint.v_domain_max)
  ^ "\n"
  ^ (indent ^  "  blocked: " ^ String.concat " "
       (List.map (fun blocked -> string_of_int blocked) (blocked_to_list var_constraint.v_blocked)))
  ^ "\n"
  ^ (indent ^  "  links: " ^ String.concat " "
       (List.map (fun link -> Var.to_string link.v_var) var_constraint.v_links))

let sort_constraint_to_string indent sort_constraint =
  (indent ^ "sort_constraint: "
   ^ Sort_inference.sort_to_string sort_constraint.s_sort) ^ "\n"
  ^ String.concat "\n" (List.map (var_constraint_to_string (indent ^ "  ")) sort_constraint.s_vars)

let constraints_to_string constraints =
  "constraints: " ^ "UNSAT: " ^ string_of_bool constraints.unsatisfiable
  ^ "\n  clause: " ^ Term.clause_to_string constraints.clause
  ^ "\n  uncons: " ^ Term.clause_to_string constraints.unconstrained
  ^ "\n  constr: " ^ Term.clause_to_string constraints.constrained
  ^ "\n  base  : " ^ Subst.subst_to_string constraints.base_subst
  ^ String.concat "\n" (List.map (sort_constraint_to_string "  ") constraints.sort_constraints)
  ^ "\n"

let var_search_to_string indent var_search =
  (indent ^ "var_search: "
   ^ Var.to_string var_search.vs_var.v_var
   ^ " : " ^ Sort_inference.sort_to_string var_search.vs_var.v_sort
(*   ^ " B:" ^ string_of_bool var_search.vs_var.vs_has_blocked*)
   (*^ "  D:" ^ String.concat "," (List.map string_of_int (Array.to_list var_search.vs_var.vs_domain)) *)
     )
  ^ "\n"
  ^ (indent ^  "  links: " ^ String.concat " "
       (List.map (fun link -> Var.to_string link.vs_var.v_var) var_search.vs_links))
  ^ "\n"
  ^ (indent ^  "  first: " ^ String.concat " " (List.map string_of_int (Array.to_list var_search.vs_var.vs_first_element)))
  ^ "\n" ^ indent ^ "  current: " ^ string_of_int var_search.vs_var.vs_current_index


let sort_search_to_string indent sort_search =
  String.concat "\n" (List.map (var_search_to_string (indent ^ "  ")) (Array.to_list sort_search.ss_vars))

let search_to_string search =
  "search: "
  ^ "UNSAT:" ^ string_of_bool search.h_unsat
  ^ "\n  clause: " ^ Term.clause_to_string search.h_constraints.clause
  ^ "\n  uncons: " ^ Term.clause_to_string search.h_constraints.unconstrained
  ^ "\n  constr: " ^ Term.clause_to_string search.h_constraints.constrained
  ^ "\n  " ^ Subst.subst_to_string search.h_subst
  ^ "\n" ^ String.concat "" (List.map (sort_search_to_string "  ") search.h_sort_search)
  ^ "\n"

(*** tests ***)


(* a constraint variable in the constraints? *)
let is_constraint_var constraints var =
  constraints.var_to_var_constraint.(Var.id_of_var var) != None






(*** constraint creation ***)

(* domain_min and links are to be set later *)
let create_var_constraint number_of_vars var sort domain_size =
  {
  v_var = var;
  v_sort = sort;
  v_domain_min = 1;
  v_domain_max = domain_size;
  v_blocked = Array.make (domain_size + 1) false;
  v_links = [];
  vs_blocked = Array.make (domain_size + 1) true;
(*  vs_domain = Array.make domain_size 0;*)
  vs_domain_size = 0;
  vs_first_element = Array.make number_of_vars 0;
  vs_current_index = -1;
}

let shrink_domain var_constraint =
  while (var_constraint.v_blocked.(var_constraint.v_domain_min)) do
    if var_constraint.v_domain_min == var_constraint.v_domain_max then
      raise UNSATISFIABLE;
    var_constraint.v_domain_min <- var_constraint.v_domain_min + 1;
  done;
  while (var_constraint.v_blocked.(var_constraint.v_domain_max)) do
    if var_constraint.v_domain_min == var_constraint.v_domain_max then
      raise UNSATISFIABLE;
    var_constraint.v_domain_max <- var_constraint.v_domain_max - 1;
  done

let create_sort_constraint sort var_constraints : sort_constraint * subst =
  (* clean up domain bounds by shrinking bounds if possible wrt blocked domain elements *)
  let rec shrink var_constraints =
    match var_constraints with
      | [] -> ()
      | var_constraint :: tail ->
	  (* simplify domain *)
	  shrink_domain var_constraint;

	  (* if unit domain, then propagate to linked variables *)
	  if var_constraint.v_domain_min == var_constraint.v_domain_max then begin
	    let element =
	      var_constraint.v_domain_min
	    in
	    (* the linked vars have to be considered again *)
	    let var_constraints' =
	      List.fold_left
		(fun acc link ->
		   if not link.v_blocked.(element) then begin
		     link.v_blocked.(element) <- true;
		     link :: acc
		   end

		   else
		     acc
		)
		tail
		var_constraint.v_links
	    in
	      shrink var_constraints'
	  end
	  else
	    shrink tail
  in
    shrink var_constraints;

    let rec remove_unit_domains var_constraints subst check =
      match check with
	| [] ->
	    var_constraints, subst
	| head :: tail ->
	    if head.v_domain_min == head.v_domain_max then begin
	      let binding =
		Subst.make_binding
		  (Subst.make_var
		     head.v_var
		     Subst.input_literal_offset)
		  (Subst.make_term
		     (Finite_domain.get_domain_element head.v_domain_min)
		     Subst.input_literal_offset)
	      in
		remove_unit_domains var_constraints (Subst.append binding subst) tail
	    end
	    else begin
	      (* set min domain size *)
	      for i = 0 to Array.length head.vs_first_element - 1 do
		head.vs_first_element.(i) <- head.v_domain_min;
	      done;

	      remove_unit_domains (head :: var_constraints) subst tail
	    end
    in
    let var_constraints, base_subst =
      remove_unit_domains [] Subst.empty var_constraints
    in
    let sort_constraint = {
      s_sort = sort;
      s_vars = var_constraints;
    }
    in
      sort_constraint, base_subst


(* create the constraints *)
let create (finite_domain: finite_domain) (clause: clause) : constraints =
  let sorts =
    Finite_domain.get_sorts finite_domain
  in
  let domain_size =
    Finite_domain.get_domain_size finite_domain
  in

  (* mapping from clause vars to constraint vars *)
  let vars =
    VarTable.create 64
  in
  let number_of_vars =
    List.length (Term.vars_of_clause clause)
  in
  let var_to_var_constraint =
    Array.make number_of_vars None
  in
  (* build mapping from var to var constraint *)
  let get_var var sort =
    try
      VarTable.find vars var
    with
      | Not_found ->
	  let var_constraint =
	    create_var_constraint number_of_vars var sort domain_size
	  in
	    var_to_var_constraint.(Var.id_of_var var) <- Some var_constraint;
	    VarTable.add vars var var_constraint;
	    var_constraint
  in

  (* register constraints *)
  let register_permutable sort var =
    let domain_min =
      Sort_inference.get_min_permutable sorts sort
    in
    let var_constraint =
      get_var var sort
    in
      var_constraint.v_domain_min <- domain_min;
      (* block non-permutable elements *)
      for i = 0 to domain_min - 1 do
	var_constraint.v_blocked.(i) <- true;
      done;
  in
  let register_disjoint sort var0 var1 =
    let var_constraint0 = get_var var0 sort
    and var_constraint1 = get_var var1 sort
    in
      var_constraint0.v_links <- var_constraint1 :: var_constraint0.v_links;
      var_constraint1.v_links <- var_constraint0 :: var_constraint1.v_links;
  in

  (* register domain elements blocked by constraint literals of the form x = 1. *)
  let block sort var element_id =
    let var_constraint =
      get_var var sort
    in
      var_constraint.v_blocked.(element_id) <- true;
  in

  (* remove and register all constraint literals *)
  let clause', constrained =
    List.partition
      (fun literal ->
	 match literal.Term.atom with
	   | Term.Const _
	   | Term.Var _ ->
	       true
		 
	   | Term.Func func ->
	       let symbol = func.Term.symbol in
	       if
		 Symbol.equal symbol Symbol.equality
		 ||
		 Symbol.equal symbol Symbol.diff
	       then begin
		 match func.Term.subterms.(0), func.Term.subterms.(1) with
		   | Term.Var var0, Term.Var var1 ->
		       (* x = y *)
		       let sort =
			 match Sort_inference.get_var_sort sorts clause var0 with
			   | Some sort -> sort
			   | None -> begin
			       match Sort_inference.get_var_sort sorts clause var1 with
				 | Some sort -> sort
				 | None ->
				     Sort_inference.var_sort
			     end
		       in
			 register_disjoint sort var0 var1;
			 false

		   | Term.Var var, (Term.Const _ as element)
		   | (Term.Const _ as element), Term.Var var ->
		       (* x = 1 *)
		       let sort =
			 match Sort_inference.get_var_sort sorts clause var with
			   | Some sort -> sort
			   | None -> Sort_inference.var_sort
		       in
		       let id =
			 Finite_domain.get_id_of_domain_element element
		       in
			 block sort var id;
			 false

		   | _ ->
		       print_endline (Term.clause_to_string clause);
		       print_endline (Term.literal_to_string literal);
		       failwith "Finite_domain_constraints.create: unexpected constraint";
	       end

	       else if Symbol.is_fd_permutable symbol then begin
		 if Const.debug && literal.Term.sign then begin
		   print_endline (Term.clause_to_string clause);
		   failwith "Finite_domain_constraints.create: pos. permutation";
		 end;

		 let var =
		   match func.Term.subterms.(0) with
		     | Term.Var var -> var
		     | _ -> failwith ("Finite_domain_constraints.create: permutation constraint not a var: " ^
					Term.clause_to_string clause)
		 in
		 let sort =
		   Sort_inference.get_argument_sort sorts symbol 0
		 in
		   register_permutable sort var;
		   false
	       end

	       else
		 true
      )
      clause
  in

  (* group constraint variables by sort *)
  let sort_constraints =
    SortTable.create 64
  in
    VarTable.iter
      (fun _ var_constraint ->
	 let var_constraints =
	   try
	     SortTable.find sort_constraints var_constraint.v_sort
	   with
	     | Not_found ->
		 []
	 in	   
	   SortTable.replace sort_constraints
	     var_constraint.v_sort
	     (var_constraint :: var_constraints)
      )
      vars;

    (* create sort_constraints *)
    let sort_constraints, unsatisfiable, base_subst =
      try
    let sort_constraints, base_subst =
      SortTable.fold
	(fun sort var_constraints (sort_constraints, base_subst) ->
	   let sort_constraint, base_subst' =
	     create_sort_constraint sort var_constraints
	   in
	     (sort_constraint :: sort_constraints, base_subst' @ base_subst)
	)
	sort_constraints
	([], Subst.empty)
    in
    let sort_constraints =
      List.sort
	(fun x y ->
	   Tools.compare_lists x.s_vars y.s_vars
	)
	sort_constraints
    in
      sort_constraints, false, base_subst
      with
	| UNSATISFIABLE ->
	    [], true, Subst.empty
    in
(*
      print_endline ("CREATE: ");
      print_endline (Term.clause_to_string clause);
      print_endline (Term.clause_to_string clause');
      print_endline (Term.clause_to_string constrained);*)
      let constraints = {
	finite_domain = finite_domain;
	clause = clause;
	unconstrained = clause';
	constrained = constrained;
	sort_constraints = sort_constraints;
	var_to_var_constraint = var_to_var_constraint;
	unsatisfiable = unsatisfiable;
	base_subst = base_subst;
      }
      in
	if print_debug then print_endline ("CREATE: ");
	if print_debug then print_endline (constraints_to_string constraints);
	constraints




(*** search creation ***)

(* create the domain after removing elements
   which can not lead to a solution.

   raises NO_SOLUTION if the domain is empty
*)

let compute_domain_size var =
  var.vs_domain_size <- var.v_domain_max - var.v_domain_min + 1;

  if var.vs_blocked.(0) then begin
    for domain_element = var.v_domain_min to var.v_domain_max do
      if var.vs_blocked.(domain_element) then
	var.vs_domain_size <- var.vs_domain_size - 1;
    done;
  end

let create_var_search var : var_search =
  (*
  let domain_index = ref 0 in
    if var.vs_blocked.(0) then begin
    for domain_element = var.v_domain_min to var.v_domain_max do
      if not var.vs_blocked.(domain_element) then begin
	(*var.vs_domain.(!domain_index) <- domain_element;*)
	domain_index := !domain_index + 1;
      end
    done;
    (* empty domain, so no solutions *)
    if !domain_index == 0 then
      raise NO_SOLUTION;

    end;
    var.vs_domain_size <- !domain_index;
					*)
    {
      vs_var = var;
      vs_links = [];
    }



module VarOffsetTable =
  Hashtbl.Make (
    struct
      type t = (var * int)

      let equal (x, xo) (y, yo) = Var.equal x y && xo == yo
      let hash (x, xo) = Var.id_of_var x * 131 + xo
    end
  )


(* - var_mappings: instantiation of constraint vars in context unifier *)
let create_sort_search constraints sort_constraint var_mappings =
  (* reset the domain for the current search,
     if it has been modified in the last search.
     (important for performance,
     unsafe array setting would even up to 10% improvement) *)
  List.iter
    (fun var_constraint ->
       if var_constraint.vs_blocked.(0) then begin
	 var_constraint.vs_blocked.(0) <- false;
	 for i = 1 to Array.length var_constraint.v_blocked - 1 do
	   if not var_constraint.v_blocked.(i) then
	     var_constraint.vs_blocked.(i) <- false
	       (*Array.unsafe_set var_constraint.vs_blocked i false*)
	 done;
       end
    )
    sort_constraint.s_vars;


  (* merge equivalence classes of unified variables,
     i.e. when the subst maps x to y. *)
  let equ_classes =
    VarOffsetTable.create 32
  in

  let rec get_repr x o =
    try
      let (x, xo) =
	VarOffsetTable.find equ_classes (x, o)
      in
	get_repr x xo
    with
      | Not_found ->
	  x, o
  in
  let set_repr x xo y yo =
    let (x, xo), (y, yo) =
      get_repr x xo, get_repr y yo
    in
      if Const.debug
	&& (xo != Subst.input_literal_offset || not (is_constraint_var constraints x))
	&& (yo != Subst.input_literal_offset || not (is_constraint_var constraints y)) then
	failwith "Finite_domain_constraints.create_sort_search: both non-constraint vars";

      if Var.equal x y && xo == yo then
	()

      (* non-constraint var can not be class representative *)
      else if xo != Subst.input_literal_offset || not (is_constraint_var constraints x) then
	VarOffsetTable.add equ_classes (x, xo) (y, yo)

      (* non-constraint var can not be class representative *)
      else if yo != Subst.input_literal_offset || not (is_constraint_var constraints y) then
	VarOffsetTable.add equ_classes (y, yo) (x, xo)

      (* arbitrary: variable with smallest id becomes class representative *)
      else begin
	let cmp =
	  Var.compare x y
	in
	  if cmp < 0 then
	    VarOffsetTable.add equ_classes (y, yo) (x, xo)
	  else if cmp > 0 then
	    VarOffsetTable.add equ_classes (x, xo) (y, yo)
	  else
	    failwith "Finite_domain_constraints.create_sort_search: x = y"
      end
  in


    (* compute domain restrictions and class representatives *)
    List.iter
      (fun var_constraint ->
	 let var = var_constraint.v_var in
	 let index = Var.id_of_var var_constraint.v_var in
	   match var_mappings.(index) with
	     | None ->
		 (* no new constraints on this var *)
		 ()

	     | Some term ->
		 begin
		   match term.Subst.st_term with
		     | Term.Func _ ->
			 failwith "Finite_domain_constraints.create_sort_search"

		     | Term.Const _element ->
			 (* bound to a domain element,
			    so remove from domain of all linked vars *)
			 let id =
			   Finite_domain.get_id_of_domain_element term.Subst.st_term
			 in
			   (* check that element in domain *)
			   if var_constraint.vs_blocked.(id) then begin
			     raise NO_SOLUTION;
			   end;

			   List.iter
			     (fun var_constraint' ->
				if not var_constraint'.vs_blocked.(id) then begin
				  var_constraint'.vs_blocked.(0) <- true;
				  var_constraint'.vs_blocked.(id) <- true
				end
			     )
			     var_constraint.v_links
			     
		     | Term.Var var' ->
			 set_repr var Subst.input_literal_offset var' term.Subst.st_offset
		 end
      )
      sort_constraint.s_vars;

  (* create links to other var_search's based on the original links to var_constraint's *)
  let links =
    Array.make (Array.length var_mappings) []
  in
  (* keep links unique and ordered increasingly *)
  let rec add_link links link =
    match links with
      | [] -> link :: links
      | l :: t ->
	  let cmp =
	    Var.compare l.v_var link.v_var
	  in
	    if cmp == 0 then
	      links
	    else if cmp > 0 then
	      link :: links
	    else
	      l :: add_link t link
  in
  (* merges links of two vars of the same equivalence class,
     where repr must be the class representative .
     raise NO_SOLUTION, if there is a disequality constraint between them *)
  let add_links repr var =
    let new_links =
      match constraints.var_to_var_constraint.(Var.id_of_var var) with
	| Some link -> link.v_links
	| _ -> failwith "Finite_domain_constraints.create_sort_search 3"
    in
    let repr_id =
      Var.id_of_var repr
    in
    let local_links =
      List.fold_left
	(fun links link ->
	   (* replace link by link to representative *)
	   let repr', _ =
	     get_repr link.v_var Subst.input_literal_offset
	   in
	     (* var set equal to a var,
		but a constraint says they have to be different. *)
	     if Var.equal repr repr' then
	       raise NO_SOLUTION;
	     
	     let link' =
	       match constraints.var_to_var_constraint.(Var.id_of_var repr') with
		 | Some link -> link
		 | None -> failwith "Finite_domain_constraints.create_sort_search 2"
	     in
	       add_link links link'
	)
	links.(repr_id)
	new_links
    in
      links.(repr_id) <- local_links;
  in

  (* create mapping from var_constraint to var_search,
     done for class representatives only *)
  let var_search_length =
    Array.length var_mappings
  in
  let var_search_ =
    Array.make var_search_length None
  in
  let set_var_search var_constraint =
    let id =
      Var.id_of_var var_constraint.v_var
    in
      var_search_.(id) <- Some (create_var_search var_constraint);
      add_links var_constraint.v_var var_constraint.v_var;
  in
    (* merge equivalence classes and add var_constraint class representatives *)
    List.iter
      (fun var_constraint ->
	 let var = var_constraint.v_var in
	 let index = Var.id_of_var var_constraint.v_var in
	   match var_mappings.(index) with
	     | None -> ()
		 
	     | Some term ->
		 begin
		   match term.Subst.st_term with
		     | Term.Func _ ->
			 failwith "Finite_domain_constraints.create_sort_search"
			   
		     | Term.Const _ ->
			 (* variable already fixed, no search here *)
			 ()
			   
		     | Term.Var _ ->
			 (* unified with another variable. *)
			 let repr, _ =
			   get_repr var Subst.input_literal_offset
			 in
			   (* this variable is the class representative,
			      so just keep it *)
			   if Var.equal var repr then begin
			     set_var_search var_constraint;
			   end
			     
			   (* intersect domain and union links with class representative *)
			   else begin
			     let repr_constraint =
			       match constraints.var_to_var_constraint.(Var.id_of_var repr) with
				 | Some x -> x
				 | None -> failwith "Finite_domain_constraints.create_sort_search2"
			     in
			     for domain_element = 1 to Array.length var_constraint.vs_blocked - 1 do
			       if
				 var_constraint.vs_blocked.(domain_element)
				 &&
				 not repr_constraint.vs_blocked.(domain_element)
			       then begin
				 repr_constraint.vs_blocked.(0) <- true;
				 repr_constraint.vs_blocked.(domain_element) <- true
			       end
			     done;
			     add_links repr var;
			   end;
		 end
      )
      sort_constraint.s_vars;

    (* add the var_constraints which are unconstrained by the context unifier *)
    Array.iteri
      (fun i var_mapping ->
	 match var_mapping with
	   | None ->
	       (* no constraint *)
	       begin
		 match var_search_.(i) with
		   | None ->
		       (* not covered by the context unifier contraints (i.e. equality *)
		       begin
			 (* a constraint variable *)
			 match constraints.var_to_var_constraint.(i) with
			   | Some var_constraint when
			       (* same sort *)
			       Sort_inference.sort_equal var_constraint.v_sort sort_constraint.s_sort ->

			       set_var_search var_constraint
			   | _ ->
			       ()
		       end
		   | Some _ ->
		       ()
	       end
	   | Some _ ->
	       ()
      )
      var_mappings;

  (* gather the var_searchs *)
  let var_searchs =
    Array.fold_left
      (fun acc var_search ->
	 match var_search with
	   | None -> acc
	   | Some var_search -> var_search :: acc
      )
      []
      var_search_
  in
    (* add links from var_searchs to var_searchs *)
    List.iter
      (fun var_search ->
	 (* compute new domain size *)
	 compute_domain_size var_search.vs_var;

	 let links =
	   links.(Var.id_of_var var_search.vs_var.v_var)
	 in
	 let search_links =
	   List.fold_left
	     (fun acc link ->
		match var_search_.(Var.id_of_var link.v_var) with
		  | None ->
		      if Const.debug && var_mappings.(Var.id_of_var link.v_var) == None then begin
			print_endline (Var.to_string var_search.vs_var.v_var);
			print_endline (Var.to_string link.v_var);
			failwith "Finite_domain_constraints var search not set"
		      end;
		      acc

		  | Some var_search ->
		      var_search :: acc
	     )
	     []
	     links
	 in
	   var_search.vs_links <- search_links
      )
      var_searchs;

    (* order according to var_constraint order *)
    let sorted =
      Array.of_list var_searchs
    in
      Array.sort
	(fun x y ->
	   let cmp =
	     Tools.compare_int x.vs_var.vs_domain_size y.vs_var.vs_domain_size
	   in
	     if cmp != 0 then
	       cmp
	     else
	       (* prefer more links *)
	       Tools.compare_lists y.vs_var.v_links x.vs_var.v_links
	)
	sorted;

      (* keep only links to variables later in ordering,
	 as with eager constraint propagation we need only this direction. *)
      let order =
	Array.make (Array.length constraints.var_to_var_constraint) false
      in
	Array.iter
	  (fun var_search ->
	     order.(Var.id_of_var var_search.vs_var.v_var) <- true;
	     (* only for vars smaller in order value has been set *)
	     let links =
	       List.find_all
		 (fun link ->
		    not order.(Var.id_of_var link.vs_var.v_var)
		 )
		 var_search.vs_links
	     in
	       var_search.vs_links <- links
	  )
	  sorted;

      {
	ss_vars = sorted;
      }


let setup constraints subst =
  if constraints.unsatisfiable then begin
    {
      h_constraints = constraints;
      h_subst = subst;
      h_mappings = [| |];
      h_sort_search = [];
      h_unsat = true;
    }
  end

  else begin
  if print_debug then print_endline ("Setup:");
  if print_debug then print_endline (Term.clause_to_string constraints.clause);
  if print_debug then print_endline (Term.clause_to_string constraints.unconstrained);
  if print_debug then print_endline (Term.clause_to_string constraints.constrained);
  if print_debug then print_endline (Subst.subst_to_string subst);
  (* set up additional constraints from var to bound domain element / var *)
  let var_mappings =
    Array.make (Array.length constraints.var_to_var_constraint) None
  in
    Subst.iter
      (fun binding ->
	 (* if the bound variable is a constraint variable, register the new constraint *)
	 if
	   binding.Subst.sb_var.Subst.sv_offset == Subst.input_literal_offset
	   &&
	   is_constraint_var constraints binding.Subst.sb_var.Subst.sv_var
	 then begin
	   if print_debug then print_endline ("New constraint(var): " ^ Subst.binding_to_string binding);
	   var_mappings.(Var.id_of_var binding.Subst.sb_var.Subst.sv_var) <-
	     Some binding.Subst.sb_term
	 end;

	 (* if the term is a constraint variable, register its new constraint *)
	 if binding.Subst.sb_term.Subst.st_offset == Subst.input_literal_offset then begin
	   match binding.Subst.sb_term.Subst.st_term with
	     | Term.Var var' when
		 is_constraint_var constraints var' ->
		 
		 if print_debug then print_endline ("New constraint(term): " ^ Subst.binding_to_string binding);
		   var_mappings.(Var.id_of_var var') <-
		     Some (
		       Subst.make_term (Term.request_var var') binding.Subst.sb_var.Subst.sv_offset
		     )

	     | _ ->
		 ()
	 end;
      )
      subst;

    (* set up the now more constrained settings for each sort *)
    let sort_searchs, unsat =
      try
	let sort_searchs =
	  List.map
	    (fun sort_constraint ->
	       create_sort_search constraints sort_constraint var_mappings
	    )
	    constraints.sort_constraints
	in
	let sort_searchs =
	  List.sort
	    (fun x y ->
	       Tools.compare_int (Array.length x.ss_vars) (Array.length y.ss_vars)
	    )
	    sort_searchs
	in
	  sort_searchs, false
      with
	| NO_SOLUTION ->
	    [], true
    in
    let search =
      {
	h_constraints = constraints;
	h_subst = subst;
	h_mappings = var_mappings;
	h_sort_search = sort_searchs;
	h_unsat = unsat;
      }
    in
      if print_debug then print_endline (search_to_string search);
      search
  end





let get_sort_solutions ~(first_only:bool) search_sort =
  if print_debug then print_endline ("get_sort_solutions");
  let search_vars =
    search_sort.ss_vars
  in
  (* var_index: last chosen var_index in search_vars *)
  let rec next_index search_var domain_index var_index =
(*   print_endline ("next_index: "
		   ^ Var.to_string search_var.vs_var.v_var ^ " "
		   ^ string_of_int domain_index ^ " "
		   ^ string_of_int var_index);*)
    (* no further usable domain element *)
    if domain_index > search_var.vs_var.v_domain_max then begin
      raise Exit
    end

    else if search_var.vs_var.vs_blocked.(domain_index) then begin
      next_index search_var (domain_index + 1) var_index
    end

    (* is the current domain element already used
       by another variable, which is linked to this one? *)
    else if
      try
	for i = 0 to var_index do
	  (* set to the domain element currently under consideration *)
	  if
	    domain_index == search_vars.(i).vs_var.vs_current_index
	      (*
	    search_var.vs_var.vs_domain.(domain_index)
	    ==
	    search_vars.(i).vs_var.vs_domain.(search_vars.(i).vs_var.vs_current_index)*)
	  then begin
	    (* and linked to this var *)
	    List.iter
	      (fun link ->
		 if link == search_var then begin
		   raise Exit
		 end
	      )
	      search_vars.(i).vs_links
	  end;
	done;
	false
      with
	| Exit ->
	    true
    then begin
      next_index search_var (domain_index + 1) var_index
    end

    (* pick the domain element (it's index, that is) - no, it is the domain element *)
    else begin
      domain_index
    end
  in

  let rec try_element search_var var_index acc =
(*    print_endline ("try_element: "
		   ^ Var.to_string search_var.vs_var.v_var ^ " "
		   ^ string_of_int var_index);*)
    (* constraint propagation  *)
    let ok =
      List.for_all
	(fun search_var' ->
	   try
	     let next_index =
	       next_index
		 search_var'
		 search_var'.vs_var.vs_first_element.(var_index)
		 var_index
	     in
(*	       print_endline ("next_index: " ^ string_of_int next_index);*)
	       search_var'.vs_var.vs_first_element.(var_index + 1) <- next_index;
	       true
	   with
	     | Exit ->
		 false
	)
	search_var.vs_links;
    in
      (* domains of remaining vars exhausted *)
      if not ok then
	acc

      (* pick next var and continue *)
      else begin
	if print_debug then print_endline (var_search_to_string "" search_var);
	if print_debug then print_endline (string_of_int search_var.vs_var.vs_current_index);
	if print_debug then print_endline ("Set: " ^ Var.to_string search_var.vs_var.v_var ^ " -> " ^ string_of_int search_var.vs_var.vs_current_index);
(*	if print_debug then print_endline ("Set: " ^ Var.to_string search_var.vs_var.v_var ^ " -> " ^ string_of_int search_var.vs_var.vs_domain.(search_var.vs_var.vs_current_index));*)
	(* pick next var *)
	let acc' =
	  get_sort_solutions' (var_index + 1) acc
	in
	let next_index =
	  (* pick next domain element of this var *)
	  try
	    Some (next_index search_var (search_var.vs_var.vs_current_index + 1) (var_index - 1))
	  with
	    | Exit -> None
	in
	  match next_index with
	    | Some next_index ->
	      search_var.vs_var.vs_current_index <- next_index;
	      try_element search_var var_index acc'
	    | None ->
		(* domain exhausted *)
		acc'
      end

  and get_sort_solutions' var_index acc =
(*    print_endline ("get_sort_solutions': " ^ string_of_int var_index);*)
    if var_index >= Array.length search_vars then begin
      let solution =
	Array.fold_left
	  (fun subst search_var ->
	     let binding =
	       Subst.make_binding
		 (Subst.make_var
		    search_var.vs_var.v_var
		    Subst.input_literal_offset)
		 (Subst.make_term
		     (Finite_domain.get_domain_element search_var.vs_var.vs_current_index)
		       (*search_var.vs_var.vs_domain.(search_var.vs_var.vs_current_index))*)
		    Subst.input_literal_offset)
	     in
	       Subst.append binding subst
	  )
	  Subst.empty
	  search_vars
      in
(*	print_endline ("solution: " ^ Subst.subst_to_string solution);*)
	if first_only then raise (FIRST_SOLUTION solution);
	solution :: acc
    end
      
    else begin
      (* try all elements for this search var *)
      let search_var =
	search_vars.(var_index)
      in
	(* set the next domain element pointer to the last domain element pointer,
	   will be reset if constraint propagation from search_var to search_var'
	   happens later on *)
	for i = var_index + 1 to Array.length search_vars - 1 do
	  let search_var' =
	    search_vars.(i)
	  in
	    search_var'.vs_var.vs_first_element.(var_index + 1) <- search_var'.vs_var.vs_first_element.(var_index)
	done;


(*	search_var.vs_var.vs_current_index <- search_var.vs_var.vs_first_element.(var_index);*)

	if var_index != 0 then
	  search_var.vs_var.vs_current_index <- search_var.vs_var.vs_first_element.(var_index)
    else begin
      try
	  search_var.vs_var.vs_current_index <- next_index search_var search_var.vs_var.v_domain_min (-1);
      with
	| Exit ->
	    print_endline (string_of_int search_var.vs_var.v_domain_min);
	    print_endline (string_of_int search_var.vs_var.v_domain_max);
	    failwith "Empty domain for first element in fd search"
    end;
(*
(*	if var_index != 0 then*)
	  search_var.vs_var.vs_current_index <- search_var.vs_var.vs_first_element.(var_index);
(*	else
	  search_var.vs_var.vs_current_index <- search_var.vs_var.v_domain_min;*)
*)
	try_element search_var var_index acc;
    end
  in
    (* all constrained variables already instantiated by context unifier *)
    if Array.length search_vars == 0 then
      Subst.empty :: []

    (* search for solutions *)
    else
      get_sort_solutions' 0 []



let get_solutions ~(first_only:bool) search : (subst * solution option) list =
  if search.h_unsat then
    []
  else
  try
    (* get all solutions per sort *)
    let sort_solutions =
      List.map
	(fun sort_search ->
	   try
	     let solutions =
	       get_sort_solutions ~first_only:first_only sort_search
	     in
	       match solutions with
		 | [] ->
		     raise Exit
		 | _ ->
		     solutions
	   with
	     | FIRST_SOLUTION solution ->
		 solution :: []
	)
	search.h_sort_search
    in
(*      print_endline ("SOL: ");
      List.iter
	(fun sol ->
	   List.iter
	     (fun sol ->
		print_endline (Subst.subst_to_string sol)
	     )
	     sol
	)
	sort_solutions;*)
      
    (* merge solutions per sort *)
    let rec merge sort_solutions constraint_subst acc =
      match sort_solutions with
	| [] ->
	    let subst' =
	      try
		(* can't use match_substs as non-constraint variables
		   might be instantiated *)
		Unification.unify_substs ~recompute:true
		  search.h_subst constraint_subst
	      with
		| Unification.UNIFICATION_FAIL ->
		    print_endline (Subst.subst_to_string search.h_subst);
		    print_endline (Subst.subst_to_string constraint_subst);
		    failwith "Finite_domain_constraints.get_solutions: subst"
	    in
	      (subst', Some (subst_to_solution constraint_subst)) :: acc
	      
	| sort_solution :: tail ->
	    List.fold_left
	      (fun acc constraint_subst' ->
		 if Const.debug then begin
		   try
		     ignore (Unification.unify_substs ~recompute:false
		       constraint_subst constraint_subst': subst)
		   with
		     | Unification.UNIFICATION_FAIL ->
			 failwith "Finite_domain_constraints.get_solutions: constraints"
		 end;
		 let constraint_subst'' =
		   constraint_subst' @ constraint_subst
		 in
		   merge tail constraint_subst'' acc
	      )
	      acc
	      sort_solution
    in
    let solutions =
      merge sort_solutions [] []
    in
      solutions

  with
    | Exit ->
(*	print_endline "EXIT _ NO SOL";*)
	(* turned out that there were no solutions *)
	[]


(* :TODO: *)
let exists_solution search =
  if search.h_unsat then
    None
  else
  match get_solutions ~first_only:true search with
    | [] -> None
    | first :: [] -> Some first
    | _ -> failwith "Finite_domain_constraints.exists_solution: more than 1 solution"

let get_solutions search = get_solutions ~first_only:false search

(*
let is_p_preserving search =
  (* any parameter instantiated? *)
  not (
  Subst.exists
    (fun binding ->
       binding.Subst.sb_var.Subst.sv_offset == Subst.input_literal_offset
       &&
       Var.is_parametric binding.Subst.sb_var.Subst.sv_var
       &&
       is_constraint_var search.h_constraints binding.Subst.sb_var.Subst.sv_var
    )
    search.h_subst
  )
*)
let is_p_preserving constraints subst =
  if constraints.unsatisfiable then
    true

  (* there is no binding ... *)
  else
  not (
  Subst.exists
    (fun binding ->
       (* ... from a constraint variable ... *)
       binding.Subst.sb_var.Subst.sv_offset == Subst.input_literal_offset
       &&
       is_constraint_var constraints binding.Subst.sb_var.Subst.sv_var
       &&
       match binding.Subst.sb_term.Subst.st_term with
	 | Term.Var var' ->
	     (* ... to a parameter *)
	     Var.is_parametric var'
	 | Term.Const symbol ->
	     if Const.debug && Const.fd_right_split_parametric
	       && Symbol.is_skolem symbol then
		 failwith "Finite_domain_constraints.is_parametric: skolem constant";

	     (* ... or skolem constant *)
	     Symbol.is_skolem symbol

	 | _ -> false
    )
    subst
  )

(*
let is_closing search =
  (* any constraint trivially unsatisfiable? *)
  if search.h_unsat then
    None
  else

  try
    (* any parameter instantiated? *)
    Subst.iter
      (fun binding ->
	 if
	   binding.Subst.sb_var.Subst.sv_offset = Subst.input_literal_offset
	   &&
	   Var.is_parametric binding.Subst.sb_var.Subst.sv_var
	   &&
	   is_constraint_var search.h_constraints binding.Subst.sb_var.Subst.sv_var
	 then
	   raise Exit
      )
      search.h_subst;

    (* any solution does, as all will be p-preserving *)
    exists_solution search

  with
    | Exit ->
	(* instantiating the constraint variables instantiates a parameter,
	   so we won't have an empty remainder for sure,
	   so there can't be any closing context unifier *)
	None
*)





let get_clause constraints =
  constraints.clause

let get_unconstrained constraints =
  constraints.unconstrained

let get_constrained constraints =
  constraints.constrained

(* check if constraint var is bound to value outside its domain *)
let is_unsatisfiable_filter constraints subst =
  try
    Subst.iter
      (fun binding ->
	 if
	   binding.Subst.sb_var.Subst.sv_offset == Subst.input_literal_offset
	   &&
	   is_constraint_var constraints binding.Subst.sb_var.Subst.sv_var
	 then begin
	   match binding.Subst.sb_term.Subst.st_term with
	     | Term.Const _ ->
		 let var_constraint =
		   match constraints.var_to_var_constraint.(Var.id_of_var binding.Subst.sb_var.Subst.sv_var) with
		     | None -> failwith "Finite_domain_constraints.is_unsatisfiable_filter"
		     | Some var_constraint -> var_constraint
		 in
		 let element =
		   Finite_domain.get_id_of_domain_element binding.Subst.sb_term.Subst.st_term
		 in
		   if var_constraint.v_blocked.(element) then
		     raise UNSATISFIABLE;			  
	     | _ -> ()
	 end;
      )
      subst;
    Statistic.inc_global_debug2 ();
    false
  with
    | UNSATISFIABLE ->
	Statistic.inc_global_debug ();
	true

let is_unsatisfiable_filter constraints subst =
  Subst.exists
    (fun binding ->
       binding.Subst.sb_var.Subst.sv_offset == Subst.input_literal_offset
       &&
       is_constraint_var constraints binding.Subst.sb_var.Subst.sv_var
       &&
       match binding.Subst.sb_term.Subst.st_term with
	   (* check that bound term is in domain *)
	 | Term.Const _ as term ->
	     begin
	       match constraints.var_to_var_constraint.(Var.id_of_var binding.Subst.sb_var.Subst.sv_var) with
		 | Some var_constraint ->
		     let element =
		       Finite_domain.get_id_of_domain_element term
		     in
		       var_constraint.v_blocked.(element)
		       
		 | None ->
		     failwith "Finite_domain_constraints.is_unsatisfiable_filter"
	     end

	 (* check that vars don't have to be different *)
	 | Term.Var var ->
	     binding.Subst.sb_term.Subst.st_offset == Subst.input_literal_offset
	     &&
	     is_constraint_var constraints var
	     &&
	     begin
	       match constraints.var_to_var_constraint.(Var.id_of_var binding.Subst.sb_var.Subst.sv_var),
		 constraints.var_to_var_constraint.(Var.id_of_var var) with
		 | Some var0, Some var1 ->
		     List.memq var1 var0.v_links
		       
		 | _ ->
		     failwith "Finite_domain_constraints.is_unsatisfiable_filter 2"
	     end

	 | _ ->
	     false
      )
      subst
