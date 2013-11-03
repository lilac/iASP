(* $Id: test_subst.ml,v 1.4 2004/04/13 15:07:46 alexf Exp $ *)

type var = Var.var
type term = Term.term
type literal = Term.literal
type subst = Subst.subst




(*** data setup ***)
let var0 = Var.create_universal 0
let var1 = Var.create_universal 1
let var2 = Var.create_universal 2
let var3 = Var.create_universal 3
let par0 = Var.create_parametric 0
let par1 = Var.create_parametric 1
let par2 = Var.create_parametric 2

let v_0 = Term.request_var var0
let v_1 = Term.request_var var1
let v_2 = Term.request_var var2
let v_3 = Term.request_var var3
let p_0 = Term.request_var par0
let b = Read_darwin.to_term "b"
let g_x = Read_darwin.to_term "g(X)"
let p_a = Read_darwin.to_term "p(a)"
let p_b = Read_darwin.to_term "p(b)"
let p_x = Read_darwin.to_term "p(X)"
let q_xy = Read_darwin.to_term "q(X, Y)"
let q_xx = Read_darwin.to_term "q(X, X)"
let q_xb = Read_darwin.to_term "q(X, b)"
let q_bx = Read_darwin.to_term "q(b, X)"
let q_bb = Read_darwin.to_term "q(b, b)"
let q_b_g_b = Read_darwin.to_term "q(b, g(b))"
let q_zy =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var var2; Term.request_var var1 |]
  )
let q_yy =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var var1; Term.request_var var1 |]
  )
let q_by =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_const (Symbol.create_symbol "b" 0); Term.request_var var1 |]
  )
let q_23 =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 2); Term.request_var (Var.create_universal 3) |]
  )


let l_q_xy = Read_darwin.to_literal "q(X, Y)"
let l_not_q_xy = Read_darwin.to_literal "-q(X, Y)"
let l_q_xb = Read_darwin.to_literal "q(X, b)"
let l_not_q_xb = Read_darwin.to_literal "-q(X, b)"




(*** var_equal ***)
let var_equal ?(success_desired:bool = true) (var: Subst.var) (expected: Subst.var) =
  fun () ->
    let success =
      Subst.var_equal var expected
    in
      OUnit.assert_bool
	("Subst.var_equal: " ^ Subst.var_to_string var ^ " and " ^ Subst.var_to_string expected)
	success_desired
	success

let not_var_equal =
  var_equal ~success_desired:false


let test_var_equal1 =
  OUnit.TestCase (var_equal (Subst.make_var var1 0) (Subst.make_var var1 0))

let test_var_equal2 =
  OUnit.TestCase (not_var_equal (Subst.make_var var1 0) (Subst.make_var var1 1))

let test_var_equal3 =
  OUnit.TestCase (not_var_equal (Subst.make_var var1 0) (Subst.make_var var1 (-1)))

let test_var_equal4 =
  OUnit.TestCase (not_var_equal (Subst.make_var var1 0) (Subst.make_var var2 0))

let test_var_equal5 =
  OUnit.TestCase (var_equal (Subst.make_var par1 0) (Subst.make_var par1 0))

let test_var_equal6 =
  OUnit.TestCase (not_var_equal (Subst.make_var var1 0) (Subst.make_var par1 0))


let suite_var_equal =
  OUnit.TestLabel (
    ("var_equal"),
    (OUnit.TestList [
       test_var_equal1;
       test_var_equal2;
       test_var_equal3;
       test_var_equal4;
       test_var_equal5;
       test_var_equal6;
     ]
    )
  )














(*** subst_equal ***)
let subst_equal ?(success_desired:bool = true) (subst: subst) (expected: subst) =
  fun () ->
    let success =
      Subst.subst_equal subst expected
    in
      OUnit.assert_bool
	("subst_equal: " ^ Subst.subst_to_string subst ^ " and " ^ Subst.subst_to_string expected)
	success_desired
	success

let not_subst_equal =
  subst_equal ~success_desired:false


let subst1 =
  Subst.set_
    Subst.empty var1 0 p_a 0
let subst2 =
  Subst.set_
    Subst.empty var1 0 p_a 0
let test_subst_equal1 =
  OUnit.TestCase (subst_equal subst1 subst2)


let subst1 =
  Subst.set_
    Subst.empty var1 1 p_a 0
let subst2 =
  Subst.set_
    Subst.empty var1 0 p_a 0
let test_subst_equal2 =
  OUnit.TestCase (not_subst_equal subst1 subst2)


let subst1 =
  Subst.set_
    Subst.empty var1 0 p_a 0
let subst2 =
  Subst.set_
    Subst.empty var1 0 p_a 1
let test_subst_equal3 =
  OUnit.TestCase (not_subst_equal subst1 subst2)


let subst1 =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 p_a 0)
    var2 0 p_b 0
let subst2 =
  Subst.set_
    Subst.empty var1 0 p_a 0
let test_subst_equal4 =
  OUnit.TestCase (not_subst_equal subst1 subst2)


let subst1 =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 p_a 0)
    var2 0 p_b 0
let subst2 =
  Subst.set_
    (Subst.set_ Subst.empty var2 0 p_b 0)
    var1 0 p_a 0
let test_subst_equal5 =
  OUnit.TestCase (subst_equal subst1 subst2);;


let subst1 =
  Subst.set_
    (Subst.set_ Subst.empty var2 0 q_xy 0)
    var1 0 b 0
let subst2 =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 b 0)
    var2 0 q_xy 0
let test_subst_equal6 =
  OUnit.TestCase (subst_equal subst1 subst2)


let suite_subst_equal =
  OUnit.TestLabel (
    ("subst_equal"),
    (OUnit.TestList [
       test_subst_equal1;
       test_subst_equal2;
       test_subst_equal3;
       test_subst_equal4;
       test_subst_equal5;
       test_subst_equal6;
     ]
    )
  )






(*** set/get ***)
let get ?(success_desired:bool = true) (subst: subst) (var: var) (offset: int) (expected: Subst.term option) =
  fun () ->
    let bound =
      Subst.get_ subst var offset
    in
    let success =
      match bound, expected with
	| None, None ->
	    true
	| Some bound_term, Some expected_term ->
	    Subst.term_equal bound_term expected_term

	| _ ->
	    false
    in
    let bound_to_string bound =
      match bound with
	| None ->
	    "Unbound"

	| Some term ->
	    Subst.term_to_string term
    in
      OUnit.assert_bool
	("get: " ^ Subst.subst_to_string subst ^ " and\n"
	 ^ Subst.var_to_string (Subst.make_var var offset) ^ " is\n"
	 ^ bound_to_string bound ^ " but should be\n"
	 ^ bound_to_string expected)
	success_desired
	success

let not_subst_equal =
  subst_equal ~success_desired:false


let subst =
  Subst.set_
    Subst.empty var1 0 p_a 0
let test_get1 =
  OUnit.TestCase (get subst var1 0 (Some (Subst.make_term p_a 0)))


let subst =
  Subst.set_
    Subst.empty var1 0 p_a 0
let test_get2 =
  OUnit.TestCase (get subst var1 1 None)


let subst =
  Subst.set_
    Subst.empty var1 1 p_a 0
let test_get3 =
  OUnit.TestCase (get subst var1 0 None)


(* bind a variable already part of a bound term *)
let subst =
  Subst.set_
    (Subst.set_ Subst.empty var2 0 q_xy 0)
    var1 0 b 0
let test_get4 =
  OUnit.TestCase (get subst var2 0 (Some (Subst.make_term q_xy 0)))
let test_get5 =
  OUnit.TestCase (get subst var2 1 None)


(* bind a variable already bound to by another variable *)
let subst =
  Subst.set_
    (Subst.set_ Subst.empty var2 0 v_1 0)
    var1 0 b 0
let test_get6 =
  OUnit.TestCase (get subst var2 0 (Some (Subst.make_term b 0)))


(* bind a variable already bound to another term *)
let subst =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 b 0)
    var2 0 v_1 0
let test_get7 =
  OUnit.TestCase (get subst var2 0 (Some (Subst.make_term b 0)))



let suite_get =
  OUnit.TestLabel (
   ("get"),
    (OUnit.TestList [
       test_get1;
       test_get2;
       test_get3;
       test_get4;
       test_get5;
       test_get6;
       test_get7;
     ]
    )
  )






(*** apply_to_term ***)
let apply_to_term ?(success_desired:bool = true) (subst: subst) (term: term) (offset: int) (expected: term) =
  fun () ->
    let applied =
      Subst.apply_to_term subst term offset
    in
    let success =
      Term.term_equal applied expected
    in
      OUnit.assert_bool
	("apply_to_term: " ^ Subst.subst_to_string subst ^ " applied to\n"
	 ^ Subst.term_to_string (Subst.make_term term offset) ^ " is\n"
	 ^ Term.term_to_string applied ^ " but should be\n"
	 ^ Term.term_to_string expected)
	success_desired
	success

let not_apply_to_term =
  apply_to_term ~success_desired:false


let subst =
  Subst.set_
    Subst.empty var1 0 b 0
let test_apply_to_term1 =
  OUnit.TestCase (apply_to_term subst q_xy 0 q_xb)

let subst =
  Subst.set_
    Subst.empty var1 0 b 0
let test_apply_to_term2 =
  OUnit.TestCase (apply_to_term subst q_xy 1 q_xy)

(* bind a variable already part of a bound term *)
let subst =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 g_x 0)
    var0 0 b 0
let test_apply_to_term3 =
  OUnit.TestCase (apply_to_term subst q_xy 0 q_b_g_b)

(* bind a variable already part of a bound term *)
let subst =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 g_x 1)
    var0 1 b 0
let test_apply_to_term3 =
  OUnit.TestCase (apply_to_term subst q_xy 1 q_bx)

(* bind a variable already bound to another term *)
let subst =
  Subst.set_
    (Subst.set_ Subst.empty var2 0 q_xy 0)
    var1 0 b 0
let test_apply_to_term4 =
  OUnit.TestCase (apply_to_term subst q_xy 0 q_xb)

(* bind to an already bound variable *)
let subst =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 b 0)
    var0 0 v_1 0
let test_apply_to_term5 =
  OUnit.TestCase (apply_to_term subst q_xy 0 q_bb)


let suite_apply_to_term =
  OUnit.TestLabel (
    ("apply_to_term"),
    (OUnit.TestList [
       test_apply_to_term1;
       test_apply_to_term2;
       test_apply_to_term3;
       test_apply_to_term4;
       test_apply_to_term5;
     ]
    )
  )






(*** apply_to_literal ***)
let apply_to_literal ?(success_desired:bool = true) (subst: subst) (literal: literal) (offset: int) (expected: literal) =
  fun () ->
    let applied =
      Subst.apply_to_literal subst literal offset
    in
    let success =
      Term.literal_equal applied expected
    in
      OUnit.assert_bool
	("apply_to_literal: " ^ Subst.subst_to_string subst ^ " applied to\n"
	 ^ string_of_int offset ^ " : " ^ Term.literal_to_string literal ^ " is\n"
	 ^ Term.literal_to_string applied ^ " but should be\n"
	 ^ Term.literal_to_string expected)
	success_desired
	success

let not_apply_to_literal =
  apply_to_literal ~success_desired:false


let subst =
  Subst.set_
    Subst.empty var1 0 b 0
let test_apply_to_literal1 =
  OUnit.TestCase (apply_to_literal subst l_q_xy 0 l_q_xb)

let subst =
  Subst.set_
    Subst.empty var1 0 b 0
let test_apply_to_literal2 =
  OUnit.TestCase (apply_to_literal subst l_not_q_xy 0 l_not_q_xb)

let subst =
  Subst.set_
    Subst.empty var1 0 b 0
let test_apply_to_literal3 =
  OUnit.TestCase (not_apply_to_literal subst l_not_q_xy 0 l_q_xb)

let subst =
  Subst.set_
    Subst.empty var1 0 b 0
let test_apply_to_literal4 =
  OUnit.TestCase (not_apply_to_literal subst l_q_xy 0 l_not_q_xb)


let suite_apply_to_literal =
  OUnit.TestLabel (
    ("apply_to_literal"),
    (OUnit.TestList [
       test_apply_to_literal1;
       test_apply_to_literal2;
       test_apply_to_literal3;
       test_apply_to_literal4;
     ]
    )
  )

















(* Substitution Tree

(*** vars_of_subst ***)
let vars_of_subst ?(success_desired:bool = true) (subst: subst) (expected: Subst.var list) =
  fun () ->
    let vars =
      Subst.vars_of_subst subst
    in
    let success =
      Tools.lists_unordered_equal Subst.Subst.var_equal vars expected
    in
      OUnit.assert_bool
	("vars_of_subst: " ^ Subst.subst_to_string subst ^" is\n"
	 ^ String.concat ", " (List.map Subst.Subst.var_to_string vars)
	 ^ " but should be\n"
	 ^ String.concat ", " (List.map Subst.Subst.var_to_string expected))
	success_desired
	success

let not_vars_of_subst =
  vars_of_subst ~success_desired:false


let test_vars_of_subst1 =
  OUnit.TestCase (vars_of_subst Subst.empty [])

let test_vars_of_subst2 =
  OUnit.TestCase (not_vars_of_subst Subst.empty [(var0, 0)])

let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 b 0)
       var1 0 v_0 0)
    var2 0 q_xy 0
let test_vars_of_subst3 =
  OUnit.TestCase (vars_of_subst subst [(var0, 0); (var1, 0); (var2, 0)])

let subst =
  Subst.set_ Subst.empty var2 0 q_xy 0
let test_vars_of_subst4 =
  OUnit.TestCase (vars_of_subst subst [(var2, 0)])

let subst =
  Subst.set_ Subst.empty var2 1 q_xy 0
let test_vars_of_subst5 =
  OUnit.TestCase (vars_of_subst subst [(var2, 1)])

let subst =
  Subst.set_ Subst.empty var2 0 q_xy 0
let test_vars_of_subst6 =
  OUnit.TestCase (not_vars_of_subst subst [(var2, 1)])

let suite_vars_of_subst =
  OUnit.TestLabel (
    ("vars_of_subst"),
    (OUnit.TestList [
       test_vars_of_subst1;
       test_vars_of_subst2;
       test_vars_of_subst3;
       test_vars_of_subst4;
       test_vars_of_subst5;
       test_vars_of_subst6;
     ]
    )
  )






(*** unbound_vars_of_subst ***)
let unbound_vars_of_subst ?(success_desired:bool = true) (subst: subst) (expected: Subst.var list) =
  fun () ->
    let vars =
      Subst.unbound_vars_of_subst subst
    in
    let success =
      Tools.lists_unordered_equal Subst.Subst.var_equal vars expected
    in
      OUnit.assert_bool
	("unbound_vars_of_subst: " ^ Subst.subst_to_string subst ^" is\n"
	 ^ String.concat ", " (List.map Subst.Subst.var_to_string vars)
	 ^ " but should be\n"
	 ^ String.concat ", " (List.map Subst.Subst.var_to_string expected))
	success_desired
	success

let not_unbound_vars_of_subst =
  unbound_vars_of_subst ~success_desired:false


let test_unbound_vars_of_subst1 =
  OUnit.TestCase (unbound_vars_of_subst Subst.empty [])

let test_unbound_vars_of_subst2 =
  OUnit.TestCase (not_unbound_vars_of_subst Subst.empty [(var0, 0)])

let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 b 0)
       var1 0 p_0 0)
    var2 0 q_xy 0
let test_unbound_vars_of_subst3 =
  OUnit.TestCase (unbound_vars_of_subst subst [(par0, 0)])

let subst =
  Subst.set_ Subst.empty var2 0 q_xy 0
let test_unbound_vars_of_subst4 =
  OUnit.TestCase (unbound_vars_of_subst subst [(var0, 0); (var1, 0)])

let subst =
  Subst.set_ Subst.empty var2 1 q_xy 2
let test_unbound_vars_of_subst5 =
  OUnit.TestCase (unbound_vars_of_subst subst [(var0, 2); (var1, 2)])

let subst =
  Subst.set_
    (Subst.set_ Subst.empty var0 0 q_xy 1)
    var1 0 q_xy 1
let test_unbound_vars_of_subst6 =
  OUnit.TestCase (unbound_vars_of_subst subst [(var0, 1); (var1, 1)])

let suite_unbound_vars_of_subst =
  OUnit.TestLabel (
    ("unbound_vars_of_subst"),
    (OUnit.TestList [
       test_unbound_vars_of_subst1;
       test_unbound_vars_of_subst2;
       test_unbound_vars_of_subst3;
       test_unbound_vars_of_subst4;
       test_unbound_vars_of_subst5;
       test_unbound_vars_of_subst6;
     ]
    )
  )











(*** prune_renaming ***)
let prune_renaming ?(success_desired:bool = true) (subst: subst) (prune_vars: Subst.var list) (expected: subst) =
  fun () ->
    let pruned =
      Subst.prune_renaming subst prune_vars
    in
    let success =
      Subst.subst_equal pruned expected
    in
      OUnit.assert_bool
	("prune_renaming: " ^ Subst.subst_to_string subst ^ " pruned to\n"
	 ^ String.concat ", " (List.map Subst.Subst.var_to_string prune_vars)
	 ^ " is\n"
	 ^ Subst.subst_to_string pruned
	 ^ " but should be\n"
	 ^ Subst.subst_to_string expected)
	success_desired
	success

let not_prune_renaming =
  prune_renaming ~success_desired:false


let test_prune_renaming1 =
  OUnit.TestCase (prune_renaming Subst.empty [] Subst.empty)

let test_prune_renaming2 =
  OUnit.TestCase (prune_renaming Subst.empty [(var0, 0)] Subst.empty)

let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 b 0)
       var1 0 p_0 0)
    var2 0 q_xy 0
let test_prune_renaming3 =
  OUnit.TestCase (prune_renaming subst [(var0, 0)] subst)


let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 b 0)
       var1 0 v_0 0)
    var2 0 q_xy 0
let test_prune_renaming4 =
  OUnit.TestCase (prune_renaming subst [(var0, 0)] subst)

let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var2 0 q_xy 0)
       var0 0 v_3 0)
    var1 0 v_3 0
let expected =
  Subst.set_
    (Subst.set_ Subst.empty var2 0 q_xy 0)
    var0 0 v_1 0
let test_prune_renaming5 =
  OUnit.TestCase (prune_renaming subst [(var3, 0)] expected)


let suite_prune_renaming =
  OUnit.TestLabel (
    ("prune_renaming"),
    (OUnit.TestList [
       test_prune_renaming1;
       test_prune_renaming2;
       test_prune_renaming3;
       test_prune_renaming4;
       test_prune_renaming5;
     ]
    )
  )










(*** project ***)
let project ?(success_desired:bool = true) (subst: subst) (project_to_vars: Subst.var list) (expected: subst) =
  fun () ->
    let projected =
      Subst.project subst project_to_vars
    in
    let success =
      Subst.subst_equal projected expected
    in
      OUnit.assert_bool
	("project: " ^ Subst.subst_to_string subst ^ " projected to\n"
	 ^ String.concat ", " (List.map Subst.Subst.var_to_string project_to_vars)
	 ^ " is\n"
	 ^ Subst.subst_to_string projected
	 ^ " but should be\n"
	 ^ Subst.subst_to_string expected)
	success_desired
	success

let not_project =
  project ~success_desired:false


let test_project1 =
  OUnit.TestCase (project Subst.empty [] Subst.empty)

let test_project2 =
  OUnit.TestCase (project Subst.empty [(var0, 0)] Subst.empty)

let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 b 0)
       var1 0 p_0 0)
    var2 0 q_xy 0
let expected =
  Subst.set_ Subst.empty var0 0 b 0
let test_project3 =
  OUnit.TestCase (project subst [(var0, 0)] expected)


let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 b 0)
       var1 0 v_0 0)
    var2 0 q_xy 0
let expected =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 b 0)
    var2 0 q_by 0
let test_project4 =
  OUnit.TestCase (project subst [(var1, 0); (var2, 0)] expected)


let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 b 0)
       var1 0 v_0 0)
    var2 0 q_xy 0
let expected =
  Subst.set_ Subst.empty var2 0 q_bb 0
let test_project5 =
  OUnit.TestCase (project subst [(var2, 0)] expected)


let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var3 0 q_xy 0)
       var0 0 v_2 0)
    var1 0 v_2 0
let expected =
  Subst.set_
    (Subst.set_ Subst.empty var3 0 q_zy 0)
    var1 0 v_2 0
let test_project6 =
  OUnit.TestCase (project subst [(var1, 0); (var3, 0)] expected)


let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var2 0 q_xy 0)
       var0 0 v_3 0)
    var1 0 v_3 0
let test_project7 =
  OUnit.TestCase (project subst [(var3, 0)] [])


let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var3 0 q_xy 0)
       var0 0 v_1 0)
    var1 0 b 0
let expected =
  Subst.set_
    (Subst.set_ Subst.empty var3 0 q_by 0)
    var1 0 b 0
let test_project8 =
  OUnit.TestCase (project subst [(var1, 0); (var3, 0)] expected)


let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var3 0 q_xy 0)
       var0 0 v_1 0)
    var1 0 b 0
let expected =
  Subst.set_
    (Subst.set_ Subst.empty var3 0 q_xb 0)
    var0 0 b 0
let test_project9 =
  OUnit.TestCase (project subst [(var0, 0); (var3, 0)] expected)

let subst =
  Subst.set_
    (Subst.set_
       (Subst.set_ Subst.empty var0 0 q_23 0)
       var1 0 v_1 0)
    var2 0 v_1 0
let expected =
  Subst.set_
    (Subst.set_ Subst.empty var1 0 v_1 0)
    var2 0 v_1 0
let test_project10 =
  OUnit.TestCase (project subst [(var1, 0); (var2, 0)] expected)



let suite_project =
  OUnit.TestLabel (
    ("project"),
    (OUnit.TestList [
       test_project1;
       test_project2;
       test_project3;
       test_project4;
       test_project5;
       test_project6;
       test_project7;
       test_project8;
       test_project9;
       test_project10;
     ]
    )
  )

*)









(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_var_equal;
    suite_subst_equal;
    suite_get;
    suite_apply_to_term;
    suite_apply_to_literal;
(*    suite_vars_of_subst;
    suite_unbound_vars_of_subst;
    suite_prune_renaming;
    suite_project;*)
  ];;


ignore (OUnit.run_test_tt_main test_suite);;
