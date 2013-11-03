(* $Id: test_term_quality.ml,v 1.5 2004/04/26 17:54:45 alexf Exp $ *)

type literal = Term.literal
type subst = Subst.subst


(*** data setup ***)
let var0 = Var.create_universal 0
let var1 = Var.create_universal 1
let var2 = Var.create_universal 2
let par0 = Var.create_parametric 0
let par1 = Var.create_parametric 1
let par2 = Var.create_parametric 2

let v_0 = Term.request_var var0
let v_1 = Term.request_var var1
let v_2 = Term.request_var var2
let p_0 = Term.request_var par0

let g_a = Read_darwin.to_term "g(a)"





(*** is_parametric ***)
let is_parametric ?(success_desired=true) (parse: string) (expected: bool) =
  fun () ->
    let literal =
      Read_darwin.to_literal parse
    in
    let success =
      (Term_quality.is_parametric literal) = expected
    in
      OUnit.assert_bool
	("is_parametric: " ^ parse)
	success_desired
	success
	
let not_is_parametric = 
  is_parametric ~success_desired:false



let test_is_parametric1 =
  OUnit.TestCase (is_parametric "f(g(a), b)" false)

let test_is_parametric2 =
  OUnit.TestCase (is_parametric "f(X, b)" false)

let test_is_parametric3 =
  OUnit.TestCase (is_parametric "f(=X, b)" true)

let test_is_parametric4 =
  OUnit.TestCase (is_parametric "f(a, g(f(X, Y)))" false)

let test_is_parametric5 =
  OUnit.TestCase (is_parametric "g(a)" false)


let suite_is_parametric =
  OUnit.TestLabel (
    ("is_parametric"),
    (OUnit.TestList [
       test_is_parametric1;
       test_is_parametric2;
       test_is_parametric3;
       test_is_parametric4;
       test_is_parametric5;
     ]
    )
  )








(*** cmp_pureness ***)
let cmp_pureness ?(success_desired=true) (parse1: string) (parse2: string) (expected: int) =
  fun () ->
    let literal1 =
      Read_darwin.to_literal parse1
    in
    let literal2 =
      Read_darwin.to_literal parse2
    in
    let pureness1 =
      Term_quality.is_parametric literal1
    in
    let pureness2 =
      Term_quality.is_parametric literal2
    in
    let cmp =
      Term_quality.cmp_pureness pureness1 pureness2
    in
    let success =
      cmp = expected
    in
      OUnit.assert_bool
	("cmp_pureness: " ^ parse1 ^ " and " ^ parse2 ^ " is " ^ string_of_int cmp
	   ^ " but should be " ^ string_of_int expected)
	success_desired
	success
	
let not_cmp_pureness = 
  cmp_pureness ~success_desired:false



let test_cmp_pureness1 =
  OUnit.TestCase (cmp_pureness "f(g(a), b)" "f(g(a), b)" 0)

let test_cmp_pureness2 =
  OUnit.TestCase (cmp_pureness "f(g(a), b)" "f(g(a), a)" 0)

let test_cmp_pureness3 =
  OUnit.TestCase (cmp_pureness "f(g(X), b)" "f(g(a), b)" 0)

let test_cmp_pureness4 =
  OUnit.TestCase (cmp_pureness "f(g(X), b)" "f(g(a), Y)" 0)

let test_cmp_pureness5 =
  OUnit.TestCase (cmp_pureness "f(g(X), b)" "f(g(a), =Y)" (-1))

let test_cmp_pureness6 =
  OUnit.TestCase (cmp_pureness "f(g(=X), b)" "f(g(a), =Y)" 0)

let test_cmp_pureness7 =
  OUnit.TestCase (cmp_pureness "f(g(=X), b)" "f(g(a), Y)" 1)

let test_cmp_pureness8 =
  OUnit.TestCase (cmp_pureness "f(g(=X), b)" "f(g(a), b)" 1)

let test_cmp_pureness9 =
  OUnit.TestCase (cmp_pureness "f(g(a), b)" "f(g(a), =Y)" (-1))

let test_cmp_pureness10 =
  OUnit.TestCase (cmp_pureness "f(g(a), b)" "f(g(a), Y)" 0)


let suite_cmp_pureness =
  OUnit.TestLabel (
    ("cmp_pureness"),
    (OUnit.TestList [
       test_cmp_pureness1;
       test_cmp_pureness2;
       test_cmp_pureness3;
       test_cmp_pureness4;
       test_cmp_pureness5;
       test_cmp_pureness6;
       test_cmp_pureness7;
       test_cmp_pureness8;
       test_cmp_pureness9;
       test_cmp_pureness10;
     ]
    )
  )







(* depth_of_literal and depth_of_literal_subst *)
let depth_of_literal_subst ?(success_desired=true) (parse: string) (subst: subst) (offset: int) (expected: int) =
  fun () ->
    let literal =
      Read_darwin.to_literal parse
    in

    (* first depth of literal *)
    let applied_literal =
      Subst.apply_to_literal subst literal offset
    in

    let depth =
      Term_quality.depth_of_literal applied_literal
    in
    let success =
      depth = expected
    in
      OUnit.assert_bool
	("depth_of_literal:\n" ^ Term.literal_to_string applied_literal ^ " is\n"
	 ^ string_of_int depth ^ "\nbut should be\n"
	 ^ string_of_int expected)
	success_desired
	success;

    (* now depth_of_literal_subst *)
    let depth =
      Term_quality.depth_of_literal_subst subst literal offset
    in
    let success =
      depth = expected
    in
      OUnit.assert_bool
	("depth_of_literal:\n"
	 ^ Subst.subst_to_string subst ^ "\n" ^ string_of_int offset ^ ": " ^ Term.literal_to_string literal ^ " is\n"
	 ^ string_of_int depth ^ "\nbut should be\n"
	 ^ string_of_int expected)
	success_desired
	success
	
let not_depth_of_literal_subst = 
  depth_of_literal_subst ~success_desired:false


let test_depth_of_literal_subst1 =
  OUnit.TestCase (depth_of_literal_subst "b" Subst.empty 0 0)


let test_depth_of_literal_subst2 =
  OUnit.TestCase (depth_of_literal_subst "-g(X)" Subst.empty 0 1)

let subst =
  Subst.set_ Subst.empty var0 0 g_a 0
let test_depth_of_literal_subst3 =
  OUnit.TestCase (not_depth_of_literal_subst "g(X)" subst 0 1)

let subst =
  Subst.set_ Subst.empty var0 0 g_a 0
let test_depth_of_literal_subst4 =
  OUnit.TestCase (depth_of_literal_subst "-g(X)" subst 0 2)

let subst =
  Subst.set_ Subst.empty var0 1 g_a 0
let test_depth_of_literal_subst5 =
  OUnit.TestCase (depth_of_literal_subst "g(X)" subst 0 1)

let subst =
  Subst.set_ Subst.empty var1 0 g_a 0
let test_depth_of_literal_subst6 =
  OUnit.TestCase (depth_of_literal_subst "g(X)" subst 0 1)

let subst =
  Subst.set_ Subst.empty par0 0 g_a 0
let test_depth_of_literal_subst7 =
  OUnit.TestCase (depth_of_literal_subst "g(X)" subst 0 1)

let test_depth_of_literal_subst8 =
  OUnit.TestCase (depth_of_literal_subst "g(X)" Subst.empty 0 1)

let test_depth_of_literal_subst9 =
  OUnit.TestCase (depth_of_literal_subst "g(f(a, b))" Subst.empty 0 2)

let subst =
  Subst.set_ Subst.empty var0 0 g_a 0
let test_depth_of_literal_subst10 =
  OUnit.TestCase (depth_of_literal_subst "f(X, g(g(g(g(a)))))" subst 0 5)



let suite_depth_of_literal_subst =
  OUnit.TestLabel (
    ("depth_of_literal_subst"),
    (OUnit.TestList [
       test_depth_of_literal_subst1;
       test_depth_of_literal_subst2;
       test_depth_of_literal_subst3;
       test_depth_of_literal_subst4;
       test_depth_of_literal_subst5;
       test_depth_of_literal_subst6;
       test_depth_of_literal_subst7;
       test_depth_of_literal_subst8;
       test_depth_of_literal_subst9;
       test_depth_of_literal_subst10;
     ]
    )
  )





(*** weight_of_literal ***)
let weight_of_literal ?(success_desired=true) (parse: string) (expected: int) =
  fun () ->
    let literal =
      Read_darwin.to_literal parse
    in
    let weight =
      Term_quality.weight_of_literal literal
    in
    let success =
      weight = expected
    in
      OUnit.assert_bool
	("weight_of_literal: "
	 ^ parse
	 ^ " is " ^ string_of_int weight
	 ^ " but should be " ^ string_of_int expected)
	success_desired
	success
	
let not_weight_of_literal = 
  weight_of_literal ~success_desired:false



let test_weight_of_literal1 =
  OUnit.TestCase (weight_of_literal "f(g(a), b)" 4)

let test_weight_of_literal2 =
  OUnit.TestCase (weight_of_literal "f(X, b)" 2)

let test_weight_of_literal3 =
  OUnit.TestCase (weight_of_literal "f(=X, b)" 3)

let test_weight_of_literal4 =
  OUnit.TestCase (weight_of_literal "f(a, g(f(X, Y)))" 4)

let test_weight_of_literal5 =
  OUnit.TestCase (weight_of_literal "g(a)" 2)


let suite_weight_of_literal =
  OUnit.TestLabel (
    ("weight_of_literal"),
    (OUnit.TestList [
       test_weight_of_literal1;
       test_weight_of_literal2;
       test_weight_of_literal3;
       test_weight_of_literal4;
       test_weight_of_literal5;
     ]
    )
  )









(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_is_parametric;
    suite_cmp_pureness;
    suite_depth_of_literal_subst;
    suite_weight_of_literal;
  ];;


ignore (OUnit.run_test_tt_main test_suite)
