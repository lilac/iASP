(* $Id: test_term.ml,v 1.3 2004/03/28 19:01:01 alexf Exp $ *)

type symbol = Symbol.symbol
type var = Var.var
type term = Term.term




(*** data setup ***)

let c_b = Symbol.create_symbol "b" 0
let c_f = Symbol.create_symbol "f" 2
let c_g = Symbol.create_symbol "g" 1




(*** term_equal ***)
let term_equal ?(success_desired=true) (parse1: string) (parse2: string) =
  fun () ->
    let term1 =
      Read_darwin.to_term parse1
    in
    let term2 =
      Read_darwin.to_term parse2
    in
    let success =
      Term.term_equal term1 term2
    in
      OUnit.assert_bool
	("term_equal: " ^ Term.term_to_string term1 ^ " and " ^ Term.term_to_string term2)
	success_desired
	success
	
let not_term_equal = 
  term_equal ~success_desired:false



let test_term_equal1 =
  OUnit.TestCase (term_equal "f(g(a), b)" "f(g(a), b)")

let test_term_equal2 =
  OUnit.TestCase (term_equal "f(g(X), Y)" "f(g(X), Y)")

let test_term_equal3 =
  OUnit.TestCase (term_equal "f(g(=X), Y)" "f(g(=X), Y)")

let test_term_equal4 =
  OUnit.TestCase (not_term_equal "f(g(=X), Y)" "f(g(X), Y)")

let test_term_equal5 =
  OUnit.TestCase (not_term_equal "f(g(a), b)" "f(g(X), Y)")

let test_term_equal6 =
  OUnit.TestCase (not_term_equal "f(g(a), b)" "f(a, b)")

let test_term_equal7 =
  OUnit.TestCase (not_term_equal "f(g(X), Y)" "f(g(X), X)")


let suite_term_equal =
  OUnit.TestLabel (
    ("term_equal"),
    (OUnit.TestList [
       test_term_equal1;
       test_term_equal2;
       test_term_equal3;
       test_term_equal4;
       test_term_equal5;
       test_term_equal6;
       test_term_equal7;
     ]
    )
  )





(*** literal_equal ***)
let literal_equal ?(success_desired=true) (parse1: string) (parse2: string) =
  fun () ->
    let literal1 =
      Read_darwin.to_literal parse1
    in
    let literal2 =
      Read_darwin.to_literal parse2
    in
    let success =
      Term.literal_equal literal1 literal2
    in
      OUnit.assert_bool
	("literal_equal: " ^ Term.literal_to_string literal1 ^ " and " ^ Term.literal_to_string literal2)
	success_desired
	success
	
let not_literal_equal = 
  literal_equal ~success_desired:false



let test_literal_equal1 =
  OUnit.TestCase (literal_equal "f(g(a), b)" "f(g(a), b)")

let test_literal_equal2 =
  OUnit.TestCase (not_literal_equal "-f(g(a), b)" "f(g(a), b)")

let test_literal_equal3 =
  OUnit.TestCase (literal_equal "-f(g(X), Y)" "-f(g(X), Y)")

let test_literal_equal4 =
  OUnit.TestCase (not_literal_equal "f(g(=X), Y)" "-f(g(=X), Y)")


let suite_literal_equal =
  OUnit.TestLabel (
    ("literal_equal"),
    (OUnit.TestList [
       test_literal_equal1;
       test_literal_equal2;
       test_literal_equal3;
       test_literal_equal4;
     ]
    )
  )






(*** clause_equal ***)
let clause_equal ?(success_desired=true) (parse1: string) (parse2: string) =
  fun () ->
    let clause1 =
      Read_darwin.to_clause parse1
    in
    let clause2 =
      Read_darwin.to_clause parse2
    in
    let success =
      Term.clause_equal clause1 clause2
    in
      OUnit.assert_bool
	("clause_equal: " ^ Term.clause_to_string clause1 ^ " and " ^ Term.clause_to_string clause2)
	success_desired
	success
	
let not_clause_equal = 
  clause_equal ~success_desired:false



let test_clause_equal1 =
  OUnit.TestCase (clause_equal "{ f(g(a), b) }" "{ f(g(a), b) }")

let test_clause_equal2 =
  OUnit.TestCase (not_clause_equal "{ f(g(a), b), -f(a, b)  }" "{ f(g(a), b) }")

let test_clause_equal3 =
  OUnit.TestCase (clause_equal "{ f(g(a), b), -f(a, b)  }" "{ -f(a, b), f(g(a), b) }")

let test_clause_equal4 =
  OUnit.TestCase (not_clause_equal "{ f(g(a), b), f(a, b)  }" "{ -f(a, b), f(g(a), b) }")

let test_clause_equal5 =
  OUnit.TestCase (clause_equal "{ f(b, a), f(a, b), f(a, b) }" "{ f(b, a), f(a, b), f(a, b) }")


let suite_clause_equal =
  OUnit.TestLabel (
    ("clause_equal"),
    (OUnit.TestList [
       test_clause_equal1;
       test_clause_equal2;
       test_clause_equal3;
       test_clause_equal4;
       test_clause_equal5;
     ]
    )
  )




      

(*** vars_of_term ***)
let vars_of_term ?(success_desired:bool = true) (parse: string) (expected: var list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let vars_of_term =
      Term.vars_of_term term
    in
    let success =
      Tools.lists_unordered_equal Var.equal vars_of_term expected
    in
      OUnit.assert_bool
	("vars_of_term: " ^ Term.term_to_string term
	 ^ "\nshould be\n" ^ String.concat " , " (List.map Var.to_string expected)
	 ^ "\nbut is\n" ^ String.concat " , " (List.map Var.to_string vars_of_term))
	success_desired
	success
	
let not_vars_of_term =
  vars_of_term ~success_desired:false

    

let test_vars_of_term1 =
  OUnit.TestCase (vars_of_term "f(g(X), Y)" [Var.create_universal 0; Var.create_universal 1])

let test_vars_of_term2 =
  OUnit.TestCase (vars_of_term "f(g(X), X)" [Var.create_universal 0])

let test_vars_of_term3 =
  OUnit.TestCase (not_vars_of_term "f(g(Y), Y)" [Var.create_universal 1])

let test_vars_of_term4 =
  OUnit.TestCase (vars_of_term "f(X, Y)" [Var.create_universal 0; Var.create_universal 1])

let test_vars_of_term5 =
  OUnit.TestCase (not_vars_of_term "f(X, X)" [Var.create_universal 0; Var.create_universal 0])

let test_vars_of_term6 =
  OUnit.TestCase (vars_of_term "f(g(g(g(Y))), g(X))" [Var.create_universal 0; Var.create_universal 1])



let suite_vars_of_term =
  OUnit.TestLabel (
    ("vars_of_term"),
    (OUnit.TestList [
       test_vars_of_term1;
       test_vars_of_term2;
       test_vars_of_term3;
       test_vars_of_term4;
       test_vars_of_term5;
       test_vars_of_term6
     ]
    )
  )








(*** vars_of_literal ***)
let vars_of_literal ?(success_desired:bool = true) (parse: string) (expected: var list) =
  fun () ->
    let literal =
      Read_darwin.to_literal parse
    in
    let vars_of_literal =
      Term.vars_of_literal literal
    in
    let success =
      Tools.lists_unordered_equal Var.equal vars_of_literal expected
    in
      OUnit.assert_bool
	("vars_of_literal: " ^ Term.literal_to_string literal
	 ^ "\nshould be\n" ^ String.concat " , " (List.map Var.to_string expected)
	 ^ "\nbut is\n" ^ String.concat " , " (List.map Var.to_string vars_of_literal))
	success_desired
	success
	
let not_vars_of_literal =
  vars_of_literal ~success_desired:false

    

let test_vars_of_literal1 =
  OUnit.TestCase (vars_of_literal "-f(g(X), Y)" [Var.create_universal 0; Var.create_universal 1])

let test_vars_of_literal2 =
  OUnit.TestCase (vars_of_literal "f(g(X), X)" [Var.create_universal 0])

let test_vars_of_literal3 =
  OUnit.TestCase (not_vars_of_literal "-f(g(Y), Y)" [Var.create_universal 1])



let suite_vars_of_literal =
  OUnit.TestLabel (
    ("vars_of_literal"),
    (OUnit.TestList [
       test_vars_of_literal1;
       test_vars_of_literal2;
       test_vars_of_literal3;
     ]
    )
  )









(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_term_equal;
    suite_literal_equal;
    suite_clause_equal;
    suite_vars_of_term;
    suite_vars_of_literal;
  ];;


ignore (OUnit.run_test_tt_main test_suite);;
