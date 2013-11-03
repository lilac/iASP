(* $Id: test_symbol.ml,v 1.3 2004/03/28 19:01:01 alexf Exp $ *)

type symbol = Symbol.symbol




(*** data setup ***)
let symbol1 = Symbol.create_symbol "a" 0;;
let symbol2 = Symbol.create_symbol "f" 1;;
let symbol3 = Symbol.create_symbol "a" 0;;

let skolem1 = Symbol.create_constant ();;
let skolem2 = Symbol.create_constant ();;



(*** equal ***)
let equal ?(success_desired=true) (symbol1: symbol) (symbol2: symbol) =
  fun () ->
    let success =
      Symbol.equal symbol1 symbol2
    in
      OUnit.assert_bool
	("equal: " ^ Symbol.to_string symbol1 ^ " and " ^ Symbol.to_string symbol2)
	success_desired
	success
    
let not_equal = 
  equal ~success_desired:false


let test_equal1 =
  OUnit.TestCase (equal symbol1 symbol1)

let test_equal2 =
  OUnit.TestCase (equal symbol2 symbol2)

let test_equal3 =
  OUnit.TestCase (not_equal symbol1 symbol2)

let test_equal4 =
  OUnit.TestCase (equal symbol1 symbol3)


let suite_equal =
  OUnit.TestLabel (
    ("equal"),
    (OUnit.TestList [
       test_equal1;
       test_equal2;
       test_equal3;
       test_equal4
     ]
    )
  )





(*** name_of_symbol ***)
let name_of_symbol ?(success_desired=true) (symbol: symbol) (expected: string) =
  fun () ->
    let name =
      Symbol.name_of_symbol symbol
    in
    let success = 
      name = expected
    in
      OUnit.assert_bool
	("name_of_symbol: " ^ Symbol.to_string symbol ^ ": " ^ name ^ " and " ^ expected)
	success_desired
	success

let not_name_of_symbol = 
  name_of_symbol ~success_desired:false


let test_name_of_symbol1 =
  OUnit.TestCase (name_of_symbol symbol1 "a")

let test_name_of_symbol2 =
  OUnit.TestCase (name_of_symbol symbol2 "f")

let test_name_of_symbol3 =
  OUnit.TestCase (name_of_symbol symbol3 "a")

let test_name_of_symbol4 =
  OUnit.TestCase (not_name_of_symbol symbol3 "f")



let suite_name_of_symbol =
  OUnit.TestLabel (
    ("name_of_symbol"),
    (OUnit.TestList [
       test_name_of_symbol1;
       test_name_of_symbol2;
       test_name_of_symbol3;
       test_name_of_symbol4
     ]
    )
  )




(*** arity_of_symbol ***)
let arity_of_symbol ?(success_desired=true) (symbol: symbol) (expected: int) =
  fun () ->
    let arity =
      Symbol.arity_of_symbol symbol
    in
    let success = 
      arity = expected
    in
      OUnit.assert_bool
	("arity_of_symbol: " ^ Symbol.to_string symbol ^ ": "
	 ^ string_of_int arity ^ " and " ^ string_of_int expected)
	success_desired
	success

let not_arity_of_symbol = 
  arity_of_symbol ~success_desired:false


let test_arity_of_symbol1 =
  OUnit.TestCase (arity_of_symbol symbol1 0)

let test_arity_of_symbol2 =
  OUnit.TestCase (arity_of_symbol symbol2 1)

let test_arity_of_symbol3 =
  OUnit.TestCase (arity_of_symbol symbol3 0)

let test_arity_of_symbol4 =
  OUnit.TestCase (not_arity_of_symbol symbol3 (-1))



let suite_arity_of_symbol =
  OUnit.TestLabel (
    ("arity_of_symbol"),
    (OUnit.TestList [
       test_arity_of_symbol1;
       test_arity_of_symbol2;
       test_arity_of_symbol3;
       test_arity_of_symbol4
     ]
    )
  )













(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_equal;
    suite_name_of_symbol;
    suite_arity_of_symbol
  ];;


ignore (OUnit.run_test_tt_main test_suite);;
