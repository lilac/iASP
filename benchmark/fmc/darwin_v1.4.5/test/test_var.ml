(* $Id: test_var.ml,v 1.3 2004/03/28 19:01:01 alexf Exp $ *)

type var = Var.var



(*** data setup ***)
let var1 = Var.create_universal 1
let var2 = Var.create_universal 2
let par1 = Var.create_parametric 1
let par2 = Var.create_parametric 2





(*** is_var ***)
let is_universal ?(success_desired:bool = true) (var: var) =
  fun () ->
    let success =
      Var.is_universal var
    in
      OUnit.assert_bool
	("is_universal: " ^ Var.to_string var)
	success_desired
	success

let not_is_universal =
  is_universal ~success_desired:false


let test_is_universal1 =
  OUnit.TestCase (is_universal var1)

let test_is_universal2 =
  OUnit.TestCase (is_universal var2)

let test_is_universal3 =
  OUnit.TestCase (not_is_universal par1)

let test_is_universal4 =
  OUnit.TestCase (not_is_universal par2)

let suite_is_universal =
  OUnit.TestLabel (
    ("is_universal"),
    (OUnit.TestList [
       test_is_universal1;
       test_is_universal2;
       test_is_universal3;
       test_is_universal4
     ]
    )
  )




(*** equal ***)
let equal ?(success_desired=true) (var1: var) (var2: var) =
  fun () ->
    let success =
      (Var.equal var1 var2)
    in
      OUnit.assert_bool
	("var_equal: " ^ Var.to_string var1 ^ " and " ^ Var.to_string var2)
	success_desired
	success

let not_equal =
  equal ~success_desired:false



let test_equal1 =
  OUnit.TestCase (equal var1 var1)

let test_equal2 =
  OUnit.TestCase (equal par1 par1)

let test_equal3 =
  OUnit.TestCase (not_equal var1 var2)

let test_equal4 =
  OUnit.TestCase (not_equal var1 par1)

let test_equal5 =
  OUnit.TestCase (not_equal par1 par2)


let suite_equal =  
  OUnit.TestLabel (
    ("equal"),
    (OUnit.TestList [
       test_equal1;
       test_equal2;
       test_equal3;
       test_equal4;
       test_equal5
     ]
    )
  )






(*** id_of_var ***)
let id_of_var ?(success_desired=true) (var: var) (expected: int) =
  fun () ->
    let success =
      ((Var.id_of_var var) = expected)
    in
      OUnit.assert_bool
	("id_of_var: " ^ Var.to_string var)
	success_desired
	success

let not_id_of_var =
  id_of_var ~success_desired:false



let test_id_of_var1 =
  OUnit.TestCase (id_of_var var1 1)

let test_id_of_var2 =
  OUnit.TestCase (id_of_var var2 2)

let test_id_of_var3 =
  OUnit.TestCase (not_id_of_var var1 0)

let test_id_of_var4 =
  OUnit.TestCase (id_of_var par1 1)

let test_id_of_var5 =
  OUnit.TestCase (not_id_of_var par2 1)



let suite_id_of_var =  
  OUnit.TestLabel (
    ("equal"),
    (OUnit.TestList [
       test_id_of_var1;
       test_id_of_var2;
       test_id_of_var3;
       test_id_of_var4;
       test_id_of_var5
     ]
    )
  )







(*** clone_renumbered ***)
let clone_renumbered (var: var) (new_number: int) =
  fun () ->
    let renumbered_var =
      Var.clone_renumbered var new_number
    in
    let success =
      (Var.is_universal var = Var.is_universal renumbered_var)
      &&
      (Var.id_of_var renumbered_var = new_number)
    in
      OUnit.assert_true
	("clone_renumbered: " ^ Var.to_string var ^ " renumbered to " ^ string_of_int new_number
	 ^ " is " ^ Var.to_string renumbered_var)
	success


let test_clone_renumbered1 =
  OUnit.TestCase (clone_renumbered var1 1)

let test_clone_renumbered2 =
  OUnit.TestCase (clone_renumbered var1 2)

let test_clone_renumbered3 =
  OUnit.TestCase (clone_renumbered par2 2)



let suite_clone_renumbered =  
  OUnit.TestLabel (
    ("equal"),
    (OUnit.TestList [
       test_clone_renumbered1;
       test_clone_renumbered2;
       test_clone_renumbered3
     ]
    )
  )





(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_is_universal;
    suite_equal;
    suite_id_of_var;
    suite_clone_renumbered
  ];;


ignore (OUnit.run_test_tt_main test_suite);;

