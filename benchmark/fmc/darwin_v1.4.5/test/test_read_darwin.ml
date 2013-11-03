(* $Id: test_read_darwin.ml,v 1.5 2004/04/29 09:24:21 alexf Exp $ *)

type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause


(*** constants ***)

let c_a = Symbol.create_symbol "a" 0;;
let c_b = Symbol.create_symbol "b" 0;;
let c_f = Symbol.create_symbol "f" 2;;
let c_g = Symbol.create_symbol "g" 1;;
let c_h = Symbol.create_symbol "h" 2;;
let c_i = Symbol.create_symbol "i" 1;;
let c_j = Symbol.create_symbol "j" 3;;



(*** terms ***)

(* a *)
let t_a =
  Term.request_const c_a;;

(* g(a) *)
let t_g_a =
  Term.request_func (c_g, [| Term.request_const c_a |]);;

(* f(g(a), a) *)
let t_f_g_a_a =
  Term.request_func (c_f, [| 
			Term.request_func (c_g, [| Term.request_const c_a |]);
			Term.request_const c_a
		      |]);;

(* f(g(a, a), a) *)
let t_f_h_aa_a =
  Term.request_func (c_f, [| 
			Term.request_func (c_h, 
					    [| Term.request_const c_a; 
					       Term.request_const c_a |]);
			Term.request_const c_a
		      |]);;

(* f(g(a), a, a) *)
let t_j_g_a_aa = 
  Term.request_func (c_j, [| 
			Term.request_func (c_g, [| Term.request_const c_a |]);
			Term.request_const c_a;
			Term.request_const c_a
		      |]);;

(* f(g(a), a, b) *)
let t_j_g_a_ab = 
  Term.request_func (c_j, [| 
			Term.request_func (c_g, [| Term.request_const c_a |]);
			Term.request_const c_a;
			Term.request_const c_b
		      |]);;

(* f(x, a) *)
let t_f_xa = 
  Term.request_func (c_f, [|
			Term.request_var (Var.create_universal 0);
			Term.request_const c_a
		      |]);;

(* f(x, y) *)
let t_f_xy = 
  Term.request_func (c_f, [|
			Term.request_var (Var.create_universal 0);
			Term.request_var (Var.create_universal 1)
		      |]);;


(* f(z, z) *)
let t_f_xx = 
  Term.request_func (c_f, [|
			Term.request_var (Var.create_universal 0);
			Term.request_var (Var.create_universal 0)
		      |]);;


(* f(g(x), y) *)
let t_f_g_x_y = 
  Term.request_func (c_f, [| 
			Term.request_func (c_g, [| Term.request_var (Var.create_universal 0) |]);
			Term.request_var (Var.create_universal 1)
		      |]);;

(* f(g(x), x) *)
let t_f_g_x_x = 
  Term.request_func (c_f, [| 
			Term.request_func (c_g, [| Term.request_var (Var.create_universal 0)|]);
			Term.request_var (Var.create_universal 0)
		      |]);;


(* f(g(x), y, z) *)
let t_j_g_x_yz = 
  Term.request_func (c_j, [| 
			Term.request_func (c_g, [| Term.request_var (Var.create_universal 0)|]);
			Term.request_var (Var.create_universal 1);
			Term.request_var (Var.create_universal 2)
		      |]);;

(* f(u, a) *)
let t_f_ua = 
  Term.request_func (c_f, [|
			Term.request_var (Var.create_parametric 0);
			Term.request_const c_a
		      |]);;

(* f(u, v) *)
let t_f_uv = 
  Term.request_func (c_f, [|
			Term.request_var (Var.create_parametric 0);
			Term.request_var (Var.create_parametric 1)
		      |]);;

(* f(u, y) *)
let t_f_uy = 
  Term.request_func (c_f, [|
			Term.request_var (Var.create_parametric 0);
			Term.request_var (Var.create_universal 1)
		      |]);;



(*** literals ***)
let l_f_xa =
  Term.request_literal true t_f_xa

let l_not_f_xa =
  Term.request_literal false t_f_xa

let l_f_g_x_y =
  Term.request_literal true t_f_g_x_y

let l_not_f_g_x_y =
  Term.request_literal false t_f_g_x_y

let l_p =
  Term.request_literal true (Term.request_const (Symbol.create_symbol "p" 0))

let l_not_p =
  Term.request_literal false (Term.request_const (Symbol.create_symbol "p" 0))



(*** clauses ***)
let c_f_uv = [
  Term.request_literal true t_f_uv;
]

let c_not_f_uv = [
  Term.request_literal false t_f_uv;
]

let c_f_uv__not_f_uv = [
  Term.request_literal true t_f_uv;
  Term.request_literal false t_f_uv;
]

let c_j_g_a_aa__not_j_g_a_ab__f_xa = [
  Term.request_literal true t_j_g_a_aa;
  Term.request_literal false t_j_g_a_ab;
  Term.request_literal true t_f_xa;
]

let c_f_xa__not_f_xx__f_xy__f_g_x_y = [
  Term.request_literal true t_f_xa;
  Term.request_literal false t_f_xx;
  Term.request_literal true t_f_xy;
  Term.request_literal true t_f_g_x_y;
]











(*** to_var ***)
let to_var ?(success_desired:bool = true) (convert: string) (expected: var) =
  fun () ->
    let converted =
      Read_darwin.to_var convert
    in
    let success =
      Var.equal converted expected
    in
      OUnit.assert_bool
	("to_var:" ^ convert ^ " is\n" 
	 ^ Var.to_string converted ^ " but should be\n" 
	 ^ Var.to_string expected)
	success_desired
	success


let not_to_var =
  to_var ~success_desired:false




let test_to_var1 =
  OUnit.TestCase (to_var "X" (Var.create_universal 0))

let test_to_var2 =
  OUnit.TestCase (to_var "Y" (Var.create_universal 0))

let test_to_var3 =
  OUnit.TestCase (not_to_var "X" (Var.create_universal 1))

let test_to_var4 =
  OUnit.TestCase (to_var "=X" (Var.create_parametric 0))


let suite_to_var =  
  OUnit.TestLabel (
    ("to_var"),
    (OUnit.TestList [
       test_to_var1;
       test_to_var2;
       test_to_var3;
       test_to_var4
     ]
    )
  )







(*** to_term ***)
let to_term ?(success_desired:bool = true) (convert: string) (expected: term) =
  fun () ->
    let converted =
      Read_darwin.to_term convert
    in
    let success =
      Term.term_equal converted expected
    in
      OUnit.assert_bool
	("to_term: " ^ convert ^ " is\n" 
	 ^ Term.term_to_string converted ^ " but should be\n" 
	 ^ Term.term_to_string expected)
	success_desired
	success


let not_to_term =
  to_term ~success_desired:false




let test_to_term1 =
  OUnit.TestCase (to_term "a" t_a)

let test_to_term2 =
  OUnit.TestCase (to_term "a" t_a)

let test_to_term3 =
  OUnit.TestCase (to_term "g(a)" t_g_a)

let test_to_term4 =
  OUnit.TestCase (to_term "f(g(a), a)" t_f_g_a_a)

let test_to_term5 =
  OUnit.TestCase (to_term "f(h(a, a), a)" t_f_h_aa_a)

let test_to_term6 =
  OUnit.TestCase (to_term "j(g(a), a, a)" t_j_g_a_aa)

let test_to_term7 =
  OUnit.TestCase (to_term "j(g(a), a, b)" t_j_g_a_ab)

let test_to_term8 =
  OUnit.TestCase (to_term "f(X, a)" t_f_xa)

let test_to_term9 =
  OUnit.TestCase (to_term "f(X, Y)" t_f_xy)

let test_to_term10 =
  OUnit.TestCase (to_term "f(X, X)" t_f_xx)

let test_to_term11 =
  OUnit.TestCase (to_term "f(g(X), Y)" t_f_g_x_y)

let test_to_term12 =
  OUnit.TestCase (to_term "f(g(X), X)" t_f_g_x_x) 

let test_to_term13 =
  OUnit.TestCase (to_term "j(g(X), Y, Z)" t_j_g_x_yz) 

let test_to_term14 =
  OUnit.TestCase (to_term "f(=U, a)" t_f_ua) 

let test_to_term15 =
  OUnit.TestCase (to_term "f(=U, =V)" t_f_uv) 

let test_to_term16 =
  OUnit.TestCase (to_term "f(=U, Y)" t_f_uy) 

let test_to_term17 =
  OUnit.TestCase (not_to_term "f(X, Y)" t_f_uy) 

let test_to_term18 =
  OUnit.TestCase (not_to_term "i(a)" t_g_a)

let test_to_term19 =
  OUnit.TestCase (not_to_term "g(b)" t_g_a)

let test_to_term20 =
  OUnit.TestCase (not_to_term "g(X)" t_g_a)


let suite_to_term =
  OUnit.TestLabel (
    ("to_term"),
    (OUnit.TestList [
       test_to_term1;
       test_to_term2;
       test_to_term3;
       test_to_term4;
       test_to_term5;
       test_to_term6;
       test_to_term7;
       test_to_term8;
       test_to_term9;
       test_to_term10;
       test_to_term11;
       test_to_term12;
       test_to_term13;
       test_to_term14;
       test_to_term15;
       test_to_term16;
       test_to_term17;
       test_to_term18;
       test_to_term19;
       test_to_term20;
     ]
    )
  )








(*** to_literal ***)
let to_literal ?(success_desired:bool = true) (convert: string) (expected: literal) =
  fun () ->
    let converted =
      Read_darwin.to_literal convert
    in
    let success =
      Term.literal_equal converted expected
    in
      OUnit.assert_bool
	("to_literal: " ^ convert ^ " is\n" 
	 ^ Term.literal_to_string converted ^ " but should be\n" 
	 ^ Term.literal_to_string expected)
	success_desired
	success


let not_to_literal =
  to_literal ~success_desired:false




let test_to_literal1 =
  OUnit.TestCase (to_literal "f(X, a)" l_f_xa)

let test_to_literal2 =
  OUnit.TestCase (to_literal "-f(X, a)" l_not_f_xa)

let test_to_literal3 =
  OUnit.TestCase (to_literal "f(g(X), Y)" l_f_g_x_y)

let test_to_literal4 =
  OUnit.TestCase (to_literal "-f(g(X), Y)" l_not_f_g_x_y)

let test_to_literal5 =
  OUnit.TestCase (to_literal "p" l_p)

let test_to_literal6 =
  OUnit.TestCase (to_literal "-p" l_not_p)



let suite_to_literal =
  OUnit.TestLabel (
    ("to_literal"),
    (OUnit.TestList [
       test_to_literal1;
       test_to_literal2;
       test_to_literal3;
       test_to_literal4;
       test_to_literal5;
       test_to_literal6
     ]
    )
  )









(*** to_clause ***)
let to_clause ?(success_desired:bool = true) (convert: string) (expected: clause) =
  fun () ->
    let converted =
      Read_darwin.to_clause convert
    in
    let success =
      Term.clause_equal converted expected
    in
      OUnit.assert_bool
	("to_clause: " ^ convert ^ " is\n" 
	 ^ Term.clause_to_string converted ^ " but should be\n" 
	 ^ Term.clause_to_string expected)
	success_desired
	success


let not_to_clause =
  to_clause ~success_desired:false





let test_to_clause1 =
  OUnit.TestCase (to_clause "{ f(=U, =V) }" c_f_uv)

let test_to_clause2 =
  OUnit.TestCase (to_clause "{ -f(=U, =V) }" c_not_f_uv)

let test_to_clause3 =
  OUnit.TestCase (to_clause "{ f(=U, =V), -f(=U, =V) }" c_f_uv__not_f_uv)

let test_to_clause4 =
  OUnit.TestCase (not_to_clause "{ f(=U, =V) }" c_f_uv__not_f_uv)

let test_to_clause5 =
  OUnit.TestCase (to_clause "{ j(g(a), a, a), -j(g(a), a, b), f(X, a) }" c_j_g_a_aa__not_j_g_a_ab__f_xa)

let test_to_clause6 =
  OUnit.TestCase (to_clause "{ f(X, a), -f(X, X), f(X, Y), f(g(X), Y) }" c_f_xa__not_f_xx__f_xy__f_g_x_y)



let suite_to_clause =
  OUnit.TestLabel (
    ("to_clause"),
    (OUnit.TestList [
       test_to_clause1;
       test_to_clause2;
       test_to_clause3;
       test_to_clause4;
       test_to_clause5;
       test_to_clause6
     ]
    )
  )





(*** to_clauses ***)


let clauses_to_string (clauses: Term.clause list) : string =
  String.concat "\n" (List.map Term.clause_to_string clauses)

let to_clauses ?(success_desired:bool = true) (clauses: string) (expected: string list) =
  fun () ->
    let converted =
      Read_darwin.to_clauses clauses
    in
    let expected_clauses =
      List.map Read_darwin.to_clause expected
    in
    let success =
      List.for_all2 Term.clause_equal converted expected_clauses
    in
      OUnit.assert_bool
	("to_clauses: should be\n" 
	 ^ clauses_to_string expected_clauses
	 ^ "\nbut is\n" ^ clauses_to_string converted)
	success_desired
	success
	


let test_to_clauses1 =
  OUnit.TestCase (to_clauses
		    "{ lives(agatha) }
{ lives(butler) }
{ lives(charles) }
{ -killed(X, Y), -richer(X, Y) }
{ -hates(agatha, X), -hates(charles, X) }
{ -hates(X, agatha), -hates(X, butler), -hates(X, charles) }
{ hates(agatha, agatha) }
{ hates(agatha, charles) }
{ hates(X, Y), -killed(X, Y) }
{ hates(butler, X), -hates(agatha, X) }
{ richer(X, agatha), hates(butler, X), -lives(X) }
{ killed(butler, agatha), killed(charles, agatha)}"
		    [
		      "{ lives(agatha) }"; 
		      "{ lives(butler) }"; 
		      "{ lives(charles) }"; 
		      "{ -killed(X, Y), -richer(X, Y) }"; 
		      "{ -hates(agatha, X), -hates(charles, X) }"; 
		      "{ -hates(X, agatha), -hates(X, butler), -hates(X, charles) }"; 
		      "{ hates(agatha, agatha) }"; 
		      "{ hates(agatha, charles) }"; 
		      "{ hates(X, Y), -killed(X, Y) }"; 
		      "{ hates(butler, X), -hates(agatha, X) }"; 
		      "{ richer(X, agatha), hates(butler, X), -lives(X) }"; 
		      "{ killed(butler, agatha), killed(charles, agatha) }"; 
		    ]
		 )


let suite_to_clauses =
  OUnit.TestLabel (
    ("to_clauses"),
    (OUnit.TestList [
       test_to_clauses1
     ]
    )
  )






(*** to_clauses = read file ***)
let to_clauses_from_file ?(success_desired:bool = true) (file: string) (expected: string list) =
  fun () ->
    let converted =
      Read_darwin.to_clauses_from_file file
    in
    let expected_clauses =
      List.map Read_darwin.to_clause expected
    in
    let success =
      List.for_all2 Term.clause_equal converted expected_clauses
    in
      OUnit.assert_bool
	("to_clauses: should be\n" 
	 ^ clauses_to_string expected_clauses
	 ^ "\nbut is\n" ^ clauses_to_string converted)
	success_desired
	success
	


let test_to_clauses_from_file1 =
  OUnit.TestCase (to_clauses_from_file "PUZ001-1.darwin"
		    [
		      "{ lives(agatha) }"; 
		      "{ lives(butler) }"; 
		      "{ lives(charles) }"; 
		      "{ -killed(X, Y), -richer(X, Y) }"; 
		      "{ -hates(agatha, X), -hates(charles, X) }"; 
		      "{ -hates(X, agatha), -hates(X, butler), -hates(X, charles) }"; 
		      "{ hates(agatha, agatha) }"; 
		      "{ hates(agatha, charles) }"; 
		      "{ hates(X, Y), -killed(X, Y) }"; 
		      "{ hates(butler, X), -hates(agatha, X) }"; 
		      "{ richer(X, agatha), hates(butler, X), -lives(X) }"; 
		      "{ killed(butler, agatha), killed(charles, agatha) }"; 
		    ]
		 )


let suite_to_clauses_from_file =
  OUnit.TestLabel (
    ("to_clauses_from_file"),
    (OUnit.TestList [
       test_to_clauses_from_file1
     ]
    )
  )



(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_to_var;
    suite_to_term;
    suite_to_literal;
    suite_to_clause;
    suite_to_clauses;
    suite_to_clauses_from_file;
  ];;


ignore (OUnit.run_test_tt_main test_suite);;

