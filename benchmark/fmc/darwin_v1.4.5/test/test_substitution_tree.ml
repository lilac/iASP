(* $Id: test_substitution_tree.ml,v 1.7 2004/06/13 18:57:28 alexf Exp $ *)

type term = Term.term
type subst = Subst.subst

module TermSubstitutionTree =
  Substitution_tree.Make (
    struct
      type t = term

      let is_equal =
	Term.term_equal

      let to_string (term: t) : string =
	Term.term_to_string term
    end
  )

type substitution_tree =
    TermSubstitutionTree.t


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
let q_uv = Read_darwin.to_term "q(=U, =V)"
let q_uu = Read_darwin.to_term "q(=U, =U)"
let q_xx = Read_darwin.to_term "q(X, X)"
let q_xb = Read_darwin.to_term "q(X, b)"
let q_bx = Read_darwin.to_term "q(b, X)"
let q_ab = Read_darwin.to_term "q(a, b)"
let q_bb = Read_darwin.to_term "q(b, b)"
let q_aa = Read_darwin.to_term "q(a, a)"
let q_b_g_b = Read_darwin.to_term "q(b, g(b))"
let w_axx = Read_darwin.to_term "w(a, X, X)"
let w_xxa = Read_darwin.to_term "w(X, X, a)"
let w_xxy = Read_darwin.to_term "w(X, X, Y)"
let w_axa = Read_darwin.to_term "w(a, X, a)"
let w_yxx = Read_darwin.to_term "w(Y, X, X)"
let w_xyz = Read_darwin.to_term "w(X, Y, Z)"

let q_01 =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 0); Term.request_var (Var.create_universal 1) |]
  )
let q_12 =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 1); Term.request_var (Var.create_universal 2) |]
  )
let q_00 =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 0); Term.request_var (Var.create_universal 0) |]
  )
let q_33 =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 3); Term.request_var (Var.create_universal 3) |]
  )





let validate_tree (tree: substitution_tree) : unit =
  TermSubstitutionTree.iter
    (fun content data ->
       if not (Unification.are_terms_variants content data) then begin
	 print_endline (TermSubstitutionTree.to_string tree);
	 OUnit.assert_failure
	   ("validate_tree: "
	    ^ " node contains \n" ^ Term.term_to_string content
	    ^ " but should contain\n" ^ Term.term_to_string data)
       end
    )
    tree

let validate_tree_contains (tree: substitution_tree) (expected: 'a list) : unit =
  List.iter
    (fun data ->
       let contained =
	 TermSubstitutionTree.find_all_variants tree data
       in
	 match contained with
	   | [] ->
	       print_endline (TermSubstitutionTree.to_string tree);
	       OUnit.assert_failure
		 ("validate_tree_contains: tree does not contain "
		  ^ Term.term_to_string data)
	       
	   | data :: [] ->
	       ()

	   | data :: _ ->
	       ()
(*	       print_endline (TermSubstitutionTree.to_string tree);
	       OUnit.assert_failure
	       ("validate_tree_contains: tree contains several instances of "
		^ Term.term_to_string data
		^ " : " ^ String.concat ", " (List.map Term.term_to_string contained))*)
	       
    )
    expected



let build_tree (nodes: string list) : substitution_tree =
  let tree =
    TermSubstitutionTree.create ()
  in
  let terms =
    List.map Read_darwin.to_term nodes
  in
(*    print_endline (TermSubstitutionTree.to_string tree);*)
    List.iter
      (fun term ->
(*	 print_endline ("ADD: " ^ Term.term_to_string term);*)
	 TermSubstitutionTree.add tree term term;
	 validate_tree tree;
(*	 print_endline (TermSubstitutionTree.to_string tree);*)
      )
      terms;

    validate_tree_contains tree terms;
(*	 print_endline (TermSubstitutionTree.to_string tree);*)

    tree;;

(*
build_tree ["q(a, a)"; "q(b, b)"; "q(X, X)"];;
build_tree ["my3(a, a, c)"; "my3(b, b, c)"; "my3(X, X, Y)"];;
build_tree ["my3(a, a, c)"; "my3(b, b, c)"; "my3(X, Y, Z)"; "my3(X, X, Y)"];;
build_tree ["my3(a, a, b)"; "my3(b, b, c)"; "my3(X, Y, Z)"; "my3(X, X, c)"];;
build_tree ["my2(X, X)"; "my2(X, Y)"];;
build_tree ["my2(X, Y)"; "my2(X, X)"];;
build_tree ["q(X, Y)"; "q(b, X)"; "q(X, X)"];;
build_tree ["q(X, Y)"; "q(X, X)"; "q(b, X)"];;
build_tree ["q(X, Y)"; "q(X, X)"; "q(X, b)"];;
build_tree ["w(X, Y, Z)"; "w(X, X, Y)"; "w(X, a, a)"];;*)
(*build_tree ["my2(X, Y)"; "my2(X, X)"; "my2(a, g(Z))"; "my2(g(X), g(X))";
"my2(g(d), g(X))"];;*)
(*build_tree ["my2(Z, g(b))"; "my2(a, b)"; "my2(c, g(d))"; "my2(b, g(a))"];;
build_tree ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(X, X)"];;
build_tree ["my3(d, e, d)"; "my3(b, b, c)"; "my3(X, b, c)"; "my3(X, Y, c)"; "my3(X, X, c)"];;
build_tree ["my3(b, b, b)"; "my3(X, X, b)"; "my3(X, X, Y)"; "my3(X, X, X)"];;

build_tree ["my2(X, Y)"; "my2(my1a(a), my1b(a))"; "my2(my1a(b), my1b(b))"];;
build_tree ["my3(X, Y, Z)"; "my3(a, my1a(a), my1b(a))"; "my3(a, my1a(b), my1b(b))"];;
build_tree ["my3(X, Y, Z)"; "my3(my1a(a), my1b(a), a)"; "my3(my1a(b), my1b(b), a)"];;*)
(*build_tree ["my2(a, a)"; "my2(a, b)"; "my2(b, a)"; "my2(a, a)"; "my2(b, a)"];;*)
(*build_tree ["my2(a, X)"; "my2(a, a)"; "my2(a, b)"; "my2(X, a)"; "my2(a, a)"; "my2(b, a)"; ];;*)

(* producivity *)
(*
let tree =
  build_tree ["my3(a, a, b)"; "my3(a, b, b)"; "my3(=X, a, a)"]
in
let producer =
  Read_darwin.to_term "my3(=Y, =X, =X)"
in
let produced =
  Read_darwin.to_term "my3(a, a, a)"
in
*)

(*let tree =
  build_tree ["my3(a, a, b)"; "my3(a, b, b)"; "my3(=X, =Y, a)"]
in
let producer =
  Read_darwin.to_term "my3(=Y, =X, =X)"
in
let produced =
  Read_darwin.to_term "my3(a, a, a)"
in
  match TermSubstitutionTree.find_non_productive tree producer produced with
    | None ->
	print_endline ("PRODUCED");
	
    | Some element ->
	print_endline ("NOT PRODUCED: " ^ Term.term_to_string element);;
*)

(*exit 0;;*)

(*** insert ***)
let insert ?(success_desired:bool = true) (tree: substitution_tree) (term: term) =
  fun () ->
    let old_representation =
      TermSubstitutionTree.to_string tree
    in
      TermSubstitutionTree.add tree term term;
      validate_tree tree(*;
      print_endline ("insert:\n"
		     ^ Term.term_to_string term
		     ^ "\ninto\n"
		     ^ old_representation
		     ^ "\nis\n"
		     ^ TermSubstitutionTree.to_string tree Term.term_to_string)*)



let tree =
  build_tree ["q(X, Y)"]
let test_insert1 =
  OUnit.TestCase (insert tree q_xb)

let tree =
  build_tree ["q(=X, =X)"]
let test_insert2 =
  OUnit.TestCase (insert tree q_uv)

let tree =
  build_tree ["q(X, X)"]
let test_insert3 =
  OUnit.TestCase (insert tree q_uu)

let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(=U, =V)"; "q(b, X)"]
let test_insert4 =
  OUnit.TestCase (insert tree q_uu)


let tree =
  build_tree ["q(b, b)"; "q(X, X)"]
let test_insert5 =
  OUnit.TestCase (insert tree q_xy)


let tree =
  build_tree ["q(X, Y)"]
let test_insert6 =
  OUnit.TestCase (insert tree q_xx)
  

let tree =
  build_tree ["q(X, Y)"; "q(b, b)"]
let test_insert7 =
  OUnit.TestCase (insert tree q_xb)
  

let tree =
  build_tree ["q(X, Y)"]
let test_insert8 =
  OUnit.TestCase (insert tree q_bx)


let tree =
  build_tree ["q(X, Y)"; "q(b, X)"]
let test_insert9 =
  OUnit.TestCase (insert tree q_xx)


let tree =
  build_tree ["q(b, b)"; "q(b, X)"]
let test_insert10 =
  OUnit.TestCase (insert tree q_xy)


let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(=U, =V)"; "q(b, X)"]
let test_insert11 =
  OUnit.TestCase (insert tree q_uu)
  

let tree =
  build_tree ["q(a, a)"]
let test_insert12 =
  OUnit.TestCase (insert tree q_bb)


let tree =
  build_tree ["q(X, Y)"; "q(=U, =V)"]
let test_insert13 =
  OUnit.TestCase (insert tree q_uu)

let tree =
  build_tree ["w(X, X, a)"; "w(a, X, X)"]
let test_insert14 =
  OUnit.TestCase (insert tree w_axa)



let tree =
  build_tree ["w(X, Y, Z)"; "w(X, X, Y)"]
let test_insert15 =
  OUnit.TestCase (insert tree w_xxa)

let suite_insert =
  OUnit.TestLabel (
    ("insert"),
    (OUnit.TestList [
       test_insert1;
       test_insert2;
       test_insert3;
       test_insert4; 
       test_insert5; 
       test_insert6;
       test_insert7;
       test_insert8;
       test_insert9;
       test_insert10;
       test_insert11;
       test_insert12;
       test_insert13;
       test_insert14;
       test_insert15;
     ]
    )
  )





(*** remove_variant ***)
let remove_variant ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in

    let old_representation =
      TermSubstitutionTree.to_string tree
    in
      
      ignore (TermSubstitutionTree.remove tree term (Some term));
      
      validate_tree_contains tree expected;

      let contained =
	TermSubstitutionTree.find_all_variants tree term
      in
	match contained with
	  | [] ->
	      ()

	  | _ ->
	      OUnit.assert_failure
	      ("remove_variant:\n"
	       ^ old_representation
	       ^ " and\n" ^ Term.term_to_string term
	       ^  " is\n "
	       ^ TermSubstitutionTree.to_string tree)

let tree =
  build_tree ["q(X, Y)"]
let test_remove_variant1 =
  OUnit.TestCase (remove_variant tree "q(X, Y)" [])

let tree =
  build_tree ["q(X, Y)"];;
let test_remove_variant2 =
  OUnit.TestCase (remove_variant tree "q(a, a)" ["q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, =X)"; "q(=X, =Y)"]
let test_remove_variant3 =
  OUnit.TestCase (remove_variant tree "q(a, b)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, =X)"; "q(=X, =Y)"])
let test_remove_variant4 =
  OUnit.TestCase (remove_variant tree "q(=X, b)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, =X)"; "q(=X, =Y)"])
let test_remove_variant5 =
  OUnit.TestCase (remove_variant tree "q(b, X)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, =X)"; "q(=X, =Y)"])
let test_remove_variant6 =
  OUnit.TestCase (remove_variant tree "q(X, X)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(b, g(b))"; "q(b, =X)"; "q(=X, =Y)"])
let test_remove_variant7 =
  OUnit.TestCase (remove_variant tree "q(b, =X)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(b, g(b))"; "q(=X, =Y)"])
let test_remove_variant8 =
  OUnit.TestCase (remove_variant tree "q(X, Y)" ["q(b, b)"; "q(X, b)"; "q(b, g(b))"; "q(=X, =Y)"])
let test_remove_variant9 =
  OUnit.TestCase (remove_variant tree "q(X, b)" ["q(b, b)"; "q(b, g(b))"; "q(=X, =Y)"])
let test_remove_variant10 =
  OUnit.TestCase (remove_variant tree "q(b, b)" ["q(b, g(b))"; "q(=X, =Y)"])
let test_remove_variant11 =
  OUnit.TestCase (remove_variant tree "q(b, g(b))" ["q(=X, =Y)"])
let test_remove_variant12 =
  OUnit.TestCase (remove_variant tree "q(=X, =Y)" [])


let suite_remove_variant =
  OUnit.TestLabel (
    ("remove_variant"),
    (OUnit.TestList [
       test_remove_variant1;
       test_remove_variant2;
       test_remove_variant3;
       test_remove_variant4;
       test_remove_variant5;
       test_remove_variant6;
       test_remove_variant7;
       test_remove_variant8;
       test_remove_variant9;
       test_remove_variant10;
       test_remove_variant11;
       test_remove_variant12;
     ]
    )
  )





(*** p_generalization ***)
let p_generalization ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in

    let nodes =
      TermSubstitutionTree.find_all_generalizations ~p_preserving:true tree term
    in
    let all_success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("p_generalization all:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	all_success;

    let node =
      TermSubstitutionTree.find_generalization ~p_preserving:true tree term
    in
    let first_success =
      match node with
	| None ->
	    List.length expected = 0

	| Some term ->
	    List.exists (Unification.are_terms_variants term) expected
    in
      OUnit.assert_bool
	("p_generalization first:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ (match node with None -> "None" | Some term -> Term.term_to_string term)
	 ^  " but should be in\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	first_success


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_p_generalization1 =
  OUnit.TestCase (p_generalization tree "q(X, X)" ["q(X, X)"; "q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_p_generalization2 =
  OUnit.TestCase (p_generalization tree "q(X, Y)" ["q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_p_generalization3 =
  OUnit.TestCase (p_generalization tree "q(b, b)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"]
let test_p_generalization4 =
  OUnit.TestCase (p_generalization tree "q(X, Y)" [])


let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, X)"; "q(=X, =Y)"]
let test_p_generalization5 =
  OUnit.TestCase (p_generalization tree "q(b, b)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(X, X)"; "q(b, X)"])
let test_p_generalization6 =
  OUnit.TestCase (p_generalization tree "q(X, b)" ["q(X, b)"; "q(X, Y)"])
let test_p_generalization7 =
  OUnit.TestCase (p_generalization tree "q(a, b)" ["q(X, Y)"; "q(X, b)"; "q(a, b)"])
let test_p_generalization8 =
  OUnit.TestCase (p_generalization tree "q(b, g(b))" ["q(X, Y)"; "q(b, g(b))"; "q(b, X)"])
let test_p_generalization9 =
  OUnit.TestCase (p_generalization tree "q(=X, =X)" ["q(X, Y)"; "q(X, X)"])

let tree =
  build_tree ["q(a, a)"; "q(b, b)"]
let test_p_generalization10 =
  OUnit.TestCase (p_generalization tree "q(b, b)" ["q(b, b)"])
let test_p_generalization11 =
  OUnit.TestCase (p_generalization tree "q(a, a)" ["q(a, a)"])
let test_p_generalization12 =
  OUnit.TestCase (p_generalization tree "q(X, X)" [])
let test_p_generalization13 =
  OUnit.TestCase (p_generalization tree "q(a, b)" [])


let tree =
  build_tree ["q(a, b)"; "q(b, a)"]
let test_p_generalization14 =
  OUnit.TestCase (p_generalization tree "q(a, b)" ["q(a, b)"])
let test_p_generalization15 =
  OUnit.TestCase (p_generalization tree "q(b, a)" ["q(b, a)"])
let test_p_generalization16 =
  OUnit.TestCase (p_generalization tree "q(a, a)" [])
let test_p_generalization17 =
  OUnit.TestCase (p_generalization tree "q(b, b)" [])
let test_p_generalization18 =
  OUnit.TestCase (p_generalization tree "q(X, Y)" [])


let tree =
  build_tree ["q(a, b)"; "q(b, a)"; "q(a, a)"; "q(b, b)"]
let test_p_generalization19 =
  OUnit.TestCase (p_generalization tree "q(a, b)" ["q(a, b)"])
let test_p_generalization20 =
  OUnit.TestCase (p_generalization tree "q(b, a)" ["q(b, a)"])
let test_p_generalization21 =
  OUnit.TestCase (p_generalization tree "q(a, a)" ["q(a, a)"])
let test_p_generalization22 =
  OUnit.TestCase (p_generalization tree "q(b, b)" ["q(b, b)"])
let test_p_generalization23 =
  OUnit.TestCase (p_generalization tree "q(X, X)" [])
let test_p_generalization24 =
  OUnit.TestCase (p_generalization tree "q(X, Y)" [])


let tree =
  build_tree ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"; "q(b, X)" ]
let test_p_generalization25 =
  OUnit.TestCase (p_generalization tree "q(a, a)" ["q(a, a)"; "q(X, X)"])
let test_p_generalization26 =
  OUnit.TestCase (p_generalization tree "q(b, b)" ["q(b, b)"; "q(X, X)"; "q(b, X)"])
let test_p_generalization27 =
  OUnit.TestCase (p_generalization tree "q(X, X)" ["q(X, X)"])
let test_p_generalization28 =
  OUnit.TestCase (p_generalization tree "q(=V, =V)" ["q(=U, =U)"; "q(X, X)"])
let test_p_generalization29 =
  OUnit.TestCase (p_generalization tree "q(a, b)" [])
let test_p_generalization30 =
  OUnit.TestCase (p_generalization tree "q(X, Y)" [])




let tree =
  build_tree [
    "g(f(a, a))";
  ]
let test_p_generalization31 =
  OUnit.TestCase (p_generalization tree "g(f(a, a))" ["g(f(a, a))"])

let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
  ]
let test_p_generalization32 =
  OUnit.TestCase (p_generalization tree "g(f(a, a))" ["g(f(a, a))"])
let test_p_generalization33 =
  OUnit.TestCase (p_generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])


let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
  ]
let test_p_generalization34 =
  OUnit.TestCase (p_generalization tree "g(f(a, a))" ["g(f(a, a))"])
let test_p_generalization35 =
  OUnit.TestCase (p_generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])
let test_p_generalization36 =
  OUnit.TestCase (p_generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))"])


  
let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))";
  ]
let test_p_generalization37 =
  OUnit.TestCase (p_generalization tree "g(f(a, a))" ["g(f(a, a))"])
let test_p_generalization38 =
  OUnit.TestCase (p_generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])
let test_p_generalization39 =
  OUnit.TestCase (p_generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))"])
let test_p_generalization40 =
  OUnit.TestCase (p_generalization tree "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))" ["g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))"])


(*
let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
(*    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), a), a))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(X3, f(f(f(X3, X4), f(X5, X4)), X5))), f(a, a)))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(X3, f(f(f(X3, X4), f(X5, X4)), X5))), f(f(X6, f(f(f(X6, X7), f(X8, X7)), X8)), f(a, a))))";*)
  ]
let test_p_generalization31 =
  OUnit.TestCase (p_generalization tree "g(f(a, a))" [0])
let test_p_generalization32 =
  OUnit.TestCase (p_generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" [1])
let test_p_generalization33 =
  OUnit.TestCase (p_generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" [2])
  *)

let suite_p_generalization =
  OUnit.TestLabel (
    ("p_generalization"),
    (OUnit.TestList [
       test_p_generalization1;
       test_p_generalization2;
       test_p_generalization3;
       test_p_generalization4;
       test_p_generalization5;
       test_p_generalization6;
       test_p_generalization7;
       test_p_generalization8;
       test_p_generalization9;
       test_p_generalization10;
       test_p_generalization11;
       test_p_generalization12;
       test_p_generalization13;
       test_p_generalization14;
       test_p_generalization15;
       test_p_generalization16;
       test_p_generalization17;
       test_p_generalization18;
       test_p_generalization19;
       test_p_generalization20;
       test_p_generalization21;
       test_p_generalization22;
       test_p_generalization23;
       test_p_generalization24;
       test_p_generalization25;
       test_p_generalization26;
       test_p_generalization27;
       test_p_generalization28;
       test_p_generalization29;
       test_p_generalization30;
       test_p_generalization31;
       test_p_generalization32;
       test_p_generalization33;
       test_p_generalization34;
       test_p_generalization35;
       test_p_generalization36;
       test_p_generalization37;
       test_p_generalization38;
       test_p_generalization39;
       test_p_generalization40;
     ]
    )
  )







(*** generalization ***)
let generalization ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.find_all_generalizations ~p_preserving:false tree term
    in
    let success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("generalization:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	success;

    let node =
      TermSubstitutionTree.find_generalization ~p_preserving:false tree term
    in
    let first_success =
      match node with
	| None ->
	    List.length expected = 0

	| Some term ->
	    List.exists (Unification.are_terms_variants term) expected
    in
      OUnit.assert_bool
	("p_generalization first:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ (match node with None -> "None" | Some term -> Term.term_to_string term)
	 ^  " but should be in\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	first_success


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_generalization1 =
  OUnit.TestCase (generalization tree "q(X, X)" ["q(X, X)"; "q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_generalization2 =
  OUnit.TestCase (generalization tree "q(X, Y)" ["q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_generalization3 =
  OUnit.TestCase (generalization tree "q(b, b)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"]
let test_generalization4 =
  OUnit.TestCase (generalization tree "q(X, Y)" [])


let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, X)"; "q(=X, =Y)"]
let test_generalization5 =
  OUnit.TestCase (generalization tree "q(b, b)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(X, X)"; "q(b, X)"; "q(=X, =Y)"])
let test_generalization6 =
  OUnit.TestCase (generalization tree "q(X, b)" ["q(X, b)"; "q(X, Y)"; "q(=X, =Y)"])
let test_generalization7 =
  OUnit.TestCase (generalization tree "q(a, b)" ["q(X, Y)"; "q(X, b)"; "q(a, b)"; "q(=X, =Y)"])
let test_generalization8 =
  OUnit.TestCase (generalization tree "q(b, g(b))" ["q(X, Y)"; "q(b, g(b))"; "q(b, X)"; "q(=X, =Y)"])
let test_generalization9 =
  OUnit.TestCase (generalization tree "q(=X, =X)" ["q(X, Y)"; "q(X, X)"; "q(=X, =Y)"])

let tree =
  build_tree ["q(a, a)"; "q(b, b)"]
let test_generalization10 =
  OUnit.TestCase (generalization tree "q(b, b)" ["q(b, b)"])
let test_generalization11 =
  OUnit.TestCase (generalization tree "q(a, a)" ["q(a, a)"])
let test_generalization12 =
  OUnit.TestCase (generalization tree "q(X, X)" [])
let test_generalization13 =
  OUnit.TestCase (generalization tree "q(a, b)" [])


let tree =
  build_tree ["q(a, b)"; "q(b, a)"]
let test_generalization14 =
  OUnit.TestCase (generalization tree "q(a, b)" ["q(a, b)"])
let test_generalization15 =
  OUnit.TestCase (generalization tree "q(b, a)" ["q(b, a)"])
let test_generalization16 =
  OUnit.TestCase (generalization tree "q(a, a)" [])
let test_generalization17 =
  OUnit.TestCase (generalization tree "q(b, b)" [])
let test_generalization18 =
  OUnit.TestCase (generalization tree "q(X, Y)" [])


let tree =
  build_tree ["q(a, b)"; "q(b, a)"; "q(a, a)"; "q(b, b)"]
let test_generalization19 =
  OUnit.TestCase (generalization tree "q(a, b)" ["q(a, b)"])
let test_generalization20 =
  OUnit.TestCase (generalization tree "q(b, a)" ["q(b, a)"])
let test_generalization21 =
  OUnit.TestCase (generalization tree "q(a, a)" ["q(a, a)"])
let test_generalization22 =
  OUnit.TestCase (generalization tree "q(b, b)" ["q(b, b)"])
let test_generalization23 =
  OUnit.TestCase (generalization tree "q(X, X)" [])
let test_generalization24 =
  OUnit.TestCase (generalization tree "q(X, Y)" [])


let tree =
  build_tree ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"; "q(b, X)" ]
let test_generalization25 =
  OUnit.TestCase (generalization tree "q(a, a)" ["q(a, a)"; "q(X, X)"; "q(=U, =U)"])
let test_generalization26 =
  OUnit.TestCase (generalization tree "q(b, b)" ["q(b, b)"; "q(X, X)"; "q(b, X)"; "q(=U, =U)"])
let test_generalization27 =
  OUnit.TestCase (generalization tree "q(X, X)" ["q(X, X)"; "q(=U, =U)"])
let test_generalization28 =
  OUnit.TestCase (generalization tree "q(=V, =V)" ["q(=U, =U)"; "q(X, X)"])
let test_generalization29 =
  OUnit.TestCase (generalization tree "q(a, b)" [])
let test_generalization30 =
  OUnit.TestCase (generalization tree "q(X, Y)" [])




let tree =
  build_tree [
    "g(f(a, a))";
  ]
let test_generalization31 =
  OUnit.TestCase (generalization tree "g(f(a, a))" ["g(f(a, a))"])

let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
  ]
let test_generalization32 =
  OUnit.TestCase (generalization tree "g(f(a, a))" ["g(f(a, a))"])
let test_generalization33 =
  OUnit.TestCase (generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])


let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
  ]
let test_generalization34 =
  OUnit.TestCase (generalization tree "g(f(a, a))" ["g(f(a, a))"])
let test_generalization35 =
  OUnit.TestCase (generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])
let test_generalization36 =
  OUnit.TestCase (generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))"])


  
let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))";
  ]
let test_generalization37 =
  OUnit.TestCase (generalization tree "g(f(a, a))" ["g(f(a, a))"])
let test_generalization38 =
  OUnit.TestCase (generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])
let test_generalization39 =
  OUnit.TestCase (generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))"])
let test_generalization40 =
  OUnit.TestCase (generalization tree "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))" ["g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))"])


(*
let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
(*    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), a), a))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(X3, f(f(f(X3, X4), f(X5, X4)), X5))), f(a, a)))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(X3, f(f(f(X3, X4), f(X5, X4)), X5))), f(f(X6, f(f(f(X6, X7), f(X8, X7)), X8)), f(a, a))))";*)
  ]
let test_generalization31 =
  OUnit.TestCase (generalization tree "g(f(a, a))" [0])
let test_generalization32 =
  OUnit.TestCase (generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" [1])
let test_generalization33 =
  OUnit.TestCase (generalization tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" [2])
  *)

let suite_generalization =
  OUnit.TestLabel (
    ("generalization"),
    (OUnit.TestList [
       test_generalization1;
       test_generalization2;
       test_generalization3;
       test_generalization4;
       test_generalization5;
       test_generalization6;
       test_generalization7;
       test_generalization8;
       test_generalization9;
       test_generalization10;
       test_generalization11;
       test_generalization12;
       test_generalization13;
       test_generalization14;
       test_generalization15;
       test_generalization16;
       test_generalization17;
       test_generalization18;
       test_generalization19;
       test_generalization20;
       test_generalization21;
       test_generalization22;
       test_generalization23;
       test_generalization24;
       test_generalization25;
       test_generalization26;
       test_generalization27;
       test_generalization28;
       test_generalization29;
       test_generalization30;
       test_generalization31;
       test_generalization32;
       test_generalization33;
       test_generalization34;
       test_generalization35;
       test_generalization36;
       test_generalization37;
       test_generalization38;
       test_generalization39;
       test_generalization40;
     ]
    )
  )









(*** p_unifiable ***)
let p_unifiable ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.find_all_unifiable ~p_preserving:true tree term
    in
    let success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("p_unifiable:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected)
	)
	success_desired
	success;

    let node =
      TermSubstitutionTree.find_unifiable ~p_preserving:true tree term
    in
    let first_success =
      match node with
	| None ->
	    List.length expected = 0

	| Some term ->
	    List.exists (Unification.are_terms_variants term) expected
    in
      OUnit.assert_bool
	("p_unifiable first:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ (match node with None -> "None" | Some term -> Term.term_to_string term)
	 ^  " but should be in\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	first_success



let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_p_unifiable1 =
  OUnit.TestCase (p_unifiable tree "q(a, b)" ["q(X, Y)"])
let test_p_unifiable2 =
  OUnit.TestCase (p_unifiable tree "q(X, X)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_p_unifiable3 =
  OUnit.TestCase (p_unifiable tree "q(b, X)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_p_unifiable4 =
  OUnit.TestCase (p_unifiable tree "q(b, =X)" ["q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(=X, =X)"]
let test_p_unifiable5 =
  OUnit.TestCase (p_unifiable tree "q(a, b)" [])
let test_p_unifiable6 =
  OUnit.TestCase (p_unifiable tree "q(X, X)" ["q(b, b)"; "q(=X, =X)"])
let test_p_unifiable7 =
  OUnit.TestCase (p_unifiable tree "q(b, X)" ["q(b, b)"])
let test_p_unifiable8 =
  OUnit.TestCase (p_unifiable tree "q(b, =X)" [])
let test_p_unifiable9 =
  OUnit.TestCase (p_unifiable tree "q(=X, =X)" ["q(=X, =X)"])



let tree =
  build_tree ["q(a, b)"; "q(b, a)"]
let test_p_unifiable10 =
  OUnit.TestCase (p_unifiable tree "q(a, b)" ["q(a, b)"])
let test_p_unifiable11 =
  OUnit.TestCase (p_unifiable tree "q(a, a)" [])
let test_p_unifiable12 =
  OUnit.TestCase (p_unifiable tree "q(b, a)" ["q(b, a)"])
let test_p_unifiable13 =
  OUnit.TestCase (p_unifiable tree "q(X, a)" ["q(b, a)"])


let tree =
  build_tree ["q(a, =X)"; "q(b, =X)"; "q(=X, a)"; "q(X, b)"]
let test_p_unifiable14 =
  OUnit.TestCase (p_unifiable tree "q(a, =X)" ["q(a, =X)"])
let test_p_unifiable15 =
  OUnit.TestCase (p_unifiable tree "q(a, X)" ["q(a, =X)"; "q(X, b)"])
let test_p_unifiable16 =
  OUnit.TestCase (p_unifiable tree "q(=X, a)" ["q(=X, a)"])
let test_p_unifiable17 =
  OUnit.TestCase (p_unifiable tree "q(X, a)" ["q(=X, a)"])
let test_p_unifiable18 =
  OUnit.TestCase (p_unifiable tree "q(X, X)" ["q(X, b)"])
let test_p_unifiable19 =
  OUnit.TestCase (p_unifiable tree "q(b, a)" [])


let tree =
  build_tree ["p(=U)"; "p(g(=U))"; "p(g(g(g(=U))))"]
let test_p_unifiable20 =
  OUnit.TestCase (p_unifiable tree "p(g(g(=U)))" [])
let test_p_unifiable21 =
  OUnit.TestCase (p_unifiable tree "p(g(=U))" ["p(g(=U))"])
let test_p_unifiable22 =
  OUnit.TestCase (p_unifiable tree "p(g(g(g(=U))))" ["p(g(g(g(=U))))"])



let suite_p_unifiable =
  OUnit.TestLabel (
    ("p_unifiable"),
    (OUnit.TestList [
       test_p_unifiable1;
       test_p_unifiable2;
       test_p_unifiable3;
       test_p_unifiable4;
       test_p_unifiable5;
       test_p_unifiable6;
       test_p_unifiable7;
       test_p_unifiable8;
       test_p_unifiable9;
       test_p_unifiable10;
       test_p_unifiable11;
       test_p_unifiable12;
       test_p_unifiable13;
       test_p_unifiable14;
       test_p_unifiable15;
       test_p_unifiable16;
       test_p_unifiable17;
       test_p_unifiable18;
       test_p_unifiable19;
       test_p_unifiable20;
       test_p_unifiable21;
       test_p_unifiable22;
     ]
    )
  )










(*** unifiable ***)
let unifiable ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.find_all_unifiable ~p_preserving:false tree term
    in
    let success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("unifiable:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected)
	)
	success_desired
	success;

    let node =
      TermSubstitutionTree.find_unifiable ~p_preserving:false tree term
    in
    let first_success =
      match node with
	| None ->
	    List.length expected = 0

	| Some term ->
	    List.exists (Unification.are_terms_variants term) expected
    in
      OUnit.assert_bool
	("p_unifiable first:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ (match node with None -> "None" | Some term -> Term.term_to_string term)
	 ^  " but should be in\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	first_success


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_unifiable1 =
  OUnit.TestCase (unifiable tree "q(a, b)" ["q(X, Y)"])
let test_unifiable2 =
  OUnit.TestCase (unifiable tree "q(X, X)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_unifiable3 =
  OUnit.TestCase (unifiable tree "q(b, X)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_unifiable4 =
  OUnit.TestCase (unifiable tree "q(b, =X)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(=X, =X)"]
let test_unifiable5 =
  OUnit.TestCase (unifiable tree "q(a, b)" [])
let test_unifiable6 =
  OUnit.TestCase (unifiable tree "q(X, X)" ["q(b, b)"; "q(=X, =X)"])
let test_unifiable7 =
  OUnit.TestCase (unifiable tree "q(b, X)" ["q(b, b)"; "q(=X, =X)"])
let test_unifiable8 =
  OUnit.TestCase (unifiable tree "q(b, =X)" ["q(b, b)"; "q(=X, =X)"])
let test_unifiable9 =
  OUnit.TestCase (unifiable tree "q(=X, =X)" ["q(b, b)"; "q(=X, =X)"])



let tree =
  build_tree ["q(a, b)"; "q(b, a)"]
let test_unifiable10 =
  OUnit.TestCase (unifiable tree "q(a, b)" ["q(a, b)"])
let test_unifiable11 =
  OUnit.TestCase (unifiable tree "q(a, a)" [])
let test_unifiable12 =
  OUnit.TestCase (unifiable tree "q(b, a)" ["q(b, a)"])
let test_unifiable13 =
  OUnit.TestCase (unifiable tree "q(X, a)" ["q(b, a)"])


let tree =
  build_tree ["q(a, =X)"; "q(b, =X)"; "q(=X, a)"; "q(X, b)"]
let test_unifiable14 =
  OUnit.TestCase (unifiable tree "q(a, =X)" ["q(a, =X)"; "q(=X, a)"; "q(X, b)"])
let test_unifiable15 =
  OUnit.TestCase (unifiable tree "q(a, X)" ["q(a, =X)"; "q(=X, a)"; "q(X, b)"])
let test_unifiable16 =
  OUnit.TestCase (unifiable tree "q(=X, a)" ["q(a, =X)"; "q(b, =X)"; "q(=X, a)"])
let test_unifiable17 =
  OUnit.TestCase (unifiable tree "q(X, a)" ["q(a, =X)"; "q(b, =X)"; "q(=X, a)"])
let test_unifiable18 =
  OUnit.TestCase (unifiable tree "q(X, X)" ["q(a, =X)"; "q(b, =X)"; "q(=X, a)"; "q(X, b)"])
let test_unifiable19 =
  OUnit.TestCase (unifiable tree "q(b, a)" ["q(b, =X)"; "q(=X, a)"])


let tree =
  build_tree ["p(=U)"; "p(g(=U))"; "p(g(g(g(=U))))"]
let test_unifiable20 =
  OUnit.TestCase (unifiable tree "p(g(g(=U)))" ["p(=U)"; "p(g(=U))"; "p(g(g(g(=U))))"])
let test_unifiable21 =
  OUnit.TestCase (unifiable tree "p(g(=U))" ["p(=U)"; "p(g(=U))"; "p(g(g(g(=U))))"])
let test_unifiable22 =
  OUnit.TestCase (unifiable tree "p(g(g(g(=U))))" ["p(=U)"; "p(g(=U))"; "p(g(g(g(=U))))"])



let suite_unifiable =
  OUnit.TestLabel (
    ("unifiable"),
    (OUnit.TestList [
       test_unifiable1;
       test_unifiable2;
       test_unifiable3;
       test_unifiable4;
       test_unifiable5;
       test_unifiable6;
       test_unifiable7;
       test_unifiable8;
       test_unifiable9;
       test_unifiable10;
       test_unifiable11;
       test_unifiable12;
       test_unifiable13;
       test_unifiable14;
       test_unifiable15;
       test_unifiable16;
       test_unifiable17;
       test_unifiable18;
       test_unifiable19;
       test_unifiable20;
       test_unifiable21;
       test_unifiable22;
     ]
    )
  )













(*** p_instance ***)
let p_instance ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.find_all_instances ~p_preserving:true tree term
    in
    let success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("p_instance:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	success;

    let node =
      TermSubstitutionTree.find_instance ~p_preserving:true tree term
    in
    let first_success =
      match node with
	| None ->
	    List.length expected = 0

	| Some term ->
	    List.exists (Unification.are_terms_variants term) expected
    in
      OUnit.assert_bool
	("p_instance first:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ (match node with None -> "None" | Some term -> Term.term_to_string term)
	 ^  " but should be in\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	first_success



let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_p_instance1 =
  OUnit.TestCase (p_instance tree "q(X, X)" ["q(X, X)"; "q(b, b)"])
let test_p_instance2 =
  OUnit.TestCase (p_instance tree "q(b, b)" ["q(b, b)"])
let test_p_instance3 =
  OUnit.TestCase (p_instance tree "q(b, X)" ["q(b, b)"])
let test_p_instance4 =
  OUnit.TestCase (p_instance tree "q(X, Z)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_p_instance5 =
  OUnit.TestCase (p_instance tree "q(a, X)" [])


let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, X)"]
let test_p_instance6 =
  OUnit.TestCase (p_instance tree "q(b, b)" ["q(b, b)"])
let test_p_instance7 =
  OUnit.TestCase (p_instance tree "q(X, b)" ["q(X, b)"; "q(b, b)"; "q(a, b)"])
let test_p_instance8 =
  OUnit.TestCase (p_instance tree "q(b, X)" ["q(b, b)"; "q(b, g(b))"; "q(b, X)"])
let test_p_instance9 =
  OUnit.TestCase (p_instance tree "q(X, X)" ["q(b, b)"; "q(X, X)"])
let test_p_instance10 =
  OUnit.TestCase (p_instance tree "q(b, g(X))" ["q(b, g(b))"])


let tree =
  build_tree ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"; "q(b, X)"; "q(b, =U)"; "q(=U, =V)"]
let test_p_instance11 =
  OUnit.TestCase (p_instance tree "q(X, X)" ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"])
let test_p_instance12 =
  OUnit.TestCase (p_instance tree "q(=U, =U)" ["q(=U, =U)"])

let test_p_instance13 =
  OUnit.TestCase (p_instance tree "q(X, =U)" ["q(=U, =U)"; "q(b, =U)"; "q(=U, =V)"])
let test_p_instance14 =
  OUnit.TestCase (p_instance tree "q(=U, b)" [])
let test_p_instance15 =
  OUnit.TestCase (p_instance tree "q(b, =U)" ["q(b, =U)"])



let suite_p_instance =
  OUnit.TestLabel (
    ("p_instance"),
    (OUnit.TestList [
       test_p_instance1;
       test_p_instance2;
       test_p_instance3;
       test_p_instance4;
       test_p_instance5;
       test_p_instance6;
       test_p_instance7;
       test_p_instance8;
       test_p_instance9;
       test_p_instance10;
       test_p_instance11;
       test_p_instance12;
       test_p_instance13;
       test_p_instance14;
       test_p_instance15;
     ]
    )
  )






(*** instance ***)
let instance ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.find_all_instances ~p_preserving:false tree term
    in
    let success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("instance:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	success;

    let node =
      TermSubstitutionTree.find_instance ~p_preserving:false tree term
    in
    let first_success =
      match node with
	| None ->
	    List.length expected = 0

	| Some term ->
	    List.exists (Unification.are_terms_variants term) expected
    in
      OUnit.assert_bool
	("instance first:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ (match node with None -> "None" | Some term -> Term.term_to_string term)
	 ^  " but should be in\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	first_success



let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_instance1 =
  OUnit.TestCase (instance tree "q(X, X)" ["q(X, X)"; "q(b, b)"])
let test_instance2 =
  OUnit.TestCase (instance tree "q(b, b)" ["q(b, b)"])
let test_instance3 =
  OUnit.TestCase (instance tree "q(b, X)" ["q(b, b)"])
let test_instance4 =
  OUnit.TestCase (instance tree "q(X, Z)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_instance5 =
  OUnit.TestCase (instance tree "q(a, X)" [])


let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, X)"]
let test_instance6 =
  OUnit.TestCase (instance tree "q(b, b)" ["q(b, b)"])
let test_instance7 =
  OUnit.TestCase (instance tree "q(X, b)" ["q(X, b)"; "q(b, b)"; "q(a, b)"])
let test_instance8 =
  OUnit.TestCase (instance tree "q(b, X)" ["q(b, b)"; "q(b, g(b))"; "q(b, X)"])
let test_instance9 =
  OUnit.TestCase (instance tree "q(X, X)" ["q(b, b)"; "q(X, X)"])
let test_instance10 =
  OUnit.TestCase (instance tree "q(b, g(X))" ["q(b, g(b))"])


let tree =
  build_tree ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"; "q(b, X)"; "q(b, =U)"; "q(=U, =V)"]
let test_instance11 =
  OUnit.TestCase (instance tree "q(X, X)" ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"])
let test_instance12 =
  OUnit.TestCase (instance tree "q(=U, =U)" ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"])
let test_instance13 =
  OUnit.TestCase (instance tree "q(X, =U)" ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"; "q(b, X)"; "q(b, =U)"; "q(=U, =V)"])
let test_instance14 =
  OUnit.TestCase (instance tree "q(=U, b)" ["q(b, b)"])
let test_instance15 =
  OUnit.TestCase (instance tree "q(b, =U)" ["q(b, b)"; "q(b, X)"; "q(b, =U)"])



let suite_instance =
  OUnit.TestLabel (
    ("instance"),
    (OUnit.TestList [
       test_instance1;
       test_instance2;
       test_instance3;
       test_instance4;
       test_instance5;
       test_instance6;
       test_instance7;
       test_instance8;
       test_instance9;
       test_instance10;
       test_instance11;
       test_instance12;
       test_instance13;
       test_instance14;
       test_instance15;
     ]
    )
  )


(*






(*** renumber_term ***)
let renumber_term ?(success_desired:bool = true) (tree: substitution_tree) (term: term)
  (expected: term) =
  
  fun () ->
    let renumbered =
      TermSubstitutionTree.renumber_term tree term
    in
    let success =
      Term.term_equal renumbered expected
    in
      OUnit.assert_bool
	("renumber_term: " ^ Term.term_to_string term
	 ^ " is\n" ^ Term.term_to_string renumbered
	 ^ " but should be\n" ^ Term.term_to_string expected)
	success_desired
	success

let not_renumber_term (tree: substitution_tree) (term: term)
  (expected: term) =
  renumber_term ~success_desired:false tree term expected


let tree =
  TermSubstitutionTree.create p_a 0

let test_renumber_term1 =
  OUnit.TestCase (renumber_term tree q_01 q_12)

let test_renumber_term2 =
  OUnit.TestCase (renumber_term tree q_00 q_33)



let suite_renumber_term =
  OUnit.TestLabel (
    ("renumber_term"),
    (OUnit.TestList [
       test_renumber_term1;
       test_renumber_term2;
     ]
    )
  )







let build_tree (nodes: string list) : term substitution_tree =
  match nodes with
    | [] ->
	failwith "build_tree"

    | head :: tail ->
	List.fold_left
	(fun tree parse ->
	   let term =
	     Read_darwin.to_term parse
	   in
	     TermSubstitutionTree.insert tree term term Unification.are_terms_variants Term.term_to_string
	)
	(TermSubstitutionTree.create (Read_darwin.to_term head) (Read_darwin.to_term head))
	tail





(*** insert ***)
let insert ?(success_desired:bool = true) (tree: substitution_tree) (term: term) =
  
  fun () ->
    let inserted =
      TermSubstitutionTree.insert tree term term Unification.are_terms_variants Term.term_to_string
    in
      print_endline ("insert:\n"
		     ^ Term.term_to_string term
		     ^ "\ninto\n"
		     ^ TermSubstitutionTree.to_string tree
		     ^ "\nis\n"
		     ^ TermSubstitutionTree.to_string inserted)


let tree =
  build_tree ["q(b, b)"]
let test_insert1 =
  OUnit.TestCase (insert tree q_xx)
  


let tree =
  build_tree ["q(b, b)"; "q(X, X)"]
let test_insert2 =
  OUnit.TestCase (insert tree q_xy)


let tree =
  build_tree ["q(X, Y)"]
let test_insert3 =
  OUnit.TestCase (insert tree q_xx)
  

let tree =
  build_tree ["q(X, Y)"; "q(b, b)"]
let test_insert4 =
  OUnit.TestCase (insert tree q_xb)


let tree =
  build_tree ["q(X, Y)"]
let test_insert5 =
  OUnit.TestCase (insert tree q_bx)


let tree =
  build_tree ["q(X, Y)"; "q(b, X)"]
let test_insert6 =
  OUnit.TestCase (insert tree q_xx)


let tree =
  build_tree ["q(b, b)"; "q(b, X)"]
let test_insert7 =
  OUnit.TestCase (insert tree q_xy)



let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(=U, =V)"; "q(b, X)"]
let test_insert8 =
  OUnit.TestCase (insert tree q_uu)
  

let tree =
  build_tree ["q(a, a)"]
let test_insert10 =
  OUnit.TestCase (insert tree q_bb)


let tree =
  build_tree ["q(X, Y)"; "q(=U, =V)"]
let test_insert11 =
  OUnit.TestCase (insert tree q_uu)


let tree =
  build_tree ["w(X, X, a)"; "w(a, X, X)"]
let test_insert9 =
  OUnit.TestCase (insert tree w_axa)


let tree =
  build_tree ["w(X, Y, Z)"; "w(X, X, Y)"]
let test_insert12 =
  OUnit.TestCase (insert tree w_xxa)


let suite_insert =
  OUnit.TestLabel (
    ("insert"),
    (OUnit.TestList [
       test_insert1; 
       test_insert2;
       test_insert3;
       test_insert4; 
       test_insert5; 
       test_insert6;
       test_insert7;
       test_insert8;
       test_insert9;
       test_insert10;
       test_insert11;
       test_insert12;
     ]
    )
  )











(*** subsumes ***)
let subsumes ?(success_desired:bool = true) (tree: term substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.subsumes tree term
    in
    let success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("subsumes:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected))
	success_desired
	success


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_subsumes1 =
  OUnit.TestCase (subsumes tree "q(X, X)" ["q(X, X)"; "q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_subsumes2 =
  OUnit.TestCase (subsumes tree "q(X, Y)" ["q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_subsumes3 =
  OUnit.TestCase (subsumes tree "q(b, b)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(X, X)"]
let test_subsumes4 =
  OUnit.TestCase (subsumes tree "q(X, Y)" [])


let tree =
  build_tree ["q(b, b)"; "q(X, b)"; "q(a, b)"; "q(X, Y)"; "q(X, X)"; "q(b, g(b))"; "q(b, X)"]
let test_subsumes5 =
  OUnit.TestCase (subsumes tree "q(b, b)" ["q(b, b)"; "q(X, b)"; "q(X, Y)"; "q(X, X)"; "q(b, X)"])
let test_subsumes6 =
  OUnit.TestCase (subsumes tree "q(X, b)" ["q(X, b)"; "q(X, Y)"])
let test_subsumes7 =
  OUnit.TestCase (subsumes tree "q(a, b)" ["q(X, Y)"; "q(X, b)"; "q(a, b)"])
let test_subsumes8 =
  OUnit.TestCase (subsumes tree "q(b, g(b))" ["q(X, Y)"; "q(b, g(b))"; "q(b, X)"])

let tree =
  build_tree ["q(a, a)"; "q(b, b)"]
let test_subsumes9 =
  OUnit.TestCase (subsumes tree "q(b, b)" ["q(b, b)"])
let test_subsumes10 =
  OUnit.TestCase (subsumes tree "q(a, a)" ["q(a, a)"])
let test_subsumes11 =
  OUnit.TestCase (subsumes tree "q(X, X)" [])
let test_subsumes12 =
  OUnit.TestCase (subsumes tree "q(a, b)" [])


let tree =
  build_tree ["q(a, b)"; "q(b, a)"]
let test_subsumes13 =
  OUnit.TestCase (subsumes tree "q(a, b)" ["q(a, b)"])
let test_subsumes14 =
  OUnit.TestCase (subsumes tree "q(b, a)" ["q(b, a)"])
let test_subsumes15 =
  OUnit.TestCase (subsumes tree "q(a, a)" [])
let test_subsumes16 =
  OUnit.TestCase (subsumes tree "q(b, b)" [])
let test_subsumes17 =
  OUnit.TestCase (subsumes tree "q(X, Y)" [])


let tree =
  build_tree ["q(a, b)"; "q(b, a)"; "q(a, a)"; "q(b, b)"]
let test_subsumes18 =
  OUnit.TestCase (subsumes tree "q(a, b)" ["q(a, b)"])
let test_subsumes19 =
  OUnit.TestCase (subsumes tree "q(b, a)" ["q(b, a)"])
let test_subsumes20 =
  OUnit.TestCase (subsumes tree "q(a, a)" ["q(a, a)"])
let test_subsumes21 =
  OUnit.TestCase (subsumes tree "q(b, b)" ["q(b, b)"])
let test_subsumes22 =
  OUnit.TestCase (subsumes tree "q(X, X)" [])
let test_subsumes23 =
  OUnit.TestCase (subsumes tree "q(X, Y)" [])


let tree =
  build_tree ["q(=U, =U)"; "q(a, a)"; "q(b, b)"; "q(X, X)"; "q(b, X)" ]
let test_subsumes24 =
  OUnit.TestCase (subsumes tree "q(a, a)" ["q(a, a)"; "q(X, X)"])
let test_subsumes25 =
  OUnit.TestCase (subsumes tree "q(b, b)" ["q(b, b)"; "q(X, X)"; "q(b, X)"])
let test_subsumes26 =
  OUnit.TestCase (subsumes tree "q(X, X)" ["q(X, X)"])
let test_subsumes27 =
  OUnit.TestCase (subsumes tree "q(=V, =V)" ["q(=U, =U)"; "q(X, X)"])
let test_subsumes28 =
  OUnit.TestCase (subsumes tree "q(a, b)" [])
let test_subsumes29 =
  OUnit.TestCase (subsumes tree "q(X, Y)" [])




let tree =
  build_tree [
    "g(f(a, a))";
  ]
let test_subsumes30 =
  OUnit.TestCase (subsumes tree "g(f(a, a))" ["g(f(a, a))"])

let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
  ]
let test_subsumes31 =
  OUnit.TestCase (subsumes tree "g(f(a, a))" ["g(f(a, a))"])
let test_subsumes32 =
  OUnit.TestCase (subsumes tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])


let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
  ]
let test_subsumes33 =
  OUnit.TestCase (subsumes tree "g(f(a, a))" ["g(f(a, a))"])
let test_subsumes34 =
  OUnit.TestCase (subsumes tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])
let test_subsumes35 =
  OUnit.TestCase (subsumes tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))"])


  
let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))";
  ]
let test_subsumes36 =
  OUnit.TestCase (subsumes tree "g(f(a, a))" ["g(f(a, a))"])
let test_subsumes37 =
  OUnit.TestCase (subsumes tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))"])
let test_subsumes38 =
  OUnit.TestCase (subsumes tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" ["g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))"])
let test_subsumes39 =
  OUnit.TestCase (subsumes tree "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))" ["g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))"])


(*
let tree =
  build_tree [
    "g(f(a, a))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))";
    "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))";
(*    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), X3), f(f(a, a), X3)))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), a), a))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(X3, f(f(f(X3, X4), f(X5, X4)), X5))), f(a, a)))";
    "g(f(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(X3, f(f(f(X3, X4), f(X5, X4)), X5))), f(f(X6, f(f(f(X6, X7), f(X8, X7)), X8)), f(a, a))))";*)
  ]
let test_subsumes31 =
  OUnit.TestCase (subsumes tree "g(f(a, a))" [0])
let test_subsumes32 =
  OUnit.TestCase (subsumes tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(a, a)))" [1])
let test_subsumes33 =
  OUnit.TestCase (subsumes tree "g(f(f(X0, f(f(f(X0, X1), f(X2, X1)), X2)), f(f(X3, f(f(f(X3, X4), f(X5, X4)), X5)), f(a, a))))" [2])
  *)

let suite_subsumes =
  OUnit.TestLabel (
    ("subsumes"),
    (OUnit.TestList [
       test_subsumes1;
       test_subsumes2;
       test_subsumes3;
       test_subsumes4;
       test_subsumes5;
       test_subsumes6;
       test_subsumes7;
       test_subsumes8;
       test_subsumes9;
       test_subsumes10;
       test_subsumes11;
       test_subsumes12;
       test_subsumes13;
       test_subsumes14;
       test_subsumes15;
       test_subsumes16;
       test_subsumes17;
       test_subsumes18;
       test_subsumes19;
       test_subsumes20;
       test_subsumes21;
       test_subsumes22;
       test_subsumes23;
       test_subsumes24;
       test_subsumes25;
       test_subsumes26;
       test_subsumes27;
       test_subsumes28;
       test_subsumes29;
       test_subsumes30;
       test_subsumes31;
       test_subsumes32;
       test_subsumes33;
       test_subsumes34;
       test_subsumes35;
       test_subsumes36;
       test_subsumes37;
       test_subsumes38;
       test_subsumes39;
     ]
    )
  )










(*** p_unifies ***)
let p_unifies ?(success_desired:bool = true) (tree: substitution_tree) (parse: string) (expected: string list) =
  fun () ->
    let term =
      Read_darwin.to_term parse
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.p_unifies tree term
    in
    let success =
      Tools.lists_unordered_equal Unification.are_terms_variants nodes expected
    in
      OUnit.assert_bool
	("p_unifies:\n"
	 ^ TermSubstitutionTree.to_string tree
	 ^ " and\n" ^ Term.term_to_string term
	 ^  " is\n " ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^  " but should be\n " ^ String.concat ", " (List.map Term.term_to_string expected)
	)
	success_desired
	success


let tree =
  build_tree ["q(b, b)"; "q(X, X)"; "q(X, Y)"]
let test_p_unifies1 =
  OUnit.TestCase (p_unifies tree "q(a, b)" ["q(X, Y)"])
let test_p_unifies2 =
  OUnit.TestCase (p_unifies tree "q(X, X)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_p_unifies3 =
  OUnit.TestCase (p_unifies tree "q(b, X)" ["q(b, b)"; "q(X, X)"; "q(X, Y)"])
let test_p_unifies4 =
  OUnit.TestCase (p_unifies tree "q(b, =X)" ["q(X, Y)"])


let tree =
  build_tree ["q(b, b)"; "q(=X, =X)"]
let test_p_unifies5 =
  OUnit.TestCase (p_unifies tree "q(a, b)" [])
let test_p_unifies6 =
  OUnit.TestCase (p_unifies tree "q(X, X)" ["q(b, b)"; "q(=X, =X)"])
let test_p_unifies7 =
  OUnit.TestCase (p_unifies tree "q(b, X)" ["q(b, b)"])
let test_p_unifies8 =
  OUnit.TestCase (p_unifies tree "q(b, =X)" [])
let test_p_unifies9 =
  OUnit.TestCase (p_unifies tree "q(=X, =X)" ["q(=X, =X)"])



let tree =
  build_tree ["q(a, b)"; "q(b, a)"]
let test_p_unifies10 =
  OUnit.TestCase (p_unifies tree "q(a, b)" ["q(a, b)"])
let test_p_unifies11 =
  OUnit.TestCase (p_unifies tree "q(a, a)" [])
let test_p_unifies12 =
  OUnit.TestCase (p_unifies tree "q(b, a)" ["q(b, a)"])
let test_p_unifies13 =
  OUnit.TestCase (p_unifies tree "q(X, a)" ["q(b, a)"])


let tree =
  build_tree ["q(a, =X)"; "q(b, =X)"; "q(=X, a)"]
let test_p_unifies14 =
  OUnit.TestCase (p_unifies tree "q(a, =X)" ["q(a, =X)"])
let test_p_unifies15 =
  OUnit.TestCase (p_unifies tree "q(a, X)" ["q(a, =X)"])
let test_p_unifies16 =
  OUnit.TestCase (p_unifies tree "q(=X, a)" ["q(=X, a)"])
let test_p_unifies17 =
  OUnit.TestCase (p_unifies tree "q(X, a)" ["q(=X, a)"])
let test_p_unifies18 =
  OUnit.TestCase (p_unifies tree "q(X, X)" [])
let test_p_unifies19 =
  OUnit.TestCase (p_unifies tree "q(b, a)" [])


let tree =
  build_tree ["p(=U)"; "p(g(=U))"; "p(g(g(g(=U))))"]
let test_p_unifies20 =
  OUnit.TestCase (p_unifies tree "p(g(g(=U)))" [])
let test_p_unifies21 =
  OUnit.TestCase (p_unifies tree "p(g(=U))" ["p(g(=U))"])
let test_p_unifies22 =
  OUnit.TestCase (p_unifies tree "p(g(g(g(=U))))" ["p(g(g(g(=U))))"])



let suite_p_unifies =
  OUnit.TestLabel (
    ("p_unifies"),
    (OUnit.TestList [
       test_p_unifies1;
       test_p_unifies2;
       test_p_unifies3;
       test_p_unifies4;
       test_p_unifies5;
       test_p_unifies6;
       test_p_unifies7;
       test_p_unifies8;
       test_p_unifies9;
       test_p_unifies10;
       test_p_unifies11;
       test_p_unifies12;
       test_p_unifies13;
       test_p_unifies14;
       test_p_unifies15;
       test_p_unifies16;
       test_p_unifies17;
       test_p_unifies18;
       test_p_unifies19;
       test_p_unifies20;
       test_p_unifies21;
       test_p_unifies22;
     ]
    )
  )






(*** is_msg ***)
let is_msg ?(success_desired:bool = true) (tree: substitution_tree) (parse: (string * string)) (expected: string list) =
  fun () ->
    let term1, term2 =
      Read_darwin.to_term (fst parse),
      Read_darwin.to_term (snd parse)
    in
    let expected =
      List.map Read_darwin.to_term expected
    in
    let nodes =
      TermSubstitutionTree.is_msg tree term1 term2
    in
    let success =
      Tools.lists_unordered_equal (=) nodes expected
    in
      OUnit.assert_bool
	("is_msg:\n"
		^ TermSubstitutionTree.to_string tree
	 ^ " msg\n" ^ Term.term_to_string term1
	 ^ " of\n" ^ Term.term_to_string term2
	 ^ " is\n" ^ String.concat ", " (List.map Term.term_to_string nodes)
	 ^ " but should be\n" ^ String.concat ", " (List.map Term.term_to_string expected)
	)
	success_desired
	success



let tree =
  build_tree ["q(a, a)"]
let test_is_msg1 =
  OUnit.TestCase (is_msg tree ("q(X, X)", "q(a, a)") [])
let test_is_msg2 =
  OUnit.TestCase (is_msg tree ("q(a, a)", "q(a, a)") [])


let tree =
  build_tree ["q(a, a)"; "q(X, a)"; "q(X, X)"]
let test_is_msg3 =
  OUnit.TestCase (is_msg tree ("q(X, X)", "q(a, a)") [])
let test_is_msg4 =
  OUnit.TestCase (is_msg tree ("q(a, a)", "q(a, a)") [])
let test_is_msg5 =
  OUnit.TestCase (is_msg tree ("q(X, Y)", "q(a, a)") ["q(X, a)"; "q(X, X)"])
let test_is_msg6 =
  OUnit.TestCase (is_msg tree ("q(X, a)", "q(a, a)") [])
let test_is_msg7 =
  OUnit.TestCase (is_msg tree ("q(=X, a)", "q(a, a)") ["q(X, a)"])


let tree =
  build_tree ["q(a, a)"; "q(=X, a)"; "q(=X, =X)"]
let test_is_msg8 =
  OUnit.TestCase (is_msg tree ("q(X, X)", "q(a, a)") ["q(=X, =X)"])
let test_is_msg9 =
  OUnit.TestCase (is_msg tree ("q(a, a)", "q(a, a)") [])
let test_is_msg10 =
  OUnit.TestCase (is_msg tree ("q(=X, a)", "q(a, a)") [])
let test_is_msg11 =
  OUnit.TestCase (is_msg tree ("q(X, a)", "q(a, a)") ["q(=X, a)"])


let tree =
  build_tree ["q(a, a)"; "q(a, b)"; "q(X, X)"]
let test_is_msg12 =
  OUnit.TestCase (is_msg tree ("q(X, X)", "q(a, a)") [])
let test_is_msg13 =
  OUnit.TestCase (is_msg tree ("q(X, a)", "q(a, a)") [])
let test_is_msg14 =
  OUnit.TestCase (is_msg tree ("q(a, X)", "q(a, a)") [])



let tree =
  build_tree ["q(a, a)"; "q(=X, a)"; "q(X, a)"; "q(=X, =X)"; "q(=X, =Y)"]
let test_is_msg15 =
  OUnit.TestCase (is_msg tree ("q(X, a)", "q(a, a)") ["q(=X, a)"])
let test_is_msg16 =
  OUnit.TestCase (is_msg tree ("q(=X, a)", "q(a, a)") ["q(X, a)"])
let test_is_msg17 =
  OUnit.TestCase (is_msg tree ("q(X, X)", "q(a, a)") ["q(=X, =X)"])
let test_is_msg18 =
  OUnit.TestCase (is_msg tree ("q(=X, =X)", "q(a, a)") [])
let test_is_msg19 =
  OUnit.TestCase (is_msg tree ("q(=X, =Y)", "q(a, a)") ["q(=X, a)"; "q(X, a)"; "q(=X, =X)"])
let test_is_msg20 =
  OUnit.TestCase (is_msg tree ("q(X, Y)", "q(a, a)") ["q(=X, a)"; "q(X, a)"; "q(=X, =X)"; "q(=X, =Y)"])



let suite_is_msg =
  OUnit.TestLabel (
    ("is_msg"),
    (OUnit.TestList [
       test_is_msg1;
       test_is_msg2;
       test_is_msg3;
       test_is_msg4;
       test_is_msg5;
       test_is_msg6;
       test_is_msg7;
       test_is_msg8;
       test_is_msg9;
       test_is_msg10;
       test_is_msg11;
       test_is_msg12;
       test_is_msg13;
       test_is_msg14;
       test_is_msg15;
       test_is_msg16;
       test_is_msg17;
       test_is_msg18;
       test_is_msg19;
       test_is_msg20;
     ]
    )
  )











(*** mscg_of_terms ***)
let mscg_of_terms ?(success_desired:bool = true) (tree: substitution_tree) (parse1: string) (parse2: string)
  (expected: string) =
  fun () ->
    let term1 =
      TermSubstitutionTree.renumber_term tree (Read_darwin.to_term parse1)
    and term2 =
      TermSubstitutionTree.renumber_term tree (Read_darwin.to_term parse2)
    and expected =
      Read_darwin.to_term expected
    in
    let mscg, subst1, subst2 =
      TermSubstitutionTree.mscg_of_terms tree term1 term2 []
    in
    let applied1 =
      Subst.do_apply_to_term_no_normalization subst1 mscg 0 0
    and applied2 =
      Subst.do_apply_to_term_no_normalization subst2 mscg 0 0
    in
    let success =
      (Unification.are_terms_variants mscg expected)
      &&
      (Term.term_equal term1 applied1)
      &&
      (Term.term_equal term2 applied2)
    in
      OUnit.assert_bool
	("mscg_of_terms:\n"
	 ^ Term.term_to_string term1 ^ " and\n"
	 ^ Term.term_to_string term2 ^ " is\n"
	 ^ Term.term_to_string mscg ^ "\n"
	 ^ "( expected: " ^ Term.term_to_string expected ^ " )\n"
	 ^ Subst.subst_to_string subst1 ^ "\n"
	 ^ "===> " ^ Term.term_to_string applied1 ^ "\n"
	 ^ Subst.subst_to_string subst2 ^ "\n"
	 ^ "===> " ^ Term.term_to_string applied2
	)
	success_desired
	success


let not_mscg_of_terms (tree: substitution_tree) (parse1: string) (parse2: string) (expected: string) =
  mscg_of_terms ~success_desired:false tree parse1 parse2 expected


let tree =
  TermSubstitutionTree.create (Read_darwin.to_term "a") 0


let test_mscg_of_terms1 =
  OUnit.TestCase (mscg_of_terms tree "q(a, b)" "q(a, c)" "q(a, X)")

let test_mscg_of_terms2 =
  OUnit.TestCase (mscg_of_terms tree "q(X, b)" "q(a, b)" "q(X, b)")

let test_mscg_of_terms3 =
  OUnit.TestCase (mscg_of_terms tree "q(a, b)" "q(X, b)" "q(X, b)")

let test_mscg_of_terms4 =
  OUnit.TestCase (mscg_of_terms tree "q(X, X)" "q(a, a)" "q(X, X)")

let test_mscg_of_terms5 =
  OUnit.TestCase (mscg_of_terms tree "q(X, X)" "q(Y, g(Y))" "q(X, Z)")


let test_mscg_of_terms6 =
  OUnit.TestCase (mscg_of_terms tree "q(X, X)" "q(a, Y)" "q(X, Y)")

let test_mscg_of_terms7 =
  OUnit.TestCase (mscg_of_terms tree "q(a, a)" "q(b, b)" "q(X, X)")

let test_mscg_of_terms8 =
  OUnit.TestCase (not_mscg_of_terms tree "q(a, a)" "q(b, b)" "q(X, Y)")

let test_mscg_of_terms9 =
  OUnit.TestCase (mscg_of_terms tree "w(a, g(X), f(X, g(a)))" "w(b, g(b), f(b, g(a)))" "w(Z, g(X), f(X, g(a)))")

let suite_mscg_of_terms =
  OUnit.TestLabel (
    ("mscg_of_terms"),
    (OUnit.TestList [
       test_mscg_of_terms1;
       test_mscg_of_terms2;
       test_mscg_of_terms3;
       test_mscg_of_terms4;
       test_mscg_of_terms5;
       test_mscg_of_terms6;
       test_mscg_of_terms7;
       test_mscg_of_terms8;
       test_mscg_of_terms9;
     ]
    )
  )







(*
(*** mscg_of_substs ***)
let mscg_of_substs ?(success_desired:bool = true) (tree: substitution_tree) (subst1: subst) (subst2: subst) =
  fun () ->
    let mscg, mscg_subst1, mscg_subst2 =
      TermSubstitutionTree.mscg_of_substs tree subst1 subst2 []
    in
      print_endline
	("mscg_of_substs:\n"
	 ^ Subst.subst_to_string subst1 ^ "\n"
	 ^ "and\n"
	 ^ Subst.subst_to_string subst2 ^ "\n"
	 ^ "is\n"
	 ^ Subst.subst_to_string mscg ^ "\n"
	 ^ Subst.subst_to_string mscg_subst1 ^ "\n"
	 ^ Subst.subst_to_string mscg_subst2
	)

(*
let not_mscg_of_substs (tree: substitution_tree) (parse1: string) (parse2: string) (expected: string) =
  mscg_of_substs ~success_desired:false tree parse1 parse2 expected
*)

let tree =
  TermSubstitutionTree.create (Read_darwin.to_term "a") 0

let subst1 =
  Subst.empty
let subst2 =
  Subst.empty
let test_mscg_of_substs1 =
  OUnit.TestCase (mscg_of_substs tree subst1 subst2)


let subst1 =
  Subst.set Subst.empty var0 0 b 0
let subst2 =
  Subst.empty
let test_mscg_of_substs2 =
  OUnit.TestCase (mscg_of_substs tree subst1 subst2)


let suite_mscg_of_substs =
  OUnit.TestLabel (
    ("mscg_of_substs"),
    (OUnit.TestList [
       test_mscg_of_substs1;
       test_mscg_of_substs2;
     ]
    )
  )

*)

*)

(*** run tests ***)
let test_suite =
  OUnit.TestList [
(*    suite_renumber_term;*)
    suite_insert;
    suite_remove_variant;
    suite_p_generalization;
    suite_generalization;
    suite_p_unifiable;
    suite_unifiable;
    suite_p_instance;
    suite_instance;
(*    suite_is_msg;
    suite_mscg_of_terms;
    suite_mscg_of_substs;*)
  ]

let _ =
  OUnit.run_test_tt_main test_suite
