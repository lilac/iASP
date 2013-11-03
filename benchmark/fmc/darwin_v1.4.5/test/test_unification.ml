(* $Id: test_unification.ml,v 1.4 2004/04/13 15:07:46 alexf Exp $ *)

type var = Var.var
type term = Term.term
type literal = Term.literal
type subst = Subst.subst



(*** data setup ***)
let var0 = Var.create_universal 0
let var1 = Var.create_universal 1
let var2 = Var.create_universal 2
let var3 = Var.create_universal 3
let var4 = Var.create_universal 4
let par0 = Var.create_parametric 0
let par1 = Var.create_parametric 1
let par2 = Var.create_parametric 2
let vari0 = Var.create_universal_indicator 0
let vari1 = Var.create_universal_indicator 1
let pari0 = Var.create_parametric_indicator 0
let pari1 = Var.create_parametric_indicator 1

let v_0 = Term.request_var var0
let v_1 = Term.request_var var1
let v_2 = Term.request_var var2
let v_3 = Term.request_var var3
let p_0 = Term.request_var par0
let p_1 = Term.request_var par1
let p_2 = Term.request_var par2
let vi_0 = Term.request_var vari0
let vi_1 = Term.request_var vari1
let pi_0 = Term.request_var pari0
let pi_1 = Term.request_var pari1

let a = Read_darwin.to_term "a"
let b = Read_darwin.to_term "b"
let f_xa = Read_darwin.to_term "f(X, a)"
let f_xx = Read_darwin.to_term "f(X, X)"
let f_xy =
  Term.request_func (
    Symbol.create_symbol "f" 2,
    [| Term.request_var var0; Term.request_var var1 |]
  )
let f_yx =
  Term.request_func (
    Symbol.create_symbol "f" 2,
    [| Term.request_var var1; Term.request_var var0 |]
  )
let f_vv = Read_darwin.to_term "f(=V, =V)"
let f_uu = Read_darwin.to_term "f(=U, =U)"
let f_uv = Read_darwin.to_term "f(=U, =V)"
let f_ux = Read_darwin.to_term "f(=U, X)"
let f_g_x_x = Read_darwin.to_term "f(g(X), X)"
let f_g_x_g_a = Read_darwin.to_term "f(g(X), g(a))"
let f_g_u_g_v = Read_darwin.to_term "f(g(=U), g(=V))"
let f_g_x_g_g_a = Read_darwin.to_term "f(g(X), g(g(a)))"
let f_x_g_x = Read_darwin.to_term "f(X, g(X))"
let g_a = Read_darwin.to_term "g(a)"
let g_x =
  Term.request_func (
    Symbol.create_symbol "g" 1,
    [| Term.request_var var0 |]
  )
let g_y =
  Term.request_func (
    Symbol.create_symbol "g" 1,
    [| Term.request_var var1 |]
  )
let g_z =
  Term.request_func (
    Symbol.create_symbol "g" 1,
    [| Term.request_var var2 |]
  )
let g_g_x = Read_darwin.to_term "g(g(X))"
let g_u = Read_darwin.to_term "g(=U)"
let g_v = 
  Term.request_func (
    Symbol.create_symbol "g" 1,
    [| Term.request_var (Var.create_parametric 1) |]
  )
let g_w = 
  Term.request_func (
    Symbol.create_symbol "g" 1,
    [| Term.request_var (Var.create_parametric 2) |]
  )

let j_xab = Read_darwin.to_term "j(X, a, b)"
let j_vab = Read_darwin.to_term "j(=V, a, b)"
let q_yz =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 1); Term.request_var (Var.create_universal 2) |]
  )
let q_xx =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 0); Term.request_var (Var.create_universal 0) |]
  )
let q_12 =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 1); Term.request_var (Var.create_universal 2) |]
  )
let q_23 =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 2); Term.request_var (Var.create_universal 3) |]
  )
let q_3b =
  Term.request_func (
    Symbol.create_symbol "q" 2,
    [| Term.request_var (Var.create_universal 3); Term.request_const (Symbol.create_symbol "b" 0) |]
  )
let q_bb = Read_darwin.to_term "q(b, b)"
let q_uu = Read_darwin.to_term "q(=U, =U)"

let g_xi =
  Term.request_func (
    Symbol.create_symbol "g" 1,
    [| vi_0 |]
  )
let g_ui =
  Term.request_func (
    Symbol.create_symbol "g" 1,
    [| pi_0 |]
  )
let f_xiyi =
  Term.request_func (
    Symbol.create_symbol "f" 2,
    [| vi_0; vi_1 |]
  )
let f_xiy =
  Term.request_func (
    Symbol.create_symbol "f" 2,
    [| vi_0; v_1 |]
  )
let f_xiu =
  Term.request_func (
    Symbol.create_symbol "f" 2,
    [| vi_0; p_1 |]
  )


let l_g_a = Read_darwin.to_literal "g(a)"
let l_not_g_a = Read_darwin.to_literal "-g(a)"
let l_g_x = Read_darwin.to_literal "g(X)"
let l_not_g_x = Read_darwin.to_literal "-g(X)"
let l_g_y =
  Term.request_literal true g_y
let l_g_v =
  Term.request_literal true g_v
let l_f_xy =
  Term.request_literal true f_xy
let l_f_yx =
  Term.request_literal true f_yx
let l_not_f_xy =
  Term.request_literal false f_xy
let l_not_f_yx =
  Term.request_literal false f_yx
let l_f_xx =
  Term.request_literal true f_xx
let l_f_uv =
  Term.request_literal true f_uv
let l_f_vv =
  Term.request_literal true f_vv











(*** are_terms_variants ***)
let are_terms_variants ?(success_desired:bool = true)
  (term1: term) (subst1: subst) (offset1: int)
  (term2: term) (subst2: subst) (offset2: int) =

  fun () ->
    let term1_instance =
      Subst.apply_to_term subst1 term1 offset1
    and term2_instance =
      Subst.apply_to_term subst2 term2 offset2
    in

      OUnit.assert_bool
	("are_terms_variants:\n"
	 ^ Term.term_to_string term1_instance
	 ^ "\nand\n"
	 ^ Term.term_to_string term2_instance)
	(Unification.are_terms_variants term1_instance term2_instance)
	success_desired(*;

      OUnit.assert_bool
	("are_terms_variants_lsubst:\n"
	 ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.term_to_string (Subst.make_term term1 offset1)
	 ^ "\nand\n"
	 ^ Term.term_to_string term2_instance)
	(Unification.are_terms_variants_lsubst term1 subst1 offset1 term2_instance)
	success_desired;

      OUnit.assert_bool
	("are_terms_variants_rsubst:\n"
	 ^ Term.term_to_string term1_instance
	 ^ "\nand\n"
	 ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.term_to_string (Subst.make_term term2 offset2))
	(Unification.are_terms_variants_rsubst term1_instance term2 subst2 offset2)
	success_desired;

      OUnit.assert_bool
	("are_terms_variants_substs:\n"
	 ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
	 ^ "\nand\n"
	 ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2))
	(Unification.are_terms_variants_substs term1 subst1 offset1 term2 subst2 offset2)
	success_desired*)


let not_are_terms_variants =
  are_terms_variants ~success_desired:false


let test_are_terms_variants1 =
  OUnit.TestCase (are_terms_variants g_a Subst.empty 0 g_a Subst.empty 0)

let test_are_terms_variants2 =
  OUnit.TestCase (are_terms_variants g_x Subst.empty 0 g_y Subst.empty 0)

let test_are_terms_variants3 =
  OUnit.TestCase (not_are_terms_variants g_a Subst.empty 0 g_x Subst.empty 0)

let test_are_terms_variants4 =
  OUnit.TestCase (not_are_terms_variants g_x Subst.empty 0 g_v Subst.empty 0)

let test_are_terms_variants5 =
  OUnit.TestCase (are_terms_variants f_xy Subst.empty 0 f_xy Subst.empty 0)

let test_are_terms_variants6 =
  OUnit.TestCase (are_terms_variants f_xy Subst.empty 0 f_yx Subst.empty 0)

let test_are_terms_variants7 =
  OUnit.TestCase (are_terms_variants f_xy Subst.empty 0 f_yx Subst.empty 1)

let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let test_are_terms_variants8 =
  OUnit.TestCase (are_terms_variants f_xy subst1 0 f_xx Subst.empty 0)


let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let test_are_terms_variants9 =
  OUnit.TestCase (are_terms_variants f_xy subst1 0 f_xy subst2 0)

let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 v_0 0
let test_are_terms_variants10 =
  OUnit.TestCase (are_terms_variants f_xy subst1 0 f_yx subst2 0)


let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 1)
    var1 0 v_1 1
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 v_0 0
let test_are_terms_variants11 =
  OUnit.TestCase (are_terms_variants f_xy subst1 0 f_yx subst2 0)


let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 1
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 v_0 0
let test_are_terms_variants12 =
  OUnit.TestCase (not_are_terms_variants f_xy subst1 0 f_yx subst2 0)


let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 v_0 0
let test_are_terms_variants13 =
  OUnit.TestCase (not_are_terms_variants f_xy subst1 0 f_yx subst2 1)



let suite_are_terms_variants =
  OUnit.TestLabel (
    ("are_terms_variants"),
    (OUnit.TestList [
       test_are_terms_variants1;
       test_are_terms_variants2;
       test_are_terms_variants3;
       test_are_terms_variants4;
       test_are_terms_variants5;
       test_are_terms_variants6;
       test_are_terms_variants7;
       test_are_terms_variants8;
       test_are_terms_variants9;
       test_are_terms_variants10;
       test_are_terms_variants11;
       test_are_terms_variants12;
       test_are_terms_variants13;
     ]
    )
  )














(*** are_literals_variants ***)
let are_literals_variants ?(success_desired:bool = true)
  (literal1: literal) (subst1: subst) (offset1: int)
  (literal2: literal) (subst2: subst) (offset2: int) =

  fun () ->
    let literal1_instance =
      Subst.apply_to_literal subst1 literal1 offset1
    and literal2_instance =
      Subst.apply_to_literal subst2 literal2 offset2
    in

      OUnit.assert_bool
	("are_literals_variants:\n"
	 ^ Term.literal_to_string literal1_instance
	 ^ "\nand\n"
	 ^ Term.literal_to_string literal2_instance)
	(Unification.are_literals_variants literal1_instance literal2_instance)
	success_desired(*;

      OUnit.assert_bool
	("are_literals_variants_lsubst:\n"
	 ^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	 ^ "\nand\n"
	 ^ Term.literal_to_string literal2_instance)
	(Unification.are_literals_variants_lsubst literal1 subst1 offset1 literal2_instance)
	success_desired;

      OUnit.assert_bool
	("are_literals_variants_rsubst:\n"
	 ^ Term.literal_to_string literal1_instance
	 ^ "\nand\n"
	 ^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	(Unification.are_literals_variants_rsubst literal1_instance literal2 subst2 offset2)
	success_desired;

      OUnit.assert_bool
	("are_literals_variants_substs:\n"
	 ^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	 ^ "\nand\n"
	 ^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	(Unification.are_literals_variants_substs literal1 subst1 offset1 literal2 subst2 offset2)
	success_desired*)


let not_are_literals_variants =
  are_literals_variants ~success_desired:false


let test_are_literals_variants1 =
  OUnit.TestCase (are_literals_variants l_g_a Subst.empty 0 l_g_a Subst.empty 1)

let test_are_literals_variants2 =
  OUnit.TestCase (not_are_literals_variants l_g_a Subst.empty 0 l_not_g_a Subst.empty 0 )

let test_are_literals_variants3 =
  OUnit.TestCase (are_literals_variants l_g_x Subst.empty 0 l_g_y Subst.empty 0)

let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 v_0 0
let test_are_literals_variants4 =
  OUnit.TestCase (are_literals_variants l_f_xy subst1 0 l_f_yx subst2 0)

let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 v_0 0
let test_are_literals_variants5 =
  OUnit.TestCase (not_are_literals_variants l_f_xy subst1 0 l_not_f_yx subst2 0)


let suite_are_literals_variants =
  OUnit.TestLabel (
    ("are_literals_variants"),
    (OUnit.TestList [
       test_are_literals_variants1;
       test_are_literals_variants2;
       test_are_literals_variants3;
       test_are_literals_variants4;
       test_are_literals_variants5;
     ]
    )
  )










(*** match_terms ***)
let match_terms ?(success_desired:bool = true)
  (term1: term) (subst1: subst) (offset1: int)
  (term2: term) (subst2: subst) (offset2: int) =

  fun () ->
    let term1_instance =
      Subst.apply_to_term subst1 term1 offset1
    and term2_instance =
      Subst.apply_to_term subst2 term2 offset2
    in
      if success_desired then begin
	let is_successful matched =
	  Term.term_equal
	    term2_instance
	    (Subst.apply_to_term matched term1_instance offset1)
	in

	let matched =
	  Unification.match_terms term1_instance offset1 term2_instance offset2
	in
	  OUnit.assert_true
	    ("match_terms:\n" ^ Term.term_to_string term1_instance ^ "\nand\n" ^ Term.term_to_string term2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);

	(*let matched =
	  Unification.match_terms_lsubst term1 subst1 offset1 term2_instance offset2
	in
	  OUnit.assert_true
	    ("match_terms_lsubst:\n"
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
	     ^ "\nand\n" ^ Term.term_to_string term2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);

	let matched =
	  Unification.match_terms_rsubst term1_instance offset1 term2 subst2 offset2
	in
	  OUnit.assert_true
	    ("match_terms_rsubst: "
	     ^ Term.term_to_string term1_instance
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2)
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);

	let matched =
	  Unification.match_terms_substs term1 subst1 offset1 term2 subst2 offset2
	in
	  OUnit.assert_true
	    ("match_terms_substs: "
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2)
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);*)
      end
      else begin
	OUnit.assert_raises
	  ~msg:("match_terms:\n" ^ Term.term_to_string term1_instance ^ "\nand\n" ^ Term.term_to_string term2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_terms term1_instance offset1 term2_instance offset2);
	(*
	OUnit.assert_raises
	  ~msg:("match_terms_lsubst:\n"
		^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
		^ "\nand\n" ^ Term.term_to_string term2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_terms_lsubst term1 subst1 offset1 term2_instance offset2);
	
	OUnit.assert_raises
	  ~msg:("match_terms_rsubst: "
		^ Term.term_to_string term1_instance
		^ " and "
		^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2))
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_terms_substs term1 subst1 offset1 term2 subst2 offset2);
	
	OUnit.assert_raises
	  ~msg:("match_terms_substs: "
		^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
		^ " and "
		^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2))
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_terms_substs term1 subst1 offset1 term2 subst2 offset2);*)
      end


let not_match_terms =
  match_terms ~success_desired:false


let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
let test_match_terms1 =
  OUnit.TestCase (match_terms g_x subst1 0 g_x subst2 0)

let test_match_terms2 =
  OUnit.TestCase (match_terms g_x Subst.empty 0 g_x Subst.empty 1)

let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 a 1
let test_match_terms3 =
  OUnit.TestCase (match_terms g_x Subst.empty 0 g_x subst2 1)

let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 0 a 0
let test_match_terms4 =
  OUnit.TestCase (not_match_terms g_u subst1 0 g_x Subst.empty 1)

let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 0 v_0 0
let test_match_terms5 =
  OUnit.TestCase (match_terms g_u subst1 0 g_u Subst.empty 1)

let test_match_terms6 =
  OUnit.TestCase (match_terms g_u Subst.empty 0 g_x Subst.empty 1)

let test_match_terms7 =
  OUnit.TestCase (match_terms g_u Subst.empty 0 g_u Subst.empty 1)

let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 a 1
let test_match_terms8 =
  OUnit.TestCase (match_terms j_xab Subst.empty 0 j_xab subst2 1)

let test_match_terms9 =
  OUnit.TestCase (not_match_terms f_xx Subst.empty 0 f_xa Subst.empty 1)

let test_match_terms10 =
  OUnit.TestCase (not_match_terms f_g_x_x Subst.empty 0 f_xx Subst.empty 1)

let test_match_terms11 =
  OUnit.TestCase (not_match_terms f_x_g_x Subst.empty 0 f_xx Subst.empty 1)

let test_match_terms12 =
  OUnit.TestCase (not_match_terms f_xx Subst.empty 0 f_g_x_g_a Subst.empty 1)


let suite_match_terms =
  OUnit.TestLabel (
    ("match_terms"),
    (OUnit.TestList [
       test_match_terms1;
       test_match_terms2;
       test_match_terms3;
       test_match_terms4;
       test_match_terms5;
       test_match_terms6;
       test_match_terms7;
       test_match_terms8;
       test_match_terms9;
       test_match_terms10;
       test_match_terms11;
       test_match_terms12;
     ]
    )
  )
















(*** match_literals ***)
let match_literals ?(success_desired:bool = true)
  (literal1: literal) (subst1: subst) (offset1: int)
  (literal2: literal) (subst2: subst) (offset2: int) =

  fun () ->
    let literal1_instance =
      Subst.apply_to_literal subst1 literal1 offset1
    and literal2_instance =
      Subst.apply_to_literal subst2 literal2 offset2
    in
      if success_desired then begin
	let is_successful matched =
	  Term.literal_equal
	    literal2_instance
	    (Subst.apply_to_literal matched literal1_instance offset1)
	in

	let matched =
	  Unification.match_literals literal1_instance offset1 literal2_instance offset2
	in
	  OUnit.assert_true
	    ("match_literals:\n" ^ Term.literal_to_string literal1_instance
	     ^ "\nand\n" ^ Term.literal_to_string literal2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);
	  (*
	let matched =
	  Unification.match_literals_lsubst literal1 subst1 offset1 literal2_instance offset2
	in
	  OUnit.assert_true
	    ("match_literals_lsubst:\n"
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	     ^ "\nand\n" ^ Term.literal_to_string literal2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);

	let matched =
	  Unification.match_literals_rsubst literal1_instance offset1 literal2 subst2 offset2
	in
	  OUnit.assert_true
	    ("match_literals_rsubst: "
	     ^ Term.literal_to_string literal1_instance
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);

	let matched =
	  Unification.match_literals_substs literal1 subst1 offset1 literal2 subst2 offset2
	in
	  OUnit.assert_true
	    ("match_literals_substs: "
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);*)
      end
      else begin
	OUnit.assert_raises
	  ~msg:("match_literals:\n" ^ Term.literal_to_string literal1_instance ^ "\nand\n" ^ Term.literal_to_string literal2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_literals literal1_instance offset1 literal2_instance offset2);
	(*
	OUnit.assert_raises
	  ~msg:("match_literals_lsubst:\n"
		^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
		^ "\nand\n" ^ Term.literal_to_string literal2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_literals_lsubst literal1 subst1 offset1 literal2_instance offset2);

	OUnit.assert_raises
	  ~msg:("match_literals_rsubst: "
		^ Term.literal_to_string literal1_instance
		^ " and "
		^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_literals_substs literal1 subst1 offset1 literal2 subst2 offset2);

	OUnit.assert_raises
	  ~msg:("match_literals_substs: "
		^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
		^ " and "
		^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_literals_substs literal1 subst1 offset1 literal2 subst2 offset2);*)
      end



let not_match_literals =
  match_literals ~success_desired:false


let test_match_literals1 =
  OUnit.TestCase (not_match_literals l_f_xy Subst.empty 0 l_not_f_yx Subst.empty 1)

let test_match_literals2 =
  OUnit.TestCase (match_literals l_f_xy Subst.empty 0 l_f_yx Subst.empty 1)

let subst1 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0
let test_match_literals3 =
  OUnit.TestCase (not_match_literals l_f_xy subst1 0 l_f_yx Subst.empty 1)

let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 v_1 1
let test_match_literals4 =
  OUnit.TestCase (match_literals l_f_xy Subst.empty 0 l_f_yx subst2 1)

let subst2 =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 v_1 1
let test_match_literals5 =
  OUnit.TestCase (not_match_literals l_not_f_xy Subst.empty 0 l_f_yx subst2 1)



let suite_match_literals =
  OUnit.TestLabel (
    ("match_literals"),
    (OUnit.TestList [
       test_match_literals1;
       test_match_literals2;
       test_match_literals3;
       test_match_literals4;
       test_match_literals5;
     ]
    )
  )






(*** p_match terms ***)
let p_match_terms ?(success_desired:bool = true)
  (term1: term) (subst1: subst) (offset1: int)
  (term2: term) (subst2: subst) (offset2: int)
  (expected: subst) =

  fun () ->
    let term1_instance =
      Subst.apply_to_term subst1 term1 offset1
    and term2_instance =
      Subst.apply_to_term subst2 term2 offset2
    in
      if success_desired then begin
	let is_successful matched =
	  (Term.term_equal
	    term2_instance
	    (Subst.apply_to_term matched term1_instance offset1))
	  &&
	  (Subst.subst_equal matched expected)
	in

	let matched =
	  Unification.match_terms ~p_preserving:true term1_instance offset1 term2_instance offset2
	in
	  OUnit.assert_true
	    ("p_match_terms:\n" ^ Term.term_to_string term1_instance
	     ^ "\nand\n" ^ Term.term_to_string term2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);
	  (*
	let matched =
	  Unification.p_match_terms_lsubst term1 subst1 offset1 term2_instance offset2
	in
	  OUnit.assert_true
	    ("p_match_terms_lsubst:\n"
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
	     ^ "\nand\n" ^ Term.term_to_string term2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);

	let matched =
	  Unification.p_match_terms_rsubst term1_instance offset1 term2 subst2 offset2
	in
	  OUnit.assert_true
	    ("p_match_terms_rsubst: "
	     ^ Term.term_to_string term1_instance
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2)
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);

	let matched =
	  Unification.p_match_terms_substs term1 subst1 offset1 term2 subst2 offset2
	in
	  OUnit.assert_true
	    ("p_match_terms_substs: "
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2)
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);*)
      end
      else begin
	OUnit.assert_raises
	  ~msg:("p_match_terms:\n" ^ Term.term_to_string term1_instance
		^ "\nand\n" ^ Term.term_to_string term2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_terms ~p_preserving:true term1_instance offset1 term2_instance offset2);
	(*
	OUnit.assert_raises
	  ~msg:("p_match_terms_lsubst:\n"
	   ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
	   ^ "\nand\n" ^ Term.term_to_string term2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.p_match_terms_lsubst term1 subst1 offset1 term2_instance offset2);

	OUnit.assert_raises
	  ~msg:("p_match_terms_rsubst: "
	   ^ Term.term_to_string term1_instance
	   ^ " and "
	   ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2))
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.p_match_terms_substs term1 subst1 offset1 term2 subst2 offset2);

	OUnit.assert_raises
	  ~msg:("p_match_terms_substs: "
	   ^ Subst.subst_to_string subst1 ^ "\n" ^ Subst.bound_term_to_string (term1, offset1)
	   ^ " and "
	   ^ Subst.subst_to_string subst2 ^ "\n" ^ Subst.bound_term_to_string (term2, offset2))
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.p_match_terms_substs term1 subst1 offset1 term2 subst2 offset2);*)
      end


let not_p_match_terms =
  p_match_terms ~success_desired:false




let subst1 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 0
let subst2 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 0
let expected =
  []
let test_p_match_terms1 =
  OUnit.TestCase (p_match_terms g_x subst1 0 g_x subst2 0 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 v_0 1
let test_p_match_terms2 =
  OUnit.TestCase (p_match_terms g_x Subst.empty 0 g_x Subst.empty 1 expected)

let subst2 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 a 1
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1
let test_p_match_terms3 =
  OUnit.TestCase (p_match_terms g_x Subst.empty 0 g_x subst2 1 expected)

(*let subst1 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par0 0 a 0
let expected =
  []
let test_p_match_terms4 =
  OUnit.TestCase (not_p_match_terms g_u subst1 0 g_x Subst.empty 1 expected)*)
(*
let subst1 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par0 0 v_0 0
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 p_0 1
let test_p_match_terms5 =
  OUnit.TestCase (p_match_terms g_u subst1 0 g_u Subst.empty 1 expected)*)

let expected =
  []
let test_p_match_terms6 =
  OUnit.TestCase (not_p_match_terms g_u Subst.empty 0 g_x Subst.empty 1 expected)

(*let subst1 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par0 0 a 0
let expected =
  []
let test_p_match_terms7 =
  OUnit.TestCase (not_p_match_terms g_u subst1 0 g_u Subst.empty 1 expected)*)

let subst2 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 a 1
let expected =
  []
let test_p_match_terms8 =
  OUnit.TestCase (not_p_match_terms j_vab Subst.empty 0 j_xab subst2 1 expected)

let expected =
  []
let test_p_match_terms9 =
  OUnit.TestCase (not_p_match_terms f_xx Subst.empty 0 f_xa Subst.empty 1 expected)

let expected =
  []
let test_p_match_terms10 =
  OUnit.TestCase (not_p_match_terms f_g_x_x Subst.empty 0 f_xx Subst.empty 1 expected)

let expected =
  []
let test_p_match_terms11 =
  OUnit.TestCase (not_p_match_terms f_x_g_x Subst.empty 0 f_xx Subst.empty 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 a 1
let test_p_match_terms12 =
  OUnit.TestCase (not_p_match_terms f_uv Subst.empty 0 f_uu Subst.empty 1 expected)


let suite_p_match_terms =
  OUnit.TestLabel (
    ("p_match_terms"),
    (OUnit.TestList [
       test_p_match_terms1;
       test_p_match_terms2;
       test_p_match_terms3;
(*       test_p_match_terms4;*)
(*       test_p_match_terms5;*)
       test_p_match_terms6;
(*       test_p_match_terms7;*)
       test_p_match_terms8;
       test_p_match_terms9;
       test_p_match_terms10;
       test_p_match_terms11;
       test_p_match_terms12;
     ]
    )
  )








(*** i_match terms ***)
let i_match_terms ?(success_desired:bool = true)
  (term1: term) (subst1: subst) (offset1: int)
  (term2: term) (subst2: subst) (offset2: int)
  (expected: subst) =

  fun () ->
    let term1_instance =
      Subst.apply_to_term subst1 term1 offset1
    and term2_instance =
      Subst.apply_to_term subst2 term2 offset2
    in
      if success_desired then begin
	let is_successful matched =
	  (Term.term_equal
	    term2_instance
	    (Subst.apply_to_term matched term1_instance offset1))
	  &&
	  (Subst.subst_equal matched expected)
	in

	let matched =
	  Unification.match_terms ~p_preserving:true ~i_preserving:true term1_instance offset1 term2_instance offset2
	in
	  OUnit.assert_true
	    ("i_match_terms:\n" ^ Term.term_to_string term1_instance
	     ^ "\nand\n" ^ Term.term_to_string term2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);
      end
      else begin
	OUnit.assert_raises
	  ~msg:("i_match_terms:\n" ^ Term.term_to_string term1_instance
		^ "\nand\n" ^ Term.term_to_string term2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_terms ~p_preserving:true ~i_preserving:true term1_instance offset1 term2_instance offset2);
      end


let not_i_match_terms =
  i_match_terms ~success_desired:false




let subst1 =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 a 0
let subst2 =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 a 0
let expected =
  []
let test_i_match_terms1 =
  OUnit.TestCase (i_match_terms g_x subst1 0 g_x subst2 0 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 v_0 1
let test_i_match_terms2 =
  OUnit.TestCase (i_match_terms g_x Subst.empty 0 g_x Subst.empty 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 vi_0 1
let test_i_match_terms3 =
  OUnit.TestCase (i_match_terms g_x Subst.empty 0 g_xi Subst.empty 1 expected)

let test_i_match_terms4 =
  OUnit.TestCase (not_i_match_terms g_xi Subst.empty 0 g_x Subst.empty 1 Subst.empty)

(*let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty vari0 0 pi_0 1*)
let test_i_match_terms5 =
  OUnit.TestCase (not_i_match_terms g_xi Subst.empty 0 g_ui Subst.empty 1 Subst.empty)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty pari0 0 pi_0 1
let test_i_match_terms6 =
  OUnit.TestCase (i_match_terms g_ui Subst.empty 0 g_ui Subst.empty 1 expected)

let test_i_match_terms7 =
  OUnit.TestCase (not_i_match_terms g_ui Subst.empty 0 g_xi Subst.empty 1 Subst.empty)

let suite_i_match_terms =
  OUnit.TestLabel (
    ("i_match_terms"),
    (OUnit.TestList [
       test_i_match_terms1;
       test_i_match_terms2;
       test_i_match_terms3;
       test_i_match_terms4;
       test_i_match_terms5;
       test_i_match_terms6;
       test_i_match_terms7;
     ]
    )
  )






(*** p_match_literals ***)
let p_match_literals ?(success_desired:bool = true)
  (literal1: literal) (subst1: subst) (offset1: int)
  (literal2: literal) (subst2: subst) (offset2: int)
  (expected: subst) =

  fun () ->
    let literal1_instance =
      Subst.apply_to_literal subst1 literal1 offset1
    and literal2_instance =
      Subst.apply_to_literal subst2 literal2 offset2
    in
      if success_desired then begin
	let is_successful matched =
	  Term.literal_equal
	    literal2_instance
	    (Subst.apply_to_literal matched literal1_instance offset1)
	in

	let matched =
	  Unification.match_literals ~p_preserving:true literal1_instance offset1 literal2_instance offset2
	in
	  OUnit.assert_true
	    ("p_match_literals:\n" ^ Term.literal_to_string literal1_instance
	     ^ "\nand\n" ^ Term.literal_to_string literal2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);
	  (*
	let matched =
	  Unification.p_match_literals_lsubst literal1 subst1 offset1 literal2_instance offset2
	in
	  OUnit.assert_true
	    ("p_match_literals_lsubst:\n"
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	     ^ "\nand\n" ^ Term.literal_to_string literal2_instance
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);
	  
	let matched =
	  Unification.p_match_literals_rsubst literal1_instance offset1 literal2 subst2 offset2
	in
	  OUnit.assert_true
	    ("p_match_literals_rsubst: "
	     ^ Term.literal_to_string literal1_instance
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);
	  
	let matched =
	  Unification.p_match_literals_substs literal1 subst1 offset1 literal2 subst2 offset2
	in
	  OUnit.assert_true
	    ("p_match_literals_substs: "
	     ^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	     ^ " and "
	     ^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2
	     ^ " is\n" ^ Subst.subst_to_string matched)
	    (is_successful matched);*)
      end
      else begin
	OUnit.assert_raises
	  ~msg:("p_match_literals:\n" ^ Term.literal_to_string literal1_instance ^ "\nand\n" ^ Term.literal_to_string literal2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.match_literals ~p_preserving:true literal1_instance offset1 literal2_instance offset2);
	(*
	OUnit.assert_raises
	  ~msg:("p_match_literals_lsubst:\n"
		^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
		^ "\nand\n" ^ Term.literal_to_string literal2_instance)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.p_match_literals_lsubst literal1 subst1 offset1 literal2_instance offset2);

	OUnit.assert_raises
	  ~msg:("p_match_literals_rsubst: "
		^ Term.literal_to_string literal1_instance
		^ " and "
		^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.p_match_literals_substs literal1 subst1 offset1 literal2 subst2 offset2);

	OUnit.assert_raises
	  ~msg:("p_match_literals_substs: "
		^ Subst.subst_to_string subst1 ^ "\n" ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
		^ " and "
		^ Subst.subst_to_string subst2 ^ "\n" ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	  Unification.UNIFICATION_FAIL
	  (fun () -> Unification.p_match_literals_substs literal1 subst1 offset1 literal2 subst2 offset2);*)
      end



let not_p_match_literals =
  p_match_literals ~success_desired:false



let expected =
  []
let test_p_match_literals1 =
  OUnit.TestCase (not_p_match_literals l_f_xy Subst.empty 0 l_not_f_yx Subst.empty 1 expected)

let expected =
  []
let test_p_match_literals2 =
  OUnit.TestCase (p_match_literals l_f_xy Subst.empty 0 l_f_yx Subst.empty 1 expected)

let subst1 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 v_1 0
let expected =
  []
let test_p_match_literals3 =
  OUnit.TestCase (not_p_match_literals l_f_xy subst1 0 l_f_yx Subst.empty 1 expected)

let subst2 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 v_1 1
let expected =
  []
let test_p_match_literals4 =
  OUnit.TestCase (p_match_literals l_f_xy Subst.empty 0 l_f_yx subst2 1 expected)

let subst2 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 v_1 1
let expected =
  []
let test_p_match_literals5 =
  OUnit.TestCase (not_p_match_literals l_not_f_xy Subst.empty 0 l_f_yx subst2 1 expected)

(*let subst1 =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par1 0 a 0
let expected =
  []
let test_p_match_literals6 =
  OUnit.TestCase (not_p_match_literals l_g_v subst1 0 l_g_v Subst.empty 1 expected)*)



let suite_p_match_literals =
  OUnit.TestLabel (
    ("p_match_literals"),
    (OUnit.TestList [
       test_p_match_literals1;
       test_p_match_literals2;
       test_p_match_literals3;
       test_p_match_literals4;
       test_p_match_literals5;
(*       test_p_match_literals6;*)
     ]
    )
  )















(*** unify_terms ***)
let unify_terms ?(success_desired:bool = true) (term1: term) (offset1: int) (term2: term) (offset2: int)
  (expected: subst) =

  fun () ->
    try
      let unified =
	Unification.unify_terms term1 offset1 term2 offset2
      in
      let success =
	Tools.lists_unordered_equal (=) unified expected
      in
	OUnit.assert_bool
	  ("unify_terms:\n" 
	   ^ Subst.term_to_string (Subst.make_term term1 offset1)
	   ^ "\nand\n"
	   ^ Subst.term_to_string (Subst.make_term term2 offset2) ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success

    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("unify_terms:\n" 
	       ^ Subst.term_to_string (Subst.make_term term1 offset1)
	       ^ "\nand\n"
	       ^ Subst.term_to_string (Subst.make_term term2 offset2) ^ " failed")	      
	  end


let not_unify_terms =
  unify_terms ~success_desired:false


let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_0 1
let test_unify_terms1 =
  OUnit.TestCase (unify_terms g_x 0 g_x 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 1
let test_unify_terms2 =
  OUnit.TestCase (unify_terms g_x 0 g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 a 0
let test_unify_terms3 =
  OUnit.TestCase (unify_terms g_a 0 g_x 1 expected)

let test_unify_terms4 =
  OUnit.TestCase (not_unify_terms f_x_g_x 0 f_g_x_x 1 [])

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_x 1)
    var0 1 a 1
let test_unify_terms5 =
  OUnit.TestCase (unify_terms f_x_g_x 0 f_g_x_g_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_0 1)
    var1 0 v_0 1
let test_unify_terms6 =
  OUnit.TestCase (unify_terms f_xy 0 f_xx 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 p_0 1)
    var1 0 p_0 1
let test_unify_terms7 =
  OUnit.TestCase (unify_terms f_xy 0 f_vv 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 p_0 0)
    par1 0 p_0 0
let test_unify_terms8 =
  OUnit.TestCase (unify_terms f_uv 0 f_xx 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 p_0 0)
    par1 0 p_0 0
let test_unify_terms8 =
  OUnit.TestCase (unify_terms f_uv 0 f_xx 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 1 p_0 0)
    var1 0 p_0 0
let test_unify_terms8 =
  OUnit.TestCase (unify_terms f_ux 0 f_xx 1 expected)


let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 1 g_x 0
let test_unify_terms9 =
  OUnit.TestCase (unify_terms g_g_x 0 g_u 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 0 p_1 0)
    par0 1 p_0 0
let test_unify_terms10 =
  OUnit.TestCase (unify_terms f_uv 0 f_vv 1 expected)


let suite_unify_terms =
  OUnit.TestLabel (
    ("unify_terms"),
    (OUnit.TestList [
       test_unify_terms1;
       test_unify_terms2;
       test_unify_terms3;
       test_unify_terms4;
       test_unify_terms5;
       test_unify_terms6;
       test_unify_terms7;
       test_unify_terms8;
       test_unify_terms9;
       test_unify_terms10;
     ]
    )
  )









(*** unify_literals ***)
let unify_literals ?(success_desired:bool = true) (literal1: literal) (offset1: int) (literal2: literal) (offset2: int)
  (expected: subst) =

  fun () ->
    try
      let unified =
	Unification.unify_literals literal1 offset1 literal2 offset2
      in
      let success =
	Tools.lists_unordered_equal (=) unified expected
      in
	OUnit.assert_bool
	  ("unify_literals:\n" 
	   ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	   ^ "\nand\n"
	   ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2 ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success

    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("unify_literals:\n" 
	       ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	       ^ "\nand\n"
	       ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	  end


let not_unify_literals =
  unify_literals ~success_desired:false


let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 1
let test_unify_literals1 =
  OUnit.TestCase (unify_literals l_g_x 0 l_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 1
let test_unify_literals2 =
  OUnit.TestCase (not_unify_literals l_not_g_x 0 l_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 1
let test_unify_literals3 =
  OUnit.TestCase (not_unify_literals l_g_x 0 l_not_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 1
let test_unify_literals4 =
  OUnit.TestCase (unify_literals l_not_g_x 0 l_not_g_a 1 expected)


let suite_unify_literals =
  OUnit.TestLabel (
    ("unify_literals"),
    (OUnit.TestList [
       test_unify_literals1;
       test_unify_literals2;
       test_unify_literals3;
       test_unify_literals4;
     ]
    )
  )











(*** p_unify_terms ***)
let p_unify_terms ?(success_desired:bool = true) (term1: term) (offset1: int) (term2: term) (offset2: int)
  (expected: subst) =

  fun () ->
    try
      let unified =
	Unification.unify_terms ~p_preserving:true term1 offset1 term2 offset2
      in
      let success =
	Tools.lists_unordered_equal (=) unified expected
      in
	OUnit.assert_bool
	  ("p_unify_terms:\n" 
	   ^ Subst.term_to_string (Subst.make_term term1 offset1)
	   ^ "\nand\n"
	   ^ Subst.term_to_string (Subst.make_term term2 offset2) ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success

    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("p_unify_terms:\n" 
	       ^ Subst.term_to_string (Subst.make_term term1 offset1)
	       ^ "\nand\n"
	       ^ Subst.term_to_string (Subst.make_term term2 offset2) ^ " failed")	      
	  end


let not_p_unify_terms =
  p_unify_terms ~success_desired:false


let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 v_0 1
let test_p_unify_terms1 =
  OUnit.TestCase (p_unify_terms g_x 0 g_x 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1
let test_p_unify_terms2 =
  OUnit.TestCase (p_unify_terms g_x 0 g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 a 0
let test_p_unify_terms3 =
  OUnit.TestCase (p_unify_terms g_a 0 g_x 1 expected)

let test_p_unify_terms4 =
  OUnit.TestCase (not_p_unify_terms f_x_g_x 0 f_g_x_x 1 [])

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_x 1)
    var0 1 a 1
let test_p_unify_terms5 =
  OUnit.TestCase (p_unify_terms f_x_g_x 0 f_g_x_g_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 v_0 1)
    var1 0 v_0 1
let test_p_unify_terms6 =
  OUnit.TestCase (p_unify_terms f_xy 0 f_xx 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 p_0 1)
    var1 0 p_0 1
let test_p_unify_terms7 =
  OUnit.TestCase (p_unify_terms f_xy 0 f_vv 1 expected)

let test_p_unify_terms8 =
  OUnit.TestCase (not_p_unify_terms f_uv 0 f_xx 1 Subst.empty)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 p_0 0)
    var1 0 p_0 0
let test_p_unify_terms9 =
  OUnit.TestCase (p_unify_terms f_ux 0 f_xx 1 expected)

let test_p_unify_terms10 =
  OUnit.TestCase (not_p_unify_terms g_g_x 0 g_u 1 [])

let test_p_unify_terms11 =
  OUnit.TestCase (not_p_unify_terms f_uv 0 f_vv 1 [])

let test_p_unify_terms12 =
  OUnit.TestCase (not_p_unify_terms f_vv 0 f_g_u_g_v 1 [])

let test_p_unify_terms13 =
  OUnit.TestCase (not_p_unify_terms f_uv 0 f_uu 1 expected)


let suite_p_unify_terms =
  OUnit.TestLabel (
    ("p_unify_terms"),
    (OUnit.TestList [
       test_p_unify_terms1;
       test_p_unify_terms2;
       test_p_unify_terms3;
       test_p_unify_terms4;
       test_p_unify_terms5;
       test_p_unify_terms6;
       test_p_unify_terms7;
       test_p_unify_terms8;
       test_p_unify_terms9;
       test_p_unify_terms10;
       test_p_unify_terms11;
       test_p_unify_terms12;
       test_p_unify_terms13;
     ]
    )
  )







(*** p_unify_literals ***)
let p_unify_literals ?(success_desired:bool = true) (literal1: literal) (offset1: int) (literal2: literal) (offset2: int)
  (expected: subst) =

  fun () ->
    try
      let unified =
	Unification.unify_literals ~p_preserving:true literal1 offset1 literal2 offset2
      in
      let success =
	Tools.lists_unordered_equal (=) unified expected
      in
	OUnit.assert_bool
	  ("p_unify_literals:\n" 
	   ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	   ^ "\nand\n"
	   ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2 ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success

    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("p_unify_literals:\n" 
	       ^ string_of_int offset1 ^ ": " ^ Term.literal_to_string literal1
	       ^ "\nand\n"
	       ^ string_of_int offset2 ^ ": " ^ Term.literal_to_string literal2)
	  end


let not_p_unify_literals =
  p_unify_literals ~success_desired:false


let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1
let test_p_unify_literals1 =
  OUnit.TestCase (p_unify_literals l_g_x 0 l_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1
let test_p_unify_literals2 =
  OUnit.TestCase (not_p_unify_literals l_not_g_x 0 l_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1
let test_p_unify_literals3 =
  OUnit.TestCase (not_p_unify_literals l_g_x 0 l_not_g_a 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1
let test_p_unify_literals4 =
  OUnit.TestCase (p_unify_literals l_not_g_x 0 l_not_g_a 1 expected)

let test_p_unify_literals5 =
  OUnit.TestCase (not_p_unify_literals l_f_uv 0 l_f_vv 1 [])


let suite_p_unify_literals =
  OUnit.TestLabel (
    ("p_unify_literals"),
    (OUnit.TestList [
       test_p_unify_literals1;
       test_p_unify_literals2;
       test_p_unify_literals3;
       test_p_unify_literals4;
       test_p_unify_literals5;
     ]
    )
  )








(*** i_unify_terms ***)
let i_unify_terms ?(success_desired:bool = true) (term1: term) (offset1: int) (term2: term) (offset2: int)
  (expected: subst) =

  fun () ->
    try
      let unified =
	Unification.unify_terms ~i_preserving:true ~p_preserving:true term1 offset1 term2 offset2
      in
      let success =
	Subst.subst_equal unified expected
      in
	OUnit.assert_bool
	  ("i_unify_terms:\n" 
	   ^ Subst.term_to_string (Subst.make_term term1 offset1)
	   ^ "\nand\n"
	   ^ Subst.term_to_string (Subst.make_term term2 offset2) ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success

    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("i_unify_terms:\n" 
	       ^ Subst.term_to_string (Subst.make_term term1 offset1)
	       ^ "\nand\n"
	       ^ Subst.term_to_string (Subst.make_term term2 offset2) ^ " failed")	      
	  end


let not_i_unify_terms =
  i_unify_terms ~success_desired:false


let test_i_unify_terms1 =
  OUnit.TestCase (not_i_unify_terms g_xi 0 g_xi 1 Subst.empty)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 1 vi_0 0
let test_i_unify_terms2 =
  OUnit.TestCase (i_unify_terms g_xi 0 g_x 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 vi_0 1
let test_i_unify_terms3 =
  OUnit.TestCase (i_unify_terms g_x 0 g_xi 1 expected)

let test_i_unify_terms4 =
  OUnit.TestCase (not_i_unify_terms g_ui 0 g_ui 1 Subst.empty)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty par0 1 pi_0 0
let test_i_unify_terms5 =
  OUnit.TestCase (i_unify_terms g_ui 0 g_u 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty par0 0 pi_0 1
let test_i_unify_terms6 =
  OUnit.TestCase (i_unify_terms g_u 0 g_ui 1 expected)

let test_i_unify_terms7 =
  OUnit.TestCase (not_i_unify_terms g_ui 0 g_xi 1 Subst.empty)


let test_i_unify_terms8 =
  OUnit.TestCase (not_i_unify_terms g_xi 0 g_a 1 Subst.empty)

let test_i_unify_terms9 =
  OUnit.TestCase (not_i_unify_terms f_xx 0 f_xiyi 1 Subst.empty)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 vi_0 1)
    var1 0 vi_1 1
let test_i_unify_terms10 =
  OUnit.TestCase (i_unify_terms f_xy 0 f_xiyi 1 expected)

let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 vi_0 1)
    var1 1 a 0
let test_i_unify_terms11 =
  OUnit.TestCase (i_unify_terms f_xa 0 f_xiy 1 expected)

let test_i_unify_terms12 =
  OUnit.TestCase (not_i_unify_terms f_xa 0 f_xiu 1 Subst.empty)

let suite_i_unify_terms =
  OUnit.TestLabel (
    ("i_unify_terms"),
    (OUnit.TestList [
       test_i_unify_terms1;
       test_i_unify_terms2;
       test_i_unify_terms3;
       test_i_unify_terms4;
       test_i_unify_terms5;
       test_i_unify_terms6;
       test_i_unify_terms7;
       test_i_unify_terms8;
       test_i_unify_terms9;
       test_i_unify_terms10;
       test_i_unify_terms11;
       test_i_unify_terms12;
     ]
    )
  )








(*** unify_substs ***)
let unify_substs ?(success_desired:bool = true) (substs: subst list) (expected: subst) =
  fun () ->
    try
      let unified =
	List.fold_left
	  (fun acc subst ->
	     Unification.unify_substs acc subst
	  )
	  Subst.empty
	  substs
      in
      let success =
	Subst.subst_equal unified expected
      in
	OUnit.assert_bool
	  ("unify_substs:\n" 
	   ^ String.concat "\n" (List.map Subst.subst_to_string substs) ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success
	  
    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("unify_substs:\n"
	       ^ String.concat "\n" (List.map Subst.subst_to_string substs)
	       ^ "\nfailed but should be\n"
	       ^ Subst.subst_to_string expected)
	  end


let not_unify_substs =
  unify_substs ~success_desired:false



let substs =
  []
let expected =
  []
let test_unify_substs1 =
  OUnit.TestCase (unify_substs substs expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
  ]
let expected =
  []
let test_unify_substs2 =
  OUnit.TestCase (not_unify_substs substs expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
let test_unify_substs3 =
  OUnit.TestCase (unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
let test_unify_substs4 =
  OUnit.TestCase (unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 b 0
  ]
let expected =
  []
let test_unify_substs5 =
  OUnit.TestCase (not_unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_y 1;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 1 v_1 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0;
  ]
let expected =
  []
let test_unify_substs6 =
  OUnit.TestCase (not_unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_y 1;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 1 v_2 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0;
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false
       (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_y 1)
       var1 1 v_2 0)
    var1 0 v_0 0
let test_unify_substs7 =
  OUnit.TestCase (unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 0 g_y 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var2 0 p_0 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 b 0;
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false
       (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 0 g_y 0)
       var1 0 b 0)
    var2 0 g_y 0
let test_unify_substs8 =
  OUnit.TestCase (unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_u 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_a 0;
  ]
let expected =
  (Subst.set_ ~p_preserving:false ~i_preserving:false
     (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_u 0)
     par0 0 a 0)
let test_unify_substs9 =
  OUnit.TestCase (unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var4 0 q_23 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false
      (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var2 0 v_1 0)
      var3 0 v_1 0;
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false
       (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var4 0 q_23 0)
       var2 0 v_1 0)
    var3 0 v_1 0
let test_unify_substs10 =
  OUnit.TestCase (unify_substs substs expected)



let suite_unify_substs =
  OUnit.TestLabel (
    ("unify_substs"),
    (OUnit.TestList [
       test_unify_substs1;
       test_unify_substs2;
       test_unify_substs3;
       test_unify_substs4;
       test_unify_substs5;
       test_unify_substs6;
       test_unify_substs7;
       test_unify_substs8;
       test_unify_substs9;
       test_unify_substs10;
     ]
    )
  )










(*** p_unify_substs ***)
let p_unify_substs ?(success_desired:bool = true) (substs: subst list) (expected: subst) =
  fun () ->
    try
      let p_unified =
	List.fold_left
	  (fun acc subst ->
	     Unification.unify_substs ~p_preserving:true acc subst
	  )
	  Subst.empty
	  substs
      in
      let success =
	Subst.subst_equal p_unified expected
      in
	OUnit.assert_bool
	  ("p_unify_substs:\n" 
	   ^ String.concat "\n" (List.map Subst.subst_to_string substs) ^ " is\n"
	   ^ Subst.subst_to_string p_unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success
	  
    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("p_unify_substs:\n"
	       ^ String.concat "\n" (List.map Subst.subst_to_string substs)
	       ^ "\nfailed but should be\n"
	       ^ Subst.subst_to_string expected)
	  end


let not_p_unify_substs =
  p_unify_substs ~success_desired:false



(*
let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par0 0 g_y 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var2 0 p_0 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 b 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false
       (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par0 0 g_y 0)
       var1 0 b 0)
    var2 0 g_y 0
let test_p_unify_substs1 =
  OUnit.TestCase (not_p_unify_substs substs expected)
*)

let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_u 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_a 0;
  ]
(*let expected =
  (Subst.set_ ~p_preserving:true ~i_preserving:false
     (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_u 0)
     par0 0 a 0)*)
let test_p_unify_substs2 =
  OUnit.TestCase (not_p_unify_substs substs Subst.empty)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_u 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_y 0;
  ]
let expected =
  (Subst.set_ ~p_preserving:true ~i_preserving:false
     (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_u 0)
     var1 0 p_0 0)
let test_p_unify_substs3 =
  OUnit.TestCase (p_unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 f_uv 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 f_uu 1;
  ]
let expected =
  []
let test_p_unify_substs4 =
  OUnit.TestCase (not_p_unify_substs substs expected)



let suite_p_unify_substs =
  OUnit.TestLabel (
    ("p_unify_substs"),
    (OUnit.TestList [
(*       test_p_unify_substs1;*)
       test_p_unify_substs2;
       test_p_unify_substs3;
       test_p_unify_substs4;
     ]
    )
  )












(*** i_unify_substs ***)
let i_unify_substs ?(success_desired:bool = true) (substs: subst list) (expected: subst) =
  fun () ->
    try
      let unified =
	List.fold_left
	  (fun acc subst ->
	     Unification.unify_substs ~p_preserving:true ~i_preserving:true acc subst
	  )
	  Subst.empty
	  substs
      in
      let success =
	Subst.subst_equal unified expected
      in
	OUnit.assert_bool
	  ("i_unify_substs:\n" 
	   ^ String.concat "\n" (List.map Subst.subst_to_string substs) ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success
	  
    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("i_unify_substs:\n"
	       ^ String.concat "\n" (List.map Subst.subst_to_string substs)
	       ^ "\nfailed but should be\n"
	       ^ Subst.subst_to_string expected)
	  end


let not_i_unify_substs =
  i_unify_substs ~success_desired:false




let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_xi 1;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_x 1;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_xi 1)
    var0 1 vi_0 1
let test_i_unify_substs1 =
  OUnit.TestCase (i_unify_substs substs expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_xi 1;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_a 1;
  ]
let test_i_unify_substs2 =
  OUnit.TestCase (not_i_unify_substs substs Subst.empty)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var2 0 vi_1 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_a 0;
  ]
let test_i_unify_substs3 =
  OUnit.TestCase (not_i_unify_substs substs Subst.empty)



let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var2 0 vi_1 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_y 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true
       (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0)
       var1 0 vi_1 0)
    var2 0 vi_1 0
let test_i_unify_substs4 =
  OUnit.TestCase (i_unify_substs substs expected)




let suite_i_unify_substs =
  OUnit.TestLabel (
    ("i_unify_substs"),
    (OUnit.TestList [
       test_i_unify_substs1;
       test_i_unify_substs2;
       test_i_unify_substs3;
       test_i_unify_substs4;
     ]
    )
  )












(*** match_substs ***)
let match_substs ?(success_desired:bool = true) (substs: subst list) (more_general_offset: int) (expected: subst) =
  fun () ->
    try
      let matched =
	List.fold_left
	  (fun acc subst ->
	     Unification.match_substs more_general_offset acc subst
	  )
	  Subst.empty
	  substs
      in
      let success =
	Subst.subst_equal matched expected
      in
	OUnit.assert_bool
	  ("match_substs:\n" 
	   ^ String.concat "\n" (List.map Subst.subst_to_string substs) ^ " is\n"
	   ^ Subst.subst_to_string matched ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success
	  
    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("match_substs:\n"
	       ^ String.concat "\n" (List.map Subst.subst_to_string substs)
	       ^ "\nfailed but should be\n"
	       ^ Subst.subst_to_string expected)
	  end


let not_match_substs =
  match_substs ~success_desired:false



let substs =
  []
let expected =
  []
let test_match_substs1 =
  OUnit.TestCase (match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
  ]
let expected =
  []
let test_match_substs2 =
  OUnit.TestCase (not_match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
let test_match_substs3 =
  OUnit.TestCase (match_substs substs 0 expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0
let test_match_substs4 =
  OUnit.TestCase (match_substs substs 0 expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 a 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 b 0
  ]
let expected =
  []
let test_match_substs5 =
  OUnit.TestCase (not_match_substs substs 0 expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_y 1;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 1 v_1 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 v_1 0;
  ]
let expected =
  []
let test_match_substs6 =
  OUnit.TestCase (not_match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 1 g_y 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var2 0 p_0 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var1 0 b 0;
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false
       (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty par0 1 g_y 0)
       var1 0 b 0)
    var2 0 p_0 0
let test_match_substs7 =
  OUnit.TestCase (match_substs substs 1 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_u 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_a 0;
  ]
let expected =
  (Subst.set_ ~p_preserving:false ~i_preserving:false
     (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_u 0)
     par0 0 a 0)
let test_match_substs8 =
  OUnit.TestCase (match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_u 1;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_a 0;
  ]
let expected =
  (Subst.set_ ~p_preserving:false ~i_preserving:false
     (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_u 0)
     par0 0 a 0)
let test_match_substs9 =
  OUnit.TestCase (not_match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_v 0;
    Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_w 0;
  ]
let expected =
  Subst.set_ ~p_preserving:false ~i_preserving:false
    (Subst.set_ ~p_preserving:false ~i_preserving:false Subst.empty var0 0 g_v 0)
    par1 0 p_2 0
let test_match_substs10 =
  OUnit.TestCase (match_substs substs 0 expected)


let suite_match_substs =
  OUnit.TestLabel (
    ("match_substs"),
    (OUnit.TestList [
       test_match_substs1;
       test_match_substs2;
       test_match_substs3;
       test_match_substs4;
       test_match_substs5;
       test_match_substs6;
       test_match_substs7;
       test_match_substs8;
       test_match_substs9;
       test_match_substs10;
     ]
    )
  )










(*** p_match_substs ***)
let p_match_substs ?(success_desired:bool = true) (substs: subst list) (subsuming_offset: int) (expected: subst) =
  fun () ->
    try
      let p_matched =
	List.fold_left
	  (fun acc subst ->
	     Unification.match_substs ~p_preserving:true subsuming_offset acc subst
	  )
	  Subst.empty
	  substs
      in
      let success =
	Subst.subst_equal p_matched expected
      in
	OUnit.assert_bool
	  ("p_match_substs:\n" 
	   ^ String.concat "\n" (List.map Subst.subst_to_string substs) ^ " is\n"
	   ^ Subst.subst_to_string p_matched ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success
	  
    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("p_match_substs:\n"
	       ^ String.concat "\n" (List.map Subst.subst_to_string substs)
	       ^ "\nfailed but should be\n"
	       ^ Subst.subst_to_string expected)
	  end


let not_p_match_substs =
  p_match_substs ~success_desired:false



(*let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par0 0 g_y 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var2 0 p_0 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 b 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false
       (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty par0 0 g_y 0)
       var1 0 b 0)
    var2 0 g_y 0
let test_p_match_substs1 =
  OUnit.TestCase (not_p_match_substs substs 0 expected)*)

let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_u 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_a 0;
  ]
(*let expected =
  (Subst.set_ ~p_preserving:true ~i_preserving:false
     (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_u 0)
     par0 0 a 0)*)
let test_p_match_substs2 =
  OUnit.TestCase (not_p_match_substs substs 0 Subst.empty)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var4 0 q_yz 1;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var4 0 q_xx 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false
       (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var4 0 q_yz 1)
       var1 1 v_0 0)
    var2 1 v_0 0
let test_p_match_substs3 =
  OUnit.TestCase (p_match_substs substs 1 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false
      (Subst.set_ ~p_preserving:true ~i_preserving:false
	 (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 b 1)
	 var2 0 b 1)
      var0 0 q_bb 1;

    Subst.set_ ~p_preserving:true ~i_preserving:false
      (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 v_3 0)
      var2 0 v_3 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false
       (Subst.set_ ~p_preserving:true ~i_preserving:false
	  (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 b 1)
	  var2 0 b 1)
       var0 0 q_bb 1)
    var3 0 b 1
let test_p_match_substs4 =
  OUnit.TestCase (p_match_substs substs 0 expected)



(* f(x, y) - f(x, x) -> f(a, a) *)
let substs =
  [ 
    (Subst.set_ ~p_preserving:true ~i_preserving:false
       (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1)
       var1 0 a 1);

    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 v_0 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1)
    var1 0 a 1
let test_p_match_substs5 =
  OUnit.TestCase (p_match_substs substs 0 expected)



let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 q_12 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 p_1 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var2 0 p_2 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 q_uu 1;
  ]
(*let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false
       (Subst.set_ ~p_preserving:true ~i_preserving:false
	  (Subst.set_ ~p_preserving:true ~i_preserving:false
	     (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 q_12 0)
	     var1 0 p_0 1)
	  var2 0 p_0 1)
       par1 0 p_0 1)
    par2 0 p_0 1*)
let test_p_match_substs6 =
  OUnit.TestCase (not_p_match_substs substs 0 Subst.empty)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_v 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_w 1;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_v 0)
    par1 0 p_2 1
let test_p_match_substs7 =
  OUnit.TestCase (p_match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_v 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 1 g_w 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:false
    (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_v 0)
    var0 1 g_w 0
let test_p_match_substs8 =
  OUnit.TestCase (p_match_substs substs 0 expected)



let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_v 0;
    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 g_w 0;
  ]
let expected =
  Subst.empty
let test_p_match_substs9 =
  OUnit.TestCase (not_p_match_substs substs 0 expected)


(* q(x, y) - q(b, b) -> b(x, b) *)
(*
let substs =
  [ 
    (Subst.set_ ~p_preserving:true ~i_preserving:false
       (Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var0 0 a 1)
       var1 0 a 1);

    Subst.set_ ~p_preserving:true ~i_preserving:false Subst.empty var1 0 v_0 0;
  ]

q/2(_0, b/0)
into
0: q/2(_1, _2)
0: { 
[ Offset 0: _0 -> 0: q/2(_1, _2) ] }

  1: q/2(b/0, b/0)
  1: { 
[ Offset 0: _1 -> 0: b/0 ]
[ Offset 0: _2 -> 0: b/0 ] }
*)

let suite_p_match_substs =
  OUnit.TestLabel (
    ("p_match_substs"),
    (OUnit.TestList [
(*       test_p_match_substs1;*)
       test_p_match_substs2;
       test_p_match_substs3;
       test_p_match_substs4;
       test_p_match_substs5;
       test_p_match_substs6;
       test_p_match_substs7;
       test_p_match_substs8;
       test_p_match_substs9;
     ]
    )
  )








(*** i_match_substs ***)
let i_match_substs ?(success_desired:bool = true) (substs: subst list) (subsuming_offset: int) (expected: subst) =
  fun () ->
    try
      let unified =
	List.fold_left
	  (fun acc subst ->
	     Unification.match_substs ~p_preserving:true ~i_preserving:true subsuming_offset acc subst
	  )
	  Subst.empty
	  substs
      in
      let success =
	Subst.subst_equal unified expected
      in
	OUnit.assert_bool
	  ("i_match_substs:\n" 
	   ^ String.concat "\n" (List.map Subst.subst_to_string substs) ^ " is\n"
	   ^ Subst.subst_to_string unified ^ " but should be\n"
	   ^ Subst.subst_to_string expected)
	  success_desired
	  success
	  
    with
      | Unification.UNIFICATION_FAIL ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("i_match_substs:\n"
	       ^ String.concat "\n" (List.map Subst.subst_to_string substs)
	       ^ "\nfailed but should be\n"
	       ^ Subst.subst_to_string expected)
	  end


let not_i_match_substs =
  i_match_substs ~success_desired:false




let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var1 0 g_x 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var1 0 g_xi 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var1 0 g_x 0)
    var0 0 vi_0 0
let test_i_match_substs1 =
  OUnit.TestCase (i_match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var1 0 g_xi 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var1 0 g_x 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var1 0 g_xi 0)
    var0 0 vi_0 0
let test_i_match_substs2 =
  OUnit.TestCase (i_match_substs substs 0 expected)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_xi 1;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_a 1;
  ]
let test_i_match_substs3 =
  OUnit.TestCase (not_i_match_substs substs 0 Subst.empty)


let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var2 0 vi_1 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_y 0;
  ]
let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true
       (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0)
       var1 0 vi_1 0)
    var2 0 vi_1 0
let test_i_match_substs4 =
  OUnit.TestCase (i_match_substs substs 0 expected)

let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_x 1;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var2 0 vi_1 0;
  ]
let test_i_match_substs5 =
  OUnit.TestCase (not_i_match_substs substs 0 Subst.empty)


let expected =
  Subst.set_ ~p_preserving:true ~i_preserving:true
    (Subst.set_ ~p_preserving:true ~i_preserving:true
       (Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0)
       vari1 0 vi_0 1)
    var2 0 vi_1 0
let substs =
  [ 
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_z 0;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var0 0 g_xi 1;
    Subst.set_ ~p_preserving:true ~i_preserving:true Subst.empty var2 0 vi_1 0;
  ]
let test_i_match_substs5 =
  OUnit.TestCase (i_match_substs substs 0 expected)





let suite_i_match_substs =
  OUnit.TestLabel (
    ("i_match_substs"),
    (OUnit.TestList [
       test_i_match_substs1;
       test_i_match_substs2;
       test_i_match_substs3;
       test_i_match_substs4;
       test_i_match_substs5;
     ]
    )
  )







(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_are_terms_variants;
    suite_are_literals_variants;
    suite_match_terms;
    suite_match_literals;
    suite_p_match_terms;
    suite_p_match_literals;
    suite_i_match_terms;
    suite_unify_terms;
    suite_unify_literals;
    suite_p_unify_terms;
    suite_p_unify_literals;
    suite_i_unify_terms;
    suite_unify_substs;
    suite_p_unify_substs;
    suite_i_unify_substs;
    suite_match_substs;
    suite_p_match_substs;
    suite_i_match_substs;
  ]

let _ =
  OUnit.run_test_tt_main test_suite
