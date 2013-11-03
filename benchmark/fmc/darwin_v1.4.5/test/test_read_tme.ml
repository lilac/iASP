(* $Id: test_read_tme.ml,v 1.4 2004/04/29 09:24:21 alexf Exp $ *)

(*** to_clauses = read file ***)
let clauses_to_string (clauses: Term.clause list) : string =
  String.concat "\n" (List.map Term.clause_to_string clauses)

let to_clauses ?(success_desired:bool = true) (file: string) (expected: string list) =
  fun () ->
    let converted =
      Read_tme.to_clauses_from_file file
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
  OUnit.TestCase (to_clauses "PUZ001-1.tme"
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



(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_to_clauses
  ]

let _ =
  OUnit.run_test_tt_main test_suite
