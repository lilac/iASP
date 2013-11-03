(* $Id: test_tools.ml,v 1.3 2004/03/28 19:01:01 alexf Exp $ *)

let list_remove_first ?(success_desired:bool = true) (condition: 'a -> bool) (list: 'a list) (expected: 'a list) =
  fun () ->
    let shortened_list =
      Tools.list_remove_first condition list
    in
    let success =
      (List.length shortened_list = List.length expected)
      &&
      (List.for_all2 (=) shortened_list expected)
    in
      OUnit.assert_bool
	("list_remove_first: ")
	success_desired
	success

let not_list_remove_first condition list expected =
  list_remove_first ~success_desired:false condition list expected


let test_list_remove_first1 =
  OUnit.TestCase (list_remove_first ((=) 2) [0; 1; 2] [0; 1])

let test_list_remove_first2 =
  OUnit.TestCase (not_list_remove_first ((=) 2) [0; 1] [0; 1; 2])

let test_list_remove_first3 =
  OUnit.TestCase (list_remove_first ((=) 2) [0; 2; 2] [0; 2])

let test_list_remove_first4 =
  OUnit.TestCase (list_remove_first ((=) 0) [0; 2; 2] [2; 2])

let test_list_remove_first5 =
  OUnit.TestCase (list_remove_first ((=) 2) [2; 0; 2] [0; 2])

let test_list_remove_first6 =
  OUnit.TestCase (list_remove_first ((=) 1) [2; 0; 2] [2; 0; 2])

let test_list_remove_first7 =
  OUnit.TestCase (list_remove_first ((=) 1) [] [])


let suite_list_remove_first =
  OUnit.TestLabel (
    ("lists_equal"),
    (OUnit.TestList [
       test_list_remove_first1;
       test_list_remove_first2;
       test_list_remove_first3;
       test_list_remove_first4;
       test_list_remove_first5;
       test_list_remove_first6;
       test_list_remove_first7;
     ]
    )
  )






(*** lists_unordered_equal ***)
let lists_unordered_equal ?(success_desired:bool = true) (equal: 'a -> 'a -> bool) (list1: 'a list) (list2: 'a list) =
  fun () ->
    let success =
      Tools.lists_unordered_equal equal list1 list2
    in
      OUnit.assert_bool
	("lists_unordered_equal: ")
	success_desired
	success

let not_lists_unordered_equal equal list1 list2 =
  lists_unordered_equal ~success_desired:false equal list1 list2


let test_lists_unordered_equal1 =
  OUnit.TestCase (lists_unordered_equal (=) [0; 1; 2] [0; 1; 2])

let test_lists_unordered_equal2 =
  OUnit.TestCase (lists_unordered_equal (=) [0; 1; 2] [2; 1; 0])

let test_lists_unordered_equal3 =
  OUnit.TestCase (not_lists_unordered_equal (=) [0; 1; 2] [2; 1; 2])

let test_lists_unordered_equal4 =
  OUnit.TestCase (lists_unordered_equal (=) [0; 0; 1; 1] [0; 0; 1; 1])

let test_lists_unordered_equal5 =
  OUnit.TestCase (not_lists_unordered_equal (=) [0; 0; 1; 1] [0; 1; 1; 1])

let test_lists_unordered_equal6 =
  OUnit.TestCase (not_lists_unordered_equal (=) [0; 1; 1; 1] [0; 0; 0; 1])

let test_lists_unordered_equal7 =
  OUnit.TestCase (not_lists_unordered_equal (=) [0; 1] [0; 1; 1])

let test_lists_unordered_equal8 =
  OUnit.TestCase (not_lists_unordered_equal (=) [0; 1; 1] [0; 1])


let suite_lists_unordered_equal =
  OUnit.TestLabel (
    ("lists_unordered_equal"),
    (OUnit.TestList [
       test_lists_unordered_equal1;
       test_lists_unordered_equal2;
       test_lists_unordered_equal3;
       test_lists_unordered_equal4;
       test_lists_unordered_equal5;
       test_lists_unordered_equal6;
       test_lists_unordered_equal7;
       test_lists_unordered_equal8;
     ]
    )
  )







(*** lists_merge ***)
let lists_merge ?(success_desired:bool = true) (to_string: 'a -> string)
  (equal: 'a -> 'a -> bool) (list1: 'a list) (list2: 'a list)
  (expected: 'a list) =
  fun () ->
    let merged =
      Tools.lists_merge equal list1 list2
    in
    let success =
      Tools.lists_unordered_equal equal merged expected
    in
      OUnit.assert_bool
	("lists_merge: "
	 ^ "[" ^ String.concat "; " (List.map to_string list1) ^ "] and\n"
	 ^ "[" ^ String.concat "; " (List.map to_string list2) ^ "] is\n"
	 ^ "[" ^ String.concat "; " (List.map to_string merged) ^ "] but should be\n"
	 ^ "[" ^ String.concat "; " (List.map to_string expected) ^ "]"
	)
	success_desired
	success
	
let not_lists_merge equal list1 list2 expected =
  lists_merge ~success_desired:false equal list1 list2 expected


let test_lists_merge1 =
  OUnit.TestCase (lists_merge string_of_int (=) [0; 1; 2] [0; 1; 2] [0; 1; 2])

let test_lists_merge2 =
  OUnit.TestCase (lists_merge string_of_int (=) [0; 1; 2] [2; 1; 0] [0; 1; 2])

let test_lists_merge3 =
  OUnit.TestCase (lists_merge string_of_int (=) [0; 1; 2] [2; 1; 2] [0; 1; 2])

let test_lists_merge4 =
  OUnit.TestCase (lists_merge string_of_int (=) [0; 0; 1; 1] [0; 0; 1; 1] [0; 0; 1; 1])

let test_lists_merge5 =
  OUnit.TestCase (lists_merge string_of_int (=) [0; 0; 1; 1] [] [0; 0; 1; 1])

let test_lists_merge6 =
  OUnit.TestCase (lists_merge string_of_int (=) [] [0; 1; 1; 1] [0; 1])

let test_lists_merge7 =
  OUnit.TestCase (lists_merge string_of_int (=) [0; 1] [5] [0; 1; 5])

let test_lists_merge8 =
  OUnit.TestCase (not_lists_merge string_of_int (=) [0] [1; 1] [0; 1; 1])


let suite_lists_merge =
  OUnit.TestLabel (
    ("lists_merge"),
    (OUnit.TestList [
       test_lists_merge1;
       test_lists_merge2;
       test_lists_merge3;
       test_lists_merge4;
       test_lists_merge5;
       test_lists_merge6;
       test_lists_merge7;
       test_lists_merge8;
     ]
    )
  )






(*** do_lists_intersect ***)
let do_lists_intersect ?(success_desired: bool = true) (to_string: 'a -> string)
  (equal: 'a -> 'a -> bool) (list1: 'a list) (list2: 'a list) =
  fun () ->
    let success =
      Tools.do_lists_intersect equal list1 list2
    in
      OUnit.assert_bool
	("do_lists_intersect_equal: "
	 ^ "[" ^ String.concat "; " (List.map to_string list1) ^ "] and\n"
	 ^ "[" ^ String.concat "; " (List.map to_string list2) ^ "]"
	)
	success_desired
	success
	
let not_do_lists_intersect to_string equal list1 list2 expected =
  do_lists_intersect ~success_desired:false to_string equal list1 list2 expected


let do_lists_intersect1 =
  OUnit.TestCase (do_lists_intersect string_of_int (=) [0; 1; 2] [0; 1; 2])

let do_lists_intersect2 =
  OUnit.TestCase (do_lists_intersect string_of_int (=) [0; 1; 2] [1; 1])

let do_lists_intersect3 =
  OUnit.TestCase (not_do_lists_intersect string_of_int (=) [0; 1; 2] [])

let do_lists_intersect4 =
  OUnit.TestCase (not_do_lists_intersect string_of_int (=) [0; 1; 2] [3])


let suite_do_lists_intersect =
  OUnit.TestLabel (
    ("do_lists_intersect"),
    (OUnit.TestList [
       do_lists_intersect1;
       do_lists_intersect2;
       do_lists_intersect3;
       do_lists_intersect4;
     ]
    )
  )





(*** lists_merge ***)
let lists_shared ?(success_desired:bool = true) (to_string: 'a -> string)
  (equal: 'a -> 'a -> bool) (lists: 'a list list) (expected: 'a list) =
  fun () ->
    let shared =
      Tools.lists_shared equal lists
    in
    let success =
      Tools.lists_unordered_equal equal shared expected
    in
      OUnit.assert_bool
	("lists_shared:\n"
	 ^ String.concat "\n"
	    (List.map (fun list -> "[" ^ String.concat "; " (List.map to_string list) ^ "]") lists )
	 ^ " is\n"
	 ^ "[" ^ String.concat "; " (List.map to_string shared) ^ "] but should be\n"
	 ^ "[" ^ String.concat "; " (List.map to_string expected) ^ "]"
	)
	success_desired
	success
	
let not_lists_shared to_string equal lists expected =
  lists_shared ~success_desired:false to_string equal lists expected

let test_lists_shared1 =
  OUnit.TestCase (lists_shared string_of_int (=) [ [0; 1; 2]; [0; 1; 2]] [0; 1; 2])

let test_lists_shared2 =
  OUnit.TestCase (lists_shared string_of_int (=) [ [0; 1; 2]; [0]; [0; 2]] [0; 2])

let test_lists_shared3 =
  OUnit.TestCase (lists_shared string_of_int (=) [ [0; 1]; [0; 2]; [1; 2]] [0; 1; 2])

let test_lists_shared4 =
  OUnit.TestCase (not_lists_shared string_of_int (=) [ [0; 1; 2]; [0]; [0; 2]] [0; 1; 2])


let suite_lists_shared =
  OUnit.TestLabel (
    ("lists_shared"),
    (OUnit.TestList [
       test_lists_shared1;
       test_lists_shared2;
       test_lists_shared3;
       test_lists_shared4;
     ]
    )
  )








(*** mapping_extend ***)
let mapping_extend ?(success_desired:bool = true) 
  (mapping: ('a * 'b) list) (key_equal: 'a -> 'a -> bool) (value_equal: 'b -> 'b -> bool)
  (key: 'a) (value: 'b)  (expected: ('a * 'b) list) =
  fun () ->
    try
      let mapped =
	Tools.mapping_extend mapping key_equal value_equal key value
      in
      let success =
	Tools.lists_unordered_equal (=) mapped expected
      in
	OUnit.assert_bool
	  ("mapping_extend: ")
	  success_desired
	  success
    with
      | Exit ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("mapping_extend: ")
	  end
	
let not_mapping_extend =
  mapping_extend ~success_desired:false



let test_mapping_extend1 =
  OUnit.TestCase (mapping_extend [] (=) (=) 0 0 [(0, 0)])

let test_mapping_extend2 =
  OUnit.TestCase (mapping_extend [(0, 0)] (=) (=) 0 0 [(0, 0)])

let test_mapping_extend3 =
  OUnit.TestCase (mapping_extend [(0, 0)] (=) (=) 1 2 [(0, 0); (1, 2)])

let test_mapping_extend4 =
  OUnit.TestCase (not_mapping_extend [(0, 0)] (=) (=) 1 0 [(0, 0); (1, 0)])

let test_mapping_extend5 =
  OUnit.TestCase (not_mapping_extend [(0, 0)] (=) (=) 0 1 [(0, 0); (0, 1)])

let test_mapping_extend6 =
  OUnit.TestCase (not_mapping_extend [(0, 0)] (=) (=) 1 1 [(0, 0); (1, 0)])



let suite_mapping_extend =
  OUnit.TestLabel (
    ("mapping_extend"),
    (OUnit.TestList [
       test_mapping_extend1;
       test_mapping_extend2;
       test_mapping_extend3;
       test_mapping_extend4;
       test_mapping_extend5;
       test_mapping_extend6;
     ]
    )
  )













(*** array_fold2 ***)
let array_fold2 ?(success_desired:bool = true)
  (fold: 'a -> 'b -> 'c -> 'a) (acc: 'a) (array1: 'b array) (array2: 'c array) (expected: 'c) =
  fun () ->
    try
      let folded =
	Tools.array_fold2 fold acc array1 array2
      in
      let success =
	folded = expected
      in
	OUnit.assert_bool
	  ("array_fold2: ")
	  success_desired
	  success
    with
      | Invalid_argument "Tools.array_fold2" ->
	  if success_desired then begin
	    OUnit.assert_failure
	      ("array_fold2: ")
	  end
	
let not_array_fold2 fold acc array1 array2 expected =
  array_fold2 ~success_desired:false fold acc array1 array2 expected



let test_array_fold21 =
  OUnit.TestCase (array_fold2
		    (fun a b c -> a + b + c)
		    0
		    [| 0; 1; 2 |]
		    [| 0; 1; 2 |]
		    6
		 )

let test_array_fold22 =
  OUnit.TestCase (not_array_fold2
		    (fun a b c -> a + b + c)
		    0
		    [| 0; 1; 2 |]
		    [| 0; 1; 2 |]
		    7
		 )

let test_array_fold23 =
  OUnit.TestCase (not_array_fold2
		    (fun a b c -> a + b + c)
		    0
		    [| 0; 1; 2 |]
		    [| 0; 1 |]
		    6
		 )

let test_array_fold24 =
  OUnit.TestCase (not_array_fold2
		    (fun a b c -> a + b + c)
		    0
		    [| 1; 9; 4; 5; 7; 3 |]
		    [| 0; 6; 6; 2 |]
		    45
		 )

let suite_array_fold2 =
  OUnit.TestLabel (
    ("array_fold2"),
    (OUnit.TestList [
       test_array_fold21;
       test_array_fold22;
       test_array_fold23;
       test_array_fold24;
     ]
    )
  )






(*** run tests ***)
let test_suite =
  OUnit.TestList [
    suite_list_remove_first;
    suite_lists_unordered_equal;
    suite_lists_merge;
    suite_do_lists_intersect;
    suite_lists_shared;
    suite_mapping_extend;
    suite_array_fold2;
  ];;


ignore (OUnit.run_test_tt_main test_suite);;
