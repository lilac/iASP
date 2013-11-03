(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2005, 2006
              The University of Iowa

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)


exception FOF of bool

exception CLAUSIFIER_RESOURCE_OUT

exception NO_SOLUTION of string

exception PARSE_ERROR


(*** Debug ***)

let debug = false

let stable_derivation = false


(*** Version ***)

let version = "Darwin 1.4.4"



(*** Preprocessing Resolution ***)

let resolvent_max_size = 3
let resolvents_max_number = 1000



(*** Partial Context Unifiers ***)

let max_cached_partial_context_unifiers = 50000


(*** Candidates ***)

let max_unprocessed_assert_candidates = 0
let max_assert_candidates = 100000
let max_assert_lookahead = 50000
let max_assert_lookahead_exceeding = 10000

let max_unprocessed_split_candidates = 100000


(*** Clause Utility ***)

let decay_clause_utility_interval = 100
let decay_clause_utility_ratio = 2


(*** Lemmas ***)

let lemma_max_constraints = 10000



(*** Restart with Jumping ***)

let jumping_min_distance = 5

let jumping_check_every_splits = 50

let jumping_time_delta = 0.1


(*** Finite Model Generation ***)

let fd_static_symmetry_reduction = true

let fd_static_symmetry_reduction_unary = fd_static_symmetry_reduction && false

let fd_isomorphism_abstraction = false

let fd_compute_cliques = true

let fd_use_canonicity = fd_static_symmetry_reduction && true

let fd_use_term_definitions = true

let fd_use_definitions = false

let fd_use_diff = true

let fd_constraint_solver = false

let fd_instantiate_axioms = fd_constraint_solver || false

let fd_right_split_parametric = true

(* needs to be true in finite domain mode if fd_right_split_parametric is false *)
let ignore_skolem_literals = (fd_constraint_solver && not fd_right_split_parametric) || false


(*** printing ***)


let print () =
  print_endline ("Constants:");

  let constants = [
    ("debug", (string_of_bool debug));
    ("stable_derivation", (string_of_bool stable_derivation));

    ("resolvent_max_size", (string_of_int resolvent_max_size));
    ("resolvents_max_number", (string_of_int resolvents_max_number));

    ("max_cached_partial_context_unifiers", (string_of_int max_cached_partial_context_unifiers));

    ("max_unprocessed_assert_candidates", (string_of_int max_unprocessed_assert_candidates));
    ("max_assert_candidates", (string_of_int max_assert_candidates));
    ("max_assert_lookahead", (string_of_int max_assert_lookahead));
    ("max_assert_lookahead_exceeding", (string_of_int max_assert_lookahead_exceeding));
    ("max_unprocessed_split_candidates", (string_of_int max_unprocessed_split_candidates));

    ("decay_clause_utility_interval", (string_of_int decay_clause_utility_interval));
    ("decay_clause_utility_ratio", (string_of_int decay_clause_utility_ratio));

    ("lemma_max_constraints", (string_of_int lemma_max_constraints));

    ("fd_static_symmetry_reduction", (string_of_bool fd_static_symmetry_reduction));
    ("fd_static_symmetry_reduction_unary", (string_of_bool fd_static_symmetry_reduction_unary));
    ("fd_isomorphism_abstraction", (string_of_bool fd_isomorphism_abstraction));
    ("fd_compute_cliques", (string_of_bool fd_compute_cliques));
    ("fd_use_canonicity", (string_of_bool fd_use_canonicity));
    ("fd_use_term_definitions", (string_of_bool fd_use_term_definitions));
    ("fd_use_definitions", (string_of_bool fd_use_definitions));
    ("fd_use_diff", (string_of_bool fd_use_diff));
    ("fd_instantiate_axioms", (string_of_bool fd_instantiate_axioms));
    ("fd_constraint_solver", (string_of_bool fd_constraint_solver));
    ("fd_right_split_parametric", (string_of_bool fd_right_split_parametric));

    ("ignore_skolem_literals", (string_of_bool ignore_skolem_literals));
  ]
  in    
    List.iter
      (fun (name, constant) ->
	 Print.print_label name constant;
      )
      constants;

  print_newline ()
