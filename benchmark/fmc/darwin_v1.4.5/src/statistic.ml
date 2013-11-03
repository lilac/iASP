(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005
              The University of Iowa
              Universitaet Koblenz-Landau 

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



(*** types ***)

type counter = Counter.counter
type config = Config.config

(* ids of the tracked attributes *)
type id =
  | ID_CLOSE
  | ID_ASSERT
  | ID_SPLIT
  | ID_RESOLVE
  | ID_SUBSUME
  | ID_COMPACT
  | ID_PRODUCTIVITY
  | ID_ASSERT_CANDIDATES
  | ID_SPLIT_CANDIDATES
  | ID_JUMPS
  | ID_DEBUG



let attributes =
  [
    (ID_CLOSE, "Close");
    (ID_ASSERT, "Assert");
    (ID_SPLIT, "Split");
    (ID_RESOLVE, "Resolve");
    (ID_SUBSUME, "Subsume");
    (ID_COMPACT, "Compact");
    (ID_PRODUCTIVITY, "Productivity Filtered");
    (ID_ASSERT_CANDIDATES, "Assert Candidates");
    (ID_SPLIT_CANDIDATES, "Split Candidates");
    (ID_JUMPS, "Jumps");
    (ID_DEBUG, "Debug");
  ]


let id_to_string id =
  try
    List.assoc id attributes
  with
    | Not_found ->
	failwith "Statistic.id_to_string"


(* for a) convencience and b) efficiency the attributes
   are stored in a Hashtable, therefore each attribute id
   is mapped to a unique int *)
let id_to_int id =
  match id with
    | ID_CLOSE -> 1
    | ID_ASSERT -> 2
    | ID_SPLIT -> 4
    | ID_RESOLVE -> 5
    | ID_SUBSUME -> 6
    | ID_COMPACT -> 7
    | ID_PRODUCTIVITY -> 8
    | ID_ASSERT_CANDIDATES -> 9
    | ID_SPLIT_CANDIDATES -> 10
    | ID_JUMPS -> 11
    | ID_DEBUG -> 12

(*let max_id = 12*)

module Attributes =
  Hashtbl.Make (
    struct
      type t = id
	  
      let equal x y : bool =
	x == y
	  
      let hash x : int =
	id_to_int x
		
    end
  )

type statistic = {
  (* environment *)
  config: config;

  (* the counters for the attributes *)
  attributes: counter Attributes.t;
}





(*** creation ***)

let create (config: config) : statistic =
  (* an attribute is created on its first access *)
  {
    config = config;
    attributes = Attributes.create 16
  }



(*** generic access ***)

let create_attribute (statistic: statistic) (id: id) : counter =
  let counter =
    Counter.create_with 0
  in
    Attributes.add statistic.attributes id counter;
    counter

let get_attribute (statistic: statistic) (id: id) : counter =
  try
    Attributes.find statistic.attributes id
  with
    | Not_found ->
	create_attribute statistic id

let get (statistic: statistic) (id: id) : int =
  Counter.value (get_attribute statistic id)

let set (statistic: statistic) (id: id) (value: int) : unit =
  Counter.set (get_attribute statistic id) value

let inc (statistic: statistic) (id: id) : unit =
  try
    Counter.inc (get_attribute statistic id)
  with
    | Counter.OVERFLOW ->
	print_endline ("Counter overflow: statistic: " ^ id_to_string id);
	set statistic id 0
     




(*** interface access ***)


let inc_close (statistic: statistic) : unit =
  inc statistic ID_CLOSE

let get_close (statistic: statistic) : int =
  get statistic ID_CLOSE


let inc_assert (statistic: statistic) : unit =
  inc statistic ID_ASSERT

let get_assert (statistic: statistic) : int =
  get statistic ID_ASSERT


let inc_split (statistic: statistic) : unit =
  inc statistic ID_SPLIT

let get_split (statistic: statistic) : int =
  get statistic ID_SPLIT


let inc_resolve (statistic: statistic) : unit =
  inc statistic ID_RESOLVE

let get_resolve (statistic: statistic) : int =
  get statistic ID_RESOLVE


let inc_subsume (statistic: statistic) : unit =
  inc statistic ID_SUBSUME

let get_subsume (statistic: statistic) : int =
  get statistic ID_SUBSUME


let inc_compact (statistic: statistic) : unit =
  inc statistic ID_COMPACT

let get_compact (statistic: statistic) : int =
  get statistic ID_COMPACT


let inc_filtered_by_productivity (statistic: statistic) : unit =
  inc statistic ID_PRODUCTIVITY

let get_filtered_by_productivity (statistic: statistic) : int =
  get statistic ID_PRODUCTIVITY


let inc_computed_assert_candidates (statistic: statistic) : unit =
  inc statistic ID_ASSERT_CANDIDATES

let get_computed_assert_candidates (statistic: statistic) : int =
  get statistic ID_ASSERT_CANDIDATES


let inc_computed_split_candidates (statistic: statistic) : unit =
  inc statistic ID_SPLIT_CANDIDATES

let get_computed_split_candidates (statistic: statistic) : int =
  get statistic ID_SPLIT_CANDIDATES

let inc_jump (statistic: statistic) : unit =
  inc statistic ID_JUMPS

let get_jump (statistic: statistic) : int =
  get statistic ID_JUMPS


let inc_debug (statistic: statistic) : unit =
  inc statistic ID_DEBUG

let get_debug (statistic: statistic) : int =
  get statistic ID_DEBUG




(*** global debug values ***)

let global_debug =
  ref (Int64.zero)

let inc_global_debug () =
  global_debug := (Int64.succ !global_debug)


let get_global_debug () =
  !global_debug



let global_debug2 =
  Counter.create_with 0

let inc_global_debug2 () =
  try
    Counter.inc global_debug2
  with
    | Counter.OVERFLOW ->
	print_endline ("Counter overflow: statistic: global debug2");
	Counter.set global_debug2 0
		       
let set_global_debug2 (value: int) =
  try
    Counter.set global_debug2 value
  with
    | Counter.OVERFLOW ->
	print_endline ("Counter overflow: statistic: global debug2");
	Counter.set global_debug2 0


let get_global_debug2 () =
  Counter.value global_debug2




(*** print ***)

let print (statistic: statistic) : unit =
  print_endline ("Statistics:");
    List.iter
      (fun (id, label) ->
(*	 if id <> ID_DEBUG then*)
	   Print.print_statistic label (get statistic id)
      )
      attributes;
    Print.print_statistic_64 "Global Debug" (!global_debug);
    Print.print_statistic "Global Debug2" (Counter.value global_debug2)
