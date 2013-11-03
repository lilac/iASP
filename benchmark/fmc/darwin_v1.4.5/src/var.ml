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


(*
  type encoding as follows:
  - highest bit: used by ocaml garbage collector
  - second highest bit: if true, then parameter variable
  - third highest bit: if true, then indicator variable
  - remainder: positive integer
*)
type var =
    int

let parameter_bit =
  1 lsl (Sys.word_size - 2)

let indicator_bit =
  1 lsl (Sys.word_size - 3)

let max_number =
  (max_int land (lnot parameter_bit) land (lnot indicator_bit))


let sanity_check (id: int) : unit =
  if Const.debug then begin
    if id > max_number then
      failwith "Var: id too big"
  end;
  ()


let is_parametric (var: var) : bool =
  (var land parameter_bit) != 0

let is_universal (var: var) : bool =
  not (is_parametric var)
(*
let is_indicator (var: var) : bool =
  (var land indicator_bit) != 0
*)


let create_universal (id: int) : var =
  sanity_check id;
  id

let create_parametric (id: int) : var =
  sanity_check id;
  id lor parameter_bit
(*
let create_universal_indicator (id: int) : var =
  sanity_check id;
  id lor indicator_bit

let create_parametric_indicator (id: int) : var =
  sanity_check id;
  (id lor indicator_bit) lor parameter_bit
*)

let clone_renumbered (var: var) (id: int) : var =
  sanity_check id;
  (var land (lnot max_number)) lor id
(*
let clone_as_indicator (var: var) : var = 
  var lor indicator_bit

let clone_as_non_indicator (var: var) : var = 
  var land (lnot indicator_bit)
*)



let equal (var1: var) (var2: var) : bool =
  var1 == var2

let id_of_var (var: var) : int =
  var land max_number

let hash_of_var (var: var) : int =
  var


let compare (x: var) (y: var) : int =
  Tools.compare_int x y


let to_string (var: var) : string =
  let id =
    id_of_var var
  in
  if is_universal var then
(*    if is_indicator var then
	"*_" ^ string_of_int id
    else*)
      "_" ^ string_of_int id
  else
(*    if is_indicator var then
	"*=" ^ string_of_int id
    else*)
	"=" ^ string_of_int id


module VarTable =
  Hashtbl.Make (
    struct
      type t = var
	  
      let equal = equal
      let hash = hash_of_var
    end
  )


(*


(* indicator variables introduce new performance/memory overhead.

   using bools for universal/indicator is
   approx. equal in terms of performance and worse in terms of memory

   using different classes for indicater/non-indicator variables
   is disastrous.
*)
type var =
  | Universal of int
  | Parametric of int
  | Universal_Indicator of int
  | Parametric_Indicator of int



let create_universal (id: int) : var =
  Universal id

let create_parametric (id: int) : var =
  Parametric id

let create_universal_indicator (id: int) : var =
  Universal_Indicator id

let create_parametric_indicator (id: int) : var =
  Parametric_Indicator id

let clone_renumbered (var: var) (id: int) : var = 
  match var with
    | Universal _ ->
	create_universal id

    | Parametric _ ->
	create_parametric id

    | Universal_Indicator _ ->
	create_universal_indicator id

    | Parametric_Indicator _ ->
	create_parametric_indicator id


let clone_as_indicator (var: var) : var = 
  match var with
    | Universal id ->
	create_universal_indicator id

    | Parametric id ->
	create_parametric_indicator id

    | Universal_Indicator _
    | Parametric_Indicator _ ->
	var

let clone_as_non_indicator (var: var) : var = 
  match var with
    | Universal _
    | Parametric _ ->
	var

    | Universal_Indicator id ->
	create_universal id

    | Parametric_Indicator id ->
	create_parametric id


let is_universal (var: var) : bool =
  match var with
    | Universal _
    | Universal_Indicator _ ->
	true

    | Parametric _
    | Parametric_Indicator _ ->
	false
      
let is_parametric (var: var) : bool =
  not (is_universal var)

let is_indicator (var: var) : bool =
  match var with
    | Universal _
    | Parametric _ ->
	false

    | Parametric_Indicator _
    | Universal_Indicator _ ->
	true


let equal (var1: var) (var2: var) : bool =
  match var1, var2 with
    | Universal id1, Universal id2
    | Parametric id1, Parametric id2
    | Universal_Indicator id1, Universal_Indicator id2
    | Parametric_Indicator id1, Parametric_Indicator id2
	when id1 = id2 ->
	true

    | _ ->
	false





let id_of_var (var: var) : int = 
  match var with
    | Universal id
    | Parametric id
    | Parametric_Indicator id
    | Universal_Indicator id ->
	id


let compare (x: var) (y: var) : int =
  let cmp =
    Tools.compare_int (id_of_var x) (id_of_var y)
  in
    if cmp <> 0 then
      cmp
    else
      (* equal id *)
      match x, y with
	  (* equal type *)
	| Universal _, Universal _
	| Parametric _, Parametric _
	| Universal_Indicator _, Universal_Indicator _
	| Parametric_Indicator _, Parametric_Indicator _ ->
	    0

	(* universal is smallest type *)
	| Universal _, _ ->
	    -1
	| _, Universal _ ->
	    1

	(* then parametric *)
	| Parametric _, _ ->
	    -1
	| _, Parametric _ ->
	    1

	(* finally universal indicator *)
	| Universal_Indicator _, _ ->
	    -1
	| _, Universal_Indicator _ ->
	    1



let to_string (var: var) : string = 
  match var with
    | Universal id ->
	"_" ^ string_of_int id

    | Parametric id ->
	"=" ^ string_of_int id

    | Universal_Indicator id ->
	"*_" ^ string_of_int id

    | Parametric_Indicator id ->
	"*=" ^ string_of_int id

*)
