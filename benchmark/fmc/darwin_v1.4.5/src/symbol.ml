(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005, 2006
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


exception OVERLOADING


type counter = Counter.counter


type sort =
  | Predicate
  | Function
  | Skolem
  | Connection
  | FD_Relation
  | FD_Size_Marker
  | FD_Element
  | FD_Symbol
  | FD_Permutable
  | FD_Diff

(* a symbol is represented by its id *)
type symbol = {
  name: string;
  pretty: string; (* name for pretty printing *)
  arity: int;
  id: int;
  sort: sort;
}



type symbol_table = {
  (* incrementing id counter for all symbols. *)
  id_counter: counter;

  (* mapping from a symbol's name to the symbol. *)
  symbols: symbol Tools.StringTable.t;
}



(*** global symbol table *)

let global =
  {
    id_counter = Counter.create ();
    symbols = Tools.StringTable.create 128;
  }



(*** decomposition ***)


let name (symbol: symbol) : string =
  symbol.name

let pretty_name (symbol: symbol) : string =
  symbol.pretty

let arity (symbol: symbol) : int =
  symbol.arity

let id (symbol: symbol) : int =
  symbol.id

let sort (symbol: symbol) : sort =
  symbol.sort

(*** representation ***)

let to_string ?(pretty: bool = true) (symbol: symbol) : string =
  if pretty then
    pretty_name symbol
  else
    name symbol

  (*  name_of_symbol symbol ^ "/" ^ string_of_int (arity_of_symbol symbol) *)


let sort_to_string (sort: sort) : string =
  match sort with
    | Predicate -> "Predicate"
    | Function -> "Function"
    | Skolem -> "Skolem"
    | Connection -> "Connection"
    | FD_Relation -> "FD_Relation"
    | FD_Size_Marker -> "FD_Size_Marker"
    | FD_Element -> "FD_Element"
    | FD_Symbol -> "FD_Symbol"
    | FD_Permutable -> "FD_Permutable"
    | FD_Diff -> "FD_Diff"



(*** comparison ***)

let equal (symbol1: symbol) (symbol2: symbol) : bool = 
  if Const.debug then begin
    if
      (symbol1 == symbol2)
	!=
      (symbol1.id = symbol2.id)
    then begin
      print_endline (symbol1.name ^ "/" ^ string_of_int symbol1.arity ^ ": " ^ string_of_int symbol1.id);
      print_endline (symbol2.name ^ "/" ^ string_of_int symbol2.arity ^ ": " ^ string_of_int symbol2.id);
      failwith "Symbol.equal";
    end
  end;

  symbol1 == symbol2


let compare (symbol1: symbol) (symbol2: symbol) : int =
  Tools.compare_int (id symbol1) (id symbol2)

let compare_name (symbol1: symbol) (symbol2: symbol) : int =
  Pervasives.compare (name symbol1) (name symbol2)



module SymbolTable =
  Hashtbl.Make (
    struct
      type t = symbol
	  
      let equal = equal
      let hash = id		
    end
  )




let is_connection (symbol: symbol) : bool =
  symbol.sort == Connection
  
let is_predicate (symbol: symbol) : bool =
  symbol.sort == Predicate || is_connection symbol

let is_function (symbol: symbol) : bool =
  symbol.sort == Function

let is_input (symbol: symbol) : bool =
  symbol.sort == Predicate || symbol.sort = Function

let is_skolem (symbol: symbol) : bool =
  symbol.sort == Skolem

let is_fd_relation (symbol: symbol) : bool =
  symbol.sort == FD_Relation

let is_fd_element (symbol: symbol) : bool =
  symbol.sort == FD_Element

let is_fd_size_marker (symbol: symbol) : bool =
  symbol.sort == FD_Size_Marker

let is_fd_symbol (symbol: symbol) : bool =
  symbol.sort == FD_Symbol

let is_fd_permutable (symbol: symbol) : bool =
  symbol.sort == FD_Permutable




(*** creation ***)

let get_by_name name =
  Tools.StringTable.find global.symbols name

let create_symbol (sort: sort) (name: string) (pretty: string) (arity: int) : symbol =
  try
    (* does the symbol already exist? *)
    let symbol =
      Tools.StringTable.find global.symbols name
    in
      if symbol.arity != arity || symbol.sort != sort then begin
        print_endline (string_of_int symbol.arity);
        print_endline (string_of_int arity);
        print_endline (sort_to_string symbol.sort);
        print_endline (sort_to_string sort);
        print_endline (to_string symbol);
	raise OVERLOADING;
      end;
	(*
      if symbol.arity != arity then begin
	  failwith ("Symbol.create_symbol: " ^ name ^ " to be registered with arity " ^ string_of_int arity
	    ^ " but already registered with arity " ^ string_of_int symbol.arity);
      end;

      if symbol.sort != sort then begin
	  failwith ("Symbol.create_symbol: " ^ name ^ " to be registered with sort " ^ sort_to_string sort
	    ^ " but already registered with sort " ^ sort_to_string symbol.sort);
      end;
	*)
      symbol
  with
    | Not_found ->
	(* no, so create and register it *)
	let id =
	  try
	    Counter.next global.id_counter
	  with
	    | Counter.OVERFLOW ->
		raise (Const.NO_SOLUTION "Symbol.create_symbol: symbol id overflow")
	in
	let symbol = {
	  name = name;
	  pretty = pretty;
	  arity = arity;
	  id = id;
	  sort = sort;
	}
	in
	  Tools.StringTable.add global.symbols name symbol;
	  symbol

let create_predicate (name: string) (arity: int) : symbol =
  create_symbol Predicate name name arity

let create_function (name: string) (arity: int) : symbol =
  create_symbol Function name name arity

(* create a skolem symbol, just use current id as name *)
let create_skolem ?(arity: int = 0) ?(name: string option = None) () : symbol =
  let name =
    match name with
      | None ->
	  "__sko" ^ (string_of_int (Counter.value global.id_counter + 1))

      | Some name->
	  name
  in
    create_symbol Skolem name name arity

(* create a connection symbol, just use current id as name *)
let create_connection (arity: int) : symbol =
  let name =
    "__con" ^ (string_of_int (Counter.value global.id_counter + 1))
  in
    create_symbol Connection name name arity

let get_fd_relation (arity: int) : symbol =
  let name =
    "__fd_r" ^ string_of_int (arity - 2)
  in
  let pretty =
    "r_"
  in
    create_symbol FD_Relation name pretty arity

let get_fd_size_marker (size: int) : symbol =
  let name =
    "__fd_m" ^ string_of_int size
  in
  let pretty =
    "more" ^ string_of_int size
  in
    create_symbol FD_Size_Marker name pretty 0

let get_fd_element (number: int) : symbol =
  let name =
    ("__fd_e" ^ string_of_int number)
  in
  let pretty =
    string_of_int number
  in
    create_symbol FD_Element name pretty 0

let create_fd_symbol (symbol: symbol) : symbol =
  create_symbol FD_Symbol ("__fd_s_" ^ (name symbol)) (name symbol) 0

let create_fd_permutable (sort_id: int) : symbol =
  let name =
    ("__fd_perm_" ^ string_of_int sort_id)
  in
    create_symbol FD_Permutable name name 1

let get_symbol_from_fd_symbol (symbol: symbol) : symbol =
  if is_fd_symbol symbol then begin
    try
      Tools.StringTable.find global.symbols symbol.pretty
    with
      | Not_found ->
	  failwith ("Symbol.get_symbol_from_fd_symbol: unknown: " ^ symbol.name)
  end

  else
    failwith ("Symbol.get_symbol_from_fd_symbol: no fd symbol: " ^ symbol.name)


(*** constants ***)


(* create and register *)
let equality =
  create_symbol Predicate "=" "=" 2

let diff =
  create_symbol FD_Diff "__diff" "diff" 2

(* shouldn't actually be Predicate,
   because then it's taken to be an input symbol.
   but as it is never added to the context this shouldn't be a problem. *)
let lemma_root =
  create_symbol Predicate "__lm_regr_root__" "__lm_regr_root__" 0
