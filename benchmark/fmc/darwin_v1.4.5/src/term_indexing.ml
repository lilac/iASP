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


type term = Term.term
type literal = Term.literal

exception ITERATOR_EMPTY

let index_offset = 0
let query_offset = 1


class virtual ['data] data =
object
  method virtual is_equal: 'data -> 'data -> bool
  method virtual to_string: 'data -> string
end
  
class virtual ['data] iterator =
object
  method virtual is_empty: bool
  method virtual next: 'data
end

class type ['data] predicate_index =
object
  method add: ?no_duplicates:bool -> term -> 'data -> unit
  method remove: term -> 'data option -> bool 
  method clear: unit
  method size: int
    
  method iter: (term -> 'data -> unit) -> unit
  method fold : 'a. ('a -> term -> 'data -> 'a) -> 'a -> 'a
  method get_generalization_iterator: p_preserving:bool -> term -> 'data iterator
  method get_shielding_iterator: term -> term -> 'data iterator
    
  method find_all_variants: term -> 'data list
  method find_all_generalizations: p_preserving:bool -> term -> 'data list
  method find_all_unifiable: p_preserving:bool -> term -> 'data list
  method find_all_unifiable_subst: p_preserving:bool -> term -> ('data * Subst.subst) list
  method find_all_instances: p_preserving:bool -> term -> 'data list
  method find_all_shielding: term -> term -> 'data list

  method find: term -> 'data option    
  method find_generalization: p_preserving:bool -> term -> 'data option
  method find_unifiable: p_preserving:bool -> term -> 'data option
  method find_instance: p_preserving:bool -> term -> 'data option
  method find_strongly_shielding: term -> term -> 'data option
  method find_shielding: term -> term -> 'data option
    
  method to_string: string
end





(*** predicate index specialized for propositional atoms. ***)

class ['data] propositional_predicate_index (__data: 'data data) (__term: term) =

object (self)
  (**** values ***)

  val _data = __data
  val _term = __term
  val mutable _entries : 'data list = []
	
  (*** creation ***)

  method clear = _entries <- []

  method size = List.length _entries


  (*** add ***)
  method add ?(no_duplicates: bool = false) (term: term) (data: 'data) : unit =
    (* term must be the same as _term *)
    if Const.debug && not (Term.term_equal term _term) then begin
      failwith "Propositional_indexing.add"
    end;

    (* don't add a duplicate entry *)
    if no_duplicates then begin
      match _entries with
	| [] ->
	    _entries <- data :: _entries;
      
	| _ ->
	    ()
    end

    else begin
      _entries <- data :: _entries;
    end


  (*** traversal ***)
    
  method iter (func: term -> 'data -> unit) : unit =
    List.iter (fun data -> func _term data) _entries
      
  method fold:
    'a. ('a -> term -> 'data -> 'a) -> 'a -> 'a =
    fun func acc ->
      List.fold_left (fun acc data -> func acc _term data) acc _entries

  method remove (_term: term) (data: 'data option) : bool =
    let removed =
      ref false
    in
      _entries <-
	Tools.list_remove_first
	(fun entry ->
	   match data with
	     | Some to_remove when
		 not (_data#is_equal entry to_remove) ->
		 false
		   
	     | _ ->
		 removed := true;
		 true
	)
	_entries;
      !removed



  (*** retrieval ***)

  method private get_one : 'data option =
    match _entries with
      | [] -> None
      | head :: _ -> Some head


  (*** variants ***)

  method find_all_variants (_term: term) : 'data list =
    _entries

  method find (_term: term) : 'data option =
    self#get_one


  (*** generalization ***)

  method find_all_generalizations ~(p_preserving: bool) (_term: term) : 'data list =
    _entries

  method find_generalization ~(p_preserving: bool) (_term: term) : 'data option =  
    self#get_one
      

  (*** unifiable ***)
    
  method find_all_unifiable ~(p_preserving: bool) (_term: term) : 'data list = 
    _entries

  method find_all_unifiable_subst ~(p_preserving: bool) (_term: term) : ('data * Subst.subst) list = 
    List.map (fun x -> (x, Subst.empty)) _entries

  method find_unifiable ~(p_preserving: bool) (_term: term) : 'data option = 
    self#get_one


  (*** instance ***)

  method find_all_instances ~(p_preserving: bool) (_term: term) : 'data list =
    _entries

  method find_instance ~(p_preserving: bool) (_term: term) : 'data option = 
    self#get_one



  (*** productivity ***)

(* productivity doesn't make too much sense with a propositional atom -
   it can never be shielded,
   but if it is in the context, it is also subsumed *)

  method find_strongly_shielding (_producer: term) (_produced: term) : 'data option =
    None

  method find_all_shielding (_producer: term) (_produced: term) : 'data list =
    []


  method find_shielding (_producer: term) (_produced: term) : 'data option =
    None

  method get_generalization_iterator ~(p_preserving: bool) (term: term) : 'data iterator =
    let generalizations =
      self#find_all_generalizations ~p_preserving:p_preserving term
    in
      object (_iterator)
	val mutable _elements = generalizations

	method is_empty =
	  match _elements with
	    | [] -> true
	    | _ -> false

	method next =
	  match _elements with
	    | [] ->
		raise ITERATOR_EMPTY

	    | element :: tail ->
		_elements <- tail;
		element
      end
	

  method get_shielding_iterator (_producer: term) (_produced: term) : 'data iterator =
    object (_iterator)
      method is_empty = true
	
      method next : 'data = raise ITERATOR_EMPTY
    end


  (*** to_string ***)

  method to_string : string =
    Term.term_to_string _term
      
end









(*** set of indexes ***)


module Table = Term.LiteralTypeTable

class ['data] index
  (__data: 'data data)
  (__create_index: 'data data -> 'data predicate_index)
   =
object (self)
  val _data = __data
  val _create_index = __create_index

  val _indexes: ('data predicate_index) Table.t = Table.create 16

  method private create_index (literal: literal) : 'data predicate_index =
    let new_index =
      (* can we get away with a really dumb index? *)
      if Term_attributes.is_propositional literal then
	new propositional_predicate_index __data literal.Term.atom

      else
	__create_index _data
    in
      Table.add _indexes literal new_index;
      new_index
  

  method find (literal: literal) : 'data predicate_index =
    try
      Table.find _indexes literal
    with
      | Not_found ->
	  self#create_index literal

  method iter (func: literal -> 'data predicate_index -> unit) : unit =
    Table.iter
      (fun literal index ->
	 func literal index
      )
      _indexes

  method fold:
    'a. ('a -> literal -> 'data predicate_index -> 'a) -> 'a -> 'a =
    fun func acc ->
    Table.fold
      (fun literal index acc ->
	 func acc literal index
      )
      _indexes
      acc

  method is_empty : bool =
    try
      self#iter
	(fun _ index ->
	   if index#size > 0 then
	     raise Exit;
	);
      true;
    with
      | Exit ->
	  false
end
