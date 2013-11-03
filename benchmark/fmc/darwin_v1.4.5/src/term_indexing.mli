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


(** term indexing *)

type term = Term.term
type literal = Term.literal

(** raised by {!Term_indexing.iterator.next} if no element is left. *)
exception ITERATOR_EMPTY


(** {6 Constants} *)

(** the offset of an index literal. *)
val index_offset: int

(** the offset of a query literal. *)
val query_offset: int



(** {6 Classes} *)


(** type of data stored in {!Term_indexing.index}. *)
class virtual ['data] data:
object
  method virtual is_equal: 'data -> 'data -> bool
  method virtual to_string: 'data -> string
end
  

(** iterator type over the {!Term_indexing.index}. *)
class virtual ['data] iterator:
object
  (** are more elements left? *)
  method virtual is_empty: bool

  (** advances to and returns the next element.
      @raise Exit if no more elements exist. *)
  method virtual next: 'data
end


(** The interface for a predicate term indexing module.

    To be used for exactly one predicate/function symbol,
    not for several at once.
    Thus adding f(x) and g(x) is not allowed.
    
    Insertion of variants of already inserted terms is allowed
    and must keep the data attached to all variants intact.
    For an explanation for {e p_preserving} and {e i_preserving} see {!Subst}.
*)
class type ['data] predicate_index =
object
  (** {2 Access} *)


  (** add [term] with attached ['data] to the index.

      if [no_duplicates] (false by default) then each term may be stored only once,
      i.e. if a term is added more than once
      only the first entry is stored with its data,
      the others are silently dropped.
  *)
  method add: ?no_duplicates:bool -> term -> 'data -> unit

  (** removes the entry associated with [term].
      if [data] is given,
      then additionally [data] must be attached the removed variant.

      returns false if no such entry exists.
  *)
  method remove: term -> 'data option -> bool 
    
  (** removes all entries. *)
  method clear: unit
    
  (** the number of elements stored in the index. *)
  method size: int
    

  (** {2 Traversal} *)

  (** like [List.iter].
      function style iterator over all term/data pairs stored in the index. *)
  method iter: (term -> 'data -> unit) -> unit
    
  (** like [List.fold]. *)
  method fold: 'a. ('a -> term -> 'data -> 'a) -> 'a -> 'a

  (** object style iterator over all terms (i.e. the attached data)
      which are generalizations of the [term]. *)
  method get_generalization_iterator: p_preserving:bool -> term -> 'data iterator

  (** object style iterator over all terms (i.e. the attached data)
      which shield the first term from the second term. *)
  method get_shielding_iterator: term -> term -> 'data iterator



  (** {2 Find All} *)
    
  (** returns the data attached to all
      (universal- and parameter-preserving) variant terms of [term]
      (used for testing only). *)
  method find_all_variants: term -> 'data list

  (** returns the data attached to all terms more general than [term]. *)
  method find_all_generalizations: p_preserving:bool -> term -> 'data list

  (** returns the data attached to all terms unifiable with [term]. *)
  method find_all_unifiable: p_preserving:bool -> term -> 'data list
  method find_all_unifiable_subst: p_preserving:bool -> term -> ('data * Subst.subst) list

  (** returns the data attached to all terms which are instances of [term]. *)
  method find_all_instances: p_preserving:bool -> term -> 'data list

  (** returns the data attached to all terms which strongly shield
      the first from the second term
      (see {!Term_indexing.predicate_index.find_strongly_shielding}).
      Note: if the second term is part of the index is counts
      as a shielding term, while the first term does not.
  *)
  method find_all_shielding: term -> term -> 'data list


  (** {2 Find First} *)

  (** returns the data attached to [term]. *)
  method find: term -> 'data option

  (** returns the data attached to a term more general than [term]. *)
  method find_generalization: p_preserving:bool -> term -> 'data option

  (** returns the data attached to a term unifiable with [term]. *)
  method find_unifiable: p_preserving:bool -> term -> 'data option

  (** returns the data attached to a term which is an instance of [term]. *)
  method find_instance: p_preserving:bool -> term -> 'data option

  (** [find_strongly_shielding term1 term2]
      
      First part of the productivity check (see the Model Evolution paper for details).

      returns None, if term2 is not strongly shielded from term1
      by the index,
      i.e. there is no true instance of term2 which is more general than term1.
      
      otherwise, returns a shielding entry.

      Note: if the second term is part of the index it counts
      as a shielding term, while the first term does not.
  *)
  method find_strongly_shielding: term -> term -> 'data option

  (** [find_shielding index term1 term2]
      Second part of the productivity check.
      
      returns all index elements that shield term2 from term1,
      i.e. which have a p-preserving instance
      which is also a true instance of term2 and more general than term1.

      Note: if the second term is part of the index it counts
      as a shielding term, while the first term does not.
  *)
  method find_shielding: term -> term -> 'data option
    


  (** {2 Representation} *)

  (** string representation of the index. *)
  method to_string: string
end



(** a wrapper for {!Term_indexing.predicate_index} over different predicates.

    supposed to be inherited by a specific term index (see e.g. {!Discrimination_tree}).
    for propositional atoms a specialized index is used in any case.

    expects the same arguments as [predicate_index],
    plus the constructor of the actual [predicate_index] type to use.
*)
class ['data] index:
  'data data ->
  ('data data -> 'data predicate_index) ->
  object
    method find: literal -> 'data predicate_index
    method iter: (literal -> 'data predicate_index -> unit) -> unit
    method fold: 'a. ('a -> literal -> 'data predicate_index -> 'a) -> 'a -> 'a
    method is_empty: bool
  end
