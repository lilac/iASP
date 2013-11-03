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



(** general functions *)


(** {2 Modules} *)


(** a specialized [Hashtbl] with ints as keys. *)
module IntTable : Hashtbl.S with type key = int

(** a specialized [Hashtbl] with strings as keys. *)
module StringTable : Hashtbl.S with type key = string


(** {2 List} *)

(** [list_remove_first predicate list]
   returns [list] without the first element meeting the [predicate].
   returns the original list if no element meets the condition. *)
val list_remove_first: ('a -> bool) -> 'a list -> 'a list

(** [lists_unordered_equal equal list1 list2].
  are [list1] and [list2] identical with respect to [equal]
  if the order of the elements is not taken into account? *)
val lists_unordered_equal: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(** are the two lists equal?
    That is, they are of same length and elements at the same element
    are equal with respect to the given comparison predicate. *)
val lists_equal: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(** merges the two lists.
    assumes that each list itself is free of duplicates. *)
val lists_merge: ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list

(** [list_add equal list element]
  adds [element] to [list] if it is not already contained. *)
val list_add: ('a -> 'a -> bool) -> 'a list -> 'a -> 'a list

(** [lists_sortedmerge_ compare list1 list2]
    merges two lists which are sorted increasingly accordingly to compare,
    returning a sorted list without duplicates. *)
val lists_sorted_merge: ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

(** like [list_add] for a sorted list *)
val list_sorted_add: ('a -> 'a -> int) -> 'a list -> 'a -> 'a list

(** [list_sorted_contains compare list element]
    checks if the increasingly sorted list contains the given element. *)
val list_sorted_contains: ('a -> 'a -> int) -> 'a list -> 'a -> bool

(** returns the first n elements of the list,
    or all, if the lists is shorter than n *)
val list_first: int -> 'a list -> 'a list

(** [lists_shared equal list1 list2].
  do [list1] and [list2] have at least one element in common? *)
val do_lists_intersect: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

(** [lists_shared equal lists]
   finds all elements which are in more than one list. *)
val lists_shared: ('a -> 'a -> bool) -> 'a list list -> 'a list

(** mapping of a list to the same type of list.
    if the list elements are immutable, and elements are mapped to themselves,
    then the tail of the original list can be reused as long as possible.
    not tail-resursive. *)
val list_map: ('a -> 'a) -> 'a list -> 'a list

val list_map_find: ('a -> bool) -> ('a -> 'b) -> ('a list) -> ('b list)

(** [mapping_extend mapping key_equal value_equal key value].
  [mapping] is considered to be a bijective mapping from [key]s to [value]s.
  [mapping_extend] tries to extend [mapping] by the mapping [key] -> [value].
  If [key] -> [value] is already part of [mapping], [mapping] remains unchanged.
  @raise Exit on failure,
  i.e. when there is an x so that [key] -> x or x -> [value]
  are already in [mapping]. *)
val mapping_extend: ('a * 'b) list -> ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 'a -> 'b -> ('a * 'b) list



(** {2 Array} *)

(** [array_iter2 f array1 array2]
  like [List.iter2] for arrays.
  @raise Exit
  if [array1] and [array2] are of different length. *)
val array_iter2: ('a -> 'b -> unit) -> 'a array -> 'b array -> unit

(** [array_fold2 f init array1 array2]
  like [List.fold_left2] for arrays.
  @raise Exit
  if [array1] and [array2] are of different length. *)
val array_fold2: ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a

(** like [List.exists] for an array. *)
val array_exists: ('a -> bool) -> 'a array -> bool

(** like [List.for_all] for an array. *)
val array_for_all: ('a -> bool) -> 'a array -> bool

(** like [List.find] for an array. *)
val array_find: ('a -> bool) -> 'a array -> 'a

(** like [List.for_all2] for an array. *)
val array_for_all2: ('a -> 'b -> bool) -> 'a array -> 'b array -> bool



(** {2 Comparison} *)

(** Specific integer comparison functions to avoid the call
    to the corresponding slower polymorphic functions. *)

(** [Pervasives.compare] specialized for integers. *)
val compare_int: int -> int -> int

(** returns -1 if first list is shorter,
    +1 is second list is shorter,
    0 otherwise *)
val compare_lists: 'a list -> 'b list -> int

(** [Pervasives.max] specialized for integers. *)
val max_int: int -> int -> int

(** finds the maximum list element according to the given comparision function.
    @raise Not_found on empty list. *)
val find_max: ('a -> 'a -> int) -> 'a list -> 'a

(** [lists_sorted_merge] specialized for int lists *)
val lists_sorted_merge_int: int list -> int list -> int list

(** [list_sorted_add] specialized for int lists *)
val list_sorted_add_int: int list -> int -> int list

(** [list_sorted_contains] specialized for int lists *)
val list_sorted_contains_int: int list -> int -> bool
