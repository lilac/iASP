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


(** heap structures *)


(** thrown if a heap exceeds its maximum size. *)
exception OVERFLOW



module type OrderedType = 
  sig
    type t

    val compare: t -> t -> int

    val to_string: t -> string
  end
(** Input signature of the heap functors.
    
    It is acceptable if different [t]s are considered to be equal by compare.
    This is not the case for e.g. the [Set] data structure of the OCaml library.
*)



module Heap (Ord: OrderedType) : sig
  type data = Ord.t

  (** the heap *)
  type t


  (** creates an empty heap. null element must be provided. *)
  val create: data -> t


  (** adds an element.
    @raise OVERFLOW if the heap is full. *)
  val add: t -> data -> unit


  (** returns the minimum element.
     @raise Not_found if the heap is empty. *)
  val min: t -> data

  (** removes the minimum element and returns it.
    @raise Not_found if the heap is empty. *)
  val remove_min: t -> data

  (** iterates over all elements (in unsorted order). *)
  val iter: (data -> unit) -> t -> unit

  (** no elements? *)
  val is_empty: t -> bool

  (** number of elements. *)
  val size: t -> int
end
(**
   The classic imperative array based heap data structure.
   Adding elements and removing the minimum element
   have logarithmic complexity.
   
   The heap can take up to [Sys.max_array_length] elements.
*)





module MinMaxHeap (Ord: OrderedType) : sig
  type data = Ord.t

  (** the heap *)
  type t

  (** creates an empty heap. null element must be provided. *)
  val create: data -> t


  (** adds an element.
    @raise OVERFLOW if the heap is full. *)
  val add: t -> data -> unit


  (** returns the minimum element.
     @raise Not_found if the heap is empty. *)
  val min: t -> data

  (** returns the maximum element.
     @raise Not_found if the heap is empty. *)
  val max: t -> data

  (** removes the minimum element and returns it.
    @raise Not_found if the heap is empty. *)
  val remove_min: t -> data

  (** removes the maximum element and returns it.
    @raise Not_found if the heap is empty. *)
  val remove_max: t -> data

  (** iterates over all elements (in unsorted order). *)
  val iter: (data -> unit) -> t -> unit

  (** no elements? *)
  val is_empty: t -> bool

  (** number of elements. *)
  val size: t -> int

  (** string representation of the heap as an array *)
  val to_string: t -> string
end
(**
   An imperative array based min-max heap data structure.
   Adding elements and removing the minimum and maximum element
   have logarithmic complexity.
   
   See:
   Atkinson, Sack, Santoro, Strothotte
   Mix-Max Heaps and Generalized Priority Queues.
   Communications of the ACM
   1986, October 26, Volume 29, Number 10, p.996-1000
   
   The heap can take up to [Sys.max_array_length] elements.
*)
