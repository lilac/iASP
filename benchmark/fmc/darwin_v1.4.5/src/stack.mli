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


(** imperative stack *)


(** {6 Types} *)

(** thrown if a stack exceeds the system given maximum size. *)
exception OVERFLOW

(** the stack *)
type 'data stack



(** {6 Functions} *)


(** {2 Creation} *)

(** [create null_element ]creates an empty stack.
    null element must be provided. *)
val create: 'data -> 'data stack

(** empties the stack *)
val clear: 'data stack -> unit


(** {2 Status} *)


(** empty stack? *)
val is_empty: 'data stack -> bool

(** the stack size *)
val size: 'data stack -> int



(** {2 Access} *)

(** pushs an element onto the stack.
    @raise OVERFLOW if the stack is full. *)
val push: 'data stack -> 'data -> unit

(** returns the top element of the stack.
    @raise Not_found if the stack is empty. *)
val top: 'data stack -> 'data
  
(** removes and returns the top element of the stack.
    @raise Not_found if the stack is empty. *)
val pop: 'data stack -> 'data

(** removes the top element of the stack.
    @raise Not_found if the stack is empty. *)
val remove_top: 'data stack -> unit




(** {2 Iteration} *)
  
(** visits each stack element in order from bottom to top,
    i.e. the top element is visited last. *)
val iter: ('data -> unit) -> 'data stack -> unit

(** visits each stack element in order from bottom to top,
    i.e. the top element is visited last. *)
val fold: ('acc -> 'data -> 'acc) -> 'acc -> 'data stack -> 'acc

(** [iter_stop apply stop] like [iter],
    but stop the traversal if [stop] is true on a stack element.
    stop is checked before apply. *)
val iter_stop: ('data -> unit) -> ('data -> bool) -> 'data stack -> unit

(** clones the stack. *)  
val map: ('data -> 'data) -> 'data stack -> 'data stack



(** {2 Sorting} *)


(** sorts the stack. *)
val sort: ('data -> 'data -> int) -> 'data stack -> unit

(** [find stack check] finds the element specified by [check].

    [stack] must be sorted according to an ordering.
    [check] delivers the result of this ordering
    for the search for element and any stack element.
    it returns 0, if the stack element is equal to the searchecd element,
    -1, if the stack element is greater,
    +1 otherwise.
*)
val find: 'data stack -> ('data -> int) -> 'data
