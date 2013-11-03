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


(** int counter

  a counter for non-negative numbers. *)

(** {6 Types} *)

type counter

(** raised if the counter is exhausted. *)
exception OVERFLOW

(** {6 Functions} *)

(** creates a fresh counter initialized with 0. *)
val create: unit -> counter

(** [create_with start_value]
  creates a new counter with [start_value]. [start_value] must be >= 0. *)
val create_with: int -> counter

(** [set value]
  sets the counter to [value]. [value] must be >= 0. *)
val set: counter -> int -> unit

(** increments the counter by 1.
  @raise OVERFLOW on overflow. *)
val inc: counter -> unit

(** increments the counter by the given positive value.
  @raise OVERFLOW on overflow. *)
val inc_by: counter -> int -> unit

(** decrements the counter by 1.
  @raise OVERFLOW if the counter drops below 0. *)
val dec: counter -> unit

(** increments the counter by 1 and returns its new value.
    Note: this implies that 0 is never returned as a counter value.

    @raise OVERFLOW on overflow.
*)
val next: counter -> int

(** returns the current value of the counter. *)
val value: counter -> int
