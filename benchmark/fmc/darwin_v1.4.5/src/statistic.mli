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


(** derivation statistic

    manages information about the derivation like
    number of inference rules applied, number of candidates computed, ...
*)


(** {6 Types} *)

type config = Config.config

type statistic


(** {6 Functions} *)


(** {2 Creation} *)

val create: config -> statistic


(** {2 Derivation Rules } *)

val inc_close: statistic -> unit
val get_close: statistic -> int

val inc_assert: statistic -> unit
val get_assert: statistic -> int

val inc_split: statistic -> unit
val get_split: statistic -> int

val inc_resolve: statistic -> unit
val get_resolve: statistic -> int

val inc_subsume: statistic -> unit
val get_subsume: statistic -> int

val inc_compact: statistic -> unit
val get_compact: statistic -> int



(** {2 Misc } *)

val inc_filtered_by_productivity: statistic -> unit
val get_filtered_by_productivity: statistic -> int
(** candidates ignored because of productivity. *)

val inc_computed_assert_candidates: statistic -> unit
val get_computed_assert_candidates: statistic -> int
(** computed assert candidates within the deepening bound. *)

val inc_computed_split_candidates: statistic -> unit
val get_computed_split_candidates: statistic -> int
(** computed split candidates (remainders) within the deeping bound. *)


val inc_jump: statistic -> unit
val get_jump: statistic -> int
(** executed jumps ({!Jumping}). *)


(** {2 Debug} *)

(** for free use during development *)

val inc_debug: statistic -> unit
val get_debug: statistic -> int


val inc_global_debug: unit -> unit
val get_global_debug: unit -> int64 (*int*)
(*val set_global_debug: int -> unit*)

val inc_global_debug2: unit -> unit
val get_global_debug2: unit -> int
val set_global_debug2: int -> unit


(** {2 Representation} *)

(** string representation of the statistics ({!Print.print_label}). *)
val print: statistic -> unit
