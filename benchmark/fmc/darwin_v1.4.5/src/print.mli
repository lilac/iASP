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


(** common print functions and constants used in ouput *)

(** {6 Constants} *)


(** the length of lables used in printing of configuration and statistic entries. *)
val label_width: int


(** {6 Functions} *)

(** [left_pad string width] ensures that [string] has at least [width]
  by adding whitespace on its left side. *)
val left_pad: string -> int -> string

(** analog to [left_pad]. *)
val right_pad: string -> int -> string

(** formatted printing of a label and its value. *)
val print_label: string -> string -> unit

(** formatted printing of an integer {!Statistic} entry and its value. *)
val print_statistic: string -> int -> unit

(** formatted printing of an integer {!Statistic} entry and its value. *)
val print_statistic_64: string -> int64 -> unit

(** formatted printing of a float {!Statistic} entry and its value. *)
val print_statistic_float: string -> float -> unit

(** outputs a string to the output channel followed by a new line. *)
val output_line: out_channel -> string -> unit
