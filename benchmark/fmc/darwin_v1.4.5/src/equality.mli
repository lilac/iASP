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



(** equality by axiomatization

Symbol.equality is fixed as the equality predicate symbol.
*)


(** {6 Types} *)

type symbol = Symbol.symbol
type clause = Term.clause



(** {6 Functions} *)

(** computes the equality axioms needed for the clause set.

    if [print_axioms] then the axioms are output.
*)
val get_axioms: print_axioms:bool -> Problem.problem -> clause list
