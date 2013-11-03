(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006
              The University of Iowa

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

(** simplifications based on equality *)


(** {6 Types} *)


type clause = Term.clause

(** this clause is simplified to the empty clause,
    so the clause set is unsatisfiable. *)
exception EMPTY_CLAUSE of clause


(** {6 Functions} *)

(** performs trivial simplificiations based on equality.

    returns the simplified clauses,
    and the clauses and literals removed from the original clause set.
    
    simplifies disequalities of the form
    - -(x = t) \/ C -> C{x -> t} where t a term or variable and x not in t

    - -(t = t) \/ C -> C where t a term or variable

    @raise EMPTY_CLAUSE if a clause is simplified to empty clause.
*)
val simplify: print:bool -> clause list -> clause list * clause list

