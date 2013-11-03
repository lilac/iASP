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


(** computes short resolvents *)


(** {6 Types} *)


type clause = Term.clause

(** these clauses simplify to the empty clause,
    so the clause set is unsatisfiable. *)
exception EMPTY_CLAUSE of (clause * clause)


(** {6 Functions} *)

(** Computes and returns short resolvents for the clause set.

    Performs binary resolution between input clauses.
    Only resolvent of size < {!Const.resolvent_max_size} are kept.
    At most {!Const.resolvents_max_number} resolvents are computed.

    [print] prints computed resolvents.

    @raise EMPTY_CLAUSE if two clauses resolved to the empty clause.
*)
val compute_resolvents: print:bool -> clause list -> clause list
