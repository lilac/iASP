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


(** making a context unifier admissible *)

(** {6 Types} *)

type subst = Subst.subst
type raw_context_unifier = Context_unifier.raw_context_unifier

(** {6 Functions} *)

(** [make_admissible config database
    raw_context_unifier subst remainder_flags]
    makes the context unifier [raw_context_unifier] admissible.

    The actual context unifiers ([subst], {!Context_unifier.recompute_unifier})
    and the remainder states of the literals
    ([remainder_flags], {!Context_unifier.compute_remainder_states})
    have to be explicitly given
    (which is just a combination of performance and convenience based
    on the current implementation and usage of [make_admissible]).

    The context unifier is made admissible
    in a way that the remainder literals contain as few parameters as possible,
    while no new remainder literals are introduced.

    Based on {!Config.mixed_literals} the resulting remainder
    may contain mixed literals,
    or literals containing either universal or parametric variables.
*)
val make_admissible: Config.config -> raw_context_unifier -> subst -> bool array -> subst
