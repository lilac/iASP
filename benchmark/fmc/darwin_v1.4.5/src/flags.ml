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





(*** types ***)


(*** flag id ***)

type flag_id =
  | FL_Resolve
  | FL_Subsume
  | FL_Compact
  | FL_Productivity
  | FL_MixedLiterals
  | FL_FiniteDomain
  | FL_FDFunctionality
  | FL_UniqueNameAssumption
  | FL_Lemma
  | FL_LemmaMin
  | FL_LemmaMax
  | FL_LemmaUIP
  | FL_LemmaParametricAssert
 
  | FL_Equality
  | FL_PlusV
  | FL_Backtracking
  | FL_IterativeDeepening
  | FL_DepthBound
  | FL_MaxDepthBound
  | FL_LookaheadExceeding
  | FL_Restart
  | FL_Jumping
  | FL_NegAssertCandidates

  | FL_EProver
  | FL_PreprocessSplit
  | FL_PreprocessUnit
  | FL_PreprocessPure
  | FL_PreprocessResolution
  | FL_PreprocessEquality
  | FL_PreprocessClausifier

  | FL_InputFormat
  | FL_ZippedSource

  | FL_TimeOutCPU
  | FL_TimeOutWC
  | FL_MemoryLimit

  | FL_PrintLevel
  | FL_PrintConfiguration
  | FL_PrintPreprocessSplit
  | FL_PrintPreprocessUnit
  | FL_PrintPreprocessPure
  | FL_PrintPreprocessResolution
  | FL_PrintPreprocessEquality
  | FL_PrintEqualityAxioms
  | FL_PrintFiniteDomainSorts
  | FL_PrintFiniteDomainTransformation
  | FL_PrintFiniteDomainAxioms
  | FL_PrintFiniteDomainProblem
  | FL_PrintLemmas
  | FL_PrintStatistics
  | FL_PrintModelContext
  | FL_PrintModelContextFile
  | FL_PrintModelDIG
  | FL_PrintModelDIGFile
  | FL_PrintModelARM
  | FL_PrintModelFinite
  | FL_PrintModelTPTP
  | FL_PrintModelTPTPFile
  | FL_PrintDerivationOnline
      (*
  | FL_PrintDerivation
  | FL_PrintDerivationPruned
  | FL_PrintDerivationFile
  | FL_PrintDerivationPrunedFile
  | FL_PrintDerivationDot
  | FL_PrintDerivationDotPruned
      *)
  | FL_PrintDerivationContextUnifier
  | FL_PrintAssertCandidates
  | FL_PrintSplitCandidates
  | FL_PrintConstants

  | FL_Help    
  | FL_Version



 


(*** flags ***)





(* the actual classes *)

(* signatures *)

class type ['a, 'b] flag_type =
object
  method id : flag_id
  method short_name : string
  method long_name : string
  method description : string list
  method value : 'a
  method is_default : bool

  method argument_to_string : 'b -> string
  method value_to_string : 'a -> string
  method opt_to_string : string list
  method signature : string

  method set : 'b -> unit
  method set_opt : 'a -> unit
end

type opt_equality =
  | EQ_None
  | EQ_Axioms

type opt_backtracking =
  | BT_Naive
  | BT_Backjumping

type opt_iterative_deepening =
  | IT_TermDepth
  | IT_TermWeight

type opt_restart =
  | RS_Eager
  | RS_Lazy
  | RS_Delayed

type opt_neg_assert_candidates =
  | NAC_Ignore
  | NAC_Lookahead
  | NAC_Use

type opt_lemma =
  | LM_None
  | LM_Grounded
  | LM_Lifted
  | LM_Propositional

type opt_preprocess_split =
  | PPS_None
  | PPS_Ground
  | PPS_NonGround

type opt_input_format =
  | FI_TME
  | FI_TPTP
  | FI_Darwin

type opt_print_fd_problem =
  | PFD_Silent
  | PFD_Print
  | PFD_Exit

type flag_type_bool = (bool, bool) flag_type
type flag_type_int = (int, int) flag_type
type flag_type_float = (float, float) flag_type
type flag_type_unit = (unit, unit) flag_type
type flag_type_string = (string, string) flag_type
type flag_type_equality = (opt_equality, string) flag_type
type flag_type_backtracking = (opt_backtracking, string) flag_type
type flag_type_iterative_deepening = (opt_iterative_deepening, string) flag_type
type flag_type_restart = (opt_restart, string) flag_type
type flag_type_neg_assert_candidates = (opt_neg_assert_candidates, string) flag_type
type flag_type_lemma = (opt_lemma, string) flag_type
type flag_type_preprocess_split = (opt_preprocess_split, string) flag_type
type flag_type_input_format = (opt_input_format, string) flag_type
type flag_type_print_fd_problem = (opt_print_fd_problem, string) flag_type


(* flag base class
   abstracts over the internal type of the flag (bool, int, string, variant type, ...)
   and the internal type of the flag, i.e. where a string is used instead of a variant type
   and later on mapped to the variant type
*)
class virtual ['a, 'b] flag_base
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: 'a) =

object (self)
  val _id = __id
  val _short_name = __short_name
  val _long_name = __long_name
  val _description = __description
  val mutable _value : 'a = __default
  val mutable _is_default = true

  method id = _id
  method short_name = _short_name
  method long_name = _long_name
  method description = _description
  method value = _value
  method is_default = _is_default

  method as_flag_type = (self :> ('a, 'b) flag_type)

  method set value =
    if self#check value then begin
      self#update value;
      _is_default <- false;
    end
    else
      self#error value

  method set_opt value = _value <- value
  method private virtual update: 'b -> unit
  method virtual check: 'b -> bool
  method virtual argument_to_string: 'b -> string
  method virtual value_to_string: 'a -> string
  method virtual opt_to_string: string list

    
  method signature : string =
    match self#opt_to_string with
      | [] ->
	  (* unit *)
          ""

      | _type :: [] ->
	  (* a type like bool, int, string, ... *)
          "<" ^ _type ^ ">"

      | _ ->
	  (* enumeration *)
          "[" ^ String.concat "|" self#opt_to_string ^ "]"

  method private error (value: 'b) : unit =
    let msg =
      "wrong argument `" ^ self#argument_to_string value ^ "'; option `"
      ^ self#short_name ^ "'/'" ^ self#long_name
      ^ "' expects a value in " ^ self#signature
    in
      raise (Arg.Bad (msg))
end

class flag_bool
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: bool) =

object (_self)
  inherit ['a, 'b] flag_base __id __short_name __long_name __description __default

  method check _value = true
  method private update value = _value <- value
  method argument_to_string value = string_of_bool value
  method value_to_string value = string_of_bool value
  method opt_to_string = ["bool"]
end

class flag_int
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: int) =

object (_self)
  inherit ['a, 'b] flag_base __id __short_name __long_name __description __default

  method check _value = true
  method private update value = _value <- value
  method argument_to_string value = string_of_int value
  method value_to_string value = string_of_int value
  method opt_to_string = ["int"]
end

class flag_float
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: float) =

object (_self)
  inherit ['a, 'b] flag_base __id __short_name __long_name __description __default

  method check _value = true
  method private update value = _value <- value
  method argument_to_string value = string_of_float value
  method value_to_string value = string_of_float value
  method opt_to_string = ["float"]
end

class flag_unit
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list) =

object (_self)
  inherit ['a, 'b] flag_base __id __short_name __long_name __description ()

  method check _value = true
  method private update () = ()
  method argument_to_string _value = ""
  method value_to_string _value = ""
  method opt_to_string = []
end

class flag_string
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: string) =

object (_self)
  inherit ['a, 'b] flag_base __id __short_name __long_name __description __default

  method check _value = true
  method private update value = _value <- value
  method argument_to_string value = value
  method value_to_string value = value
  method opt_to_string = ["string"]
end


class flag_file
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: string) =

object (_self)
  inherit flag_string __id __short_name __long_name __description __default

  method opt_to_string = ["file name"]
end



(* base flag for variant types.
   takes a string as the input value but leaves the internal type unspecified. *)
class virtual ['a] flag_variant
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: 'a) =

object (self)

  method virtual mapping : ('a * string) list

  inherit ['a, 'b] flag_base __id __short_name __long_name __description __default

  method check (value: string) : bool =
    try
      ignore (
	List.find
	  (fun (_, name) ->
	     name = value
	  )
	  self#mapping
	  : ('a * string)
      );
      true
    with
      | Not_found ->
	  false

  method private update (value: string) : unit =
    try
      let (opt, _) =
	List.find
	  (fun (_, name) ->
	     name = value
	  )
	  self#mapping
      in
	_value <- opt;
    with
      | Not_found ->
	  failwith "Flag_variant.update"

  method argument_to_string value = value

  method value_to_string _opt : string =
    let (_, name) =
      List.find
	(fun (opt, _) ->
	   opt = _opt
	)
	self#mapping
    in
      name

  method opt_to_string =
    List.map
      (fun (_opt, name) ->
	 name
      )
      self#mapping
end



class flag_equality
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_equality) =

object (_self)

  method mapping = [
    (EQ_None, "None");
    (EQ_Axioms, "Axioms");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end


class flag_backtracking
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_backtracking) =

object (_self)

  method mapping = [
    (BT_Naive, "Naive");
    (BT_Backjumping, "Backjumping");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end



class flag_iterative_deepening
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_iterative_deepening) =

object (_self)

  method mapping = [
    (IT_TermDepth, "TermDepth");
    (IT_TermWeight, "TermWeight");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end


class flag_restart
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_restart) =

object (_self)

  method mapping = [
    (RS_Eager, "Eager");
    (RS_Lazy, "Lazy");
    (RS_Delayed, "Delayed");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end


class flag_neg_assert_candidates
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_neg_assert_candidates) =

object (_self)

  method mapping = [
    (NAC_Ignore, "Ignore");
    (NAC_Lookahead, "Lookahead");
    (NAC_Use, "Use");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end


class flag_lemma
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_lemma) =

object (_self)

  method mapping = [
    (LM_None, "None");
    (LM_Grounded, "Grounded");
    (LM_Lifted, "Lifted");
    (LM_Propositional, "Propositional");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end

class flag_preprocess_split
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_preprocess_split) =

object (_self)

  method mapping = [
    (PPS_None, "None");
    (PPS_Ground, "Ground");
    (PPS_NonGround, "NonGround");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end

class flag_input_format
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_input_format) =

object (_self)

  method mapping = [
    (FI_TME, "tme");
    (FI_TPTP, "tptp");
    (FI_Darwin, "darwin");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end

class flag_print_fd_problem
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list)
  (__default: opt_print_fd_problem) =

object (_self)

  method mapping = [
    (PFD_Silent, "Silent");
    (PFD_Print, "Print");
    (PFD_Exit, "Exit");
  ]

  inherit ['a] flag_variant __id __short_name __long_name __description __default
end


exception VERSION

let help_short = "short"
let help_long = "long"

class flag_help_short
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list) =

object (_self)
  inherit flag_unit __id __short_name __long_name __description

  method private update _value = raise (Arg.Help help_short)
end

class flag_help_long
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list) =

object (_self)
  inherit flag_unit __id __short_name __long_name __description

  method private update _value = raise (Arg.Help help_long)
end



class flag_version
  (__id: flag_id)
  (__short_name: string) (__long_name: string) (__description: string list) =

object (_self)
  inherit flag_unit __id __short_name __long_name __description

  method private update _value = raise (VERSION)
end




type flag =
  | Bool of flag_bool
  | Int of flag_int
  | Float of flag_float
  | String of flag_string
  | Unit of flag_unit
  | Equality of flag_equality
  | Backtracking of flag_backtracking
  | IterativeDeepening of flag_iterative_deepening
  | Restart of flag_restart
  | HornCandidates of flag_neg_assert_candidates
  | Lemma of flag_lemma
  | PreprocessSplit of flag_preprocess_split
  | InputFormat of flag_input_format
  | PrintFDProblem of flag_print_fd_problem


type flags = {
  flags : flag list;
  mutable problem_file_name : string;
}












(*** flag processing ***)

type 'a apply_to_flag = {
  apply : 'b 'c. ('b, 'c) flag_type -> 'a;
}
(*
let coerce flag_base =
  (flag_base :> ('a, 'b) flag_type)
*)
let process_flag (flag: flag) (apply: 'a apply_to_flag) : 'a =
  match flag with
    | Int                flag_base -> apply.apply flag_base#as_flag_type
    | Float              flag_base -> apply.apply flag_base#as_flag_type
    | Bool               flag_base -> apply.apply flag_base#as_flag_type
    | String             flag_base -> apply.apply flag_base#as_flag_type
    | Unit               flag_base -> apply.apply flag_base#as_flag_type
    | Equality           flag_base -> apply.apply flag_base#as_flag_type
    | Backtracking       flag_base -> apply.apply flag_base#as_flag_type
    | IterativeDeepening flag_base -> apply.apply flag_base#as_flag_type
    | Restart            flag_base -> apply.apply flag_base#as_flag_type
    | HornCandidates     flag_base -> apply.apply flag_base#as_flag_type
    | Lemma              flag_base -> apply.apply flag_base#as_flag_type
    | PreprocessSplit    flag_base -> apply.apply flag_base#as_flag_type
    | InputFormat        flag_base -> apply.apply flag_base#as_flag_type
    | PrintFDProblem     flag_base -> apply.apply flag_base#as_flag_type






(*** flag access ***)


let short_name_of_flag (flag: flag) : string =
  let apply = {
    apply = fun flag -> flag#short_name
  }
  in
    process_flag flag apply

let get_flag (flags: flag list) (id: flag_id) : flag =
  let apply = {
    apply = fun flag -> id = flag#id
  }
  in
    List.find
      (fun flag ->
	 process_flag flag apply
    )
    flags



(* flag access and coercing to signatue *)

let get_flag_bool (flags: flag list) (id: flag_id) : flag_type_bool =
  match get_flag flags id with
    | Bool flag -> (flag :> flag_type_bool)
    | _ -> failwith "Flags.get_flag_bool"
  
let get_flag_int (flags: flag list) (id: flag_id) : flag_type_int =
  match get_flag flags id with
    | Int flag -> (flag :> flag_type_int)
    | _ -> failwith "Flags.get_flag_int"

let get_flag_float (flags: flag list) (id: flag_id) : flag_type_float =
  match get_flag flags id with
    | Float flag -> (flag :> flag_type_float)
    | _ -> failwith "Flags.get_flag_float"

let get_flag_unit (flags: flag list) (id: flag_id) : flag_type_unit =
  match get_flag flags id with
    | Unit flag -> (flag :> flag_type_unit)
    | _ -> failwith "Flags.get_flag_unit"

let get_flag_string (flags: flag list) (id: flag_id) : flag_type_string =
  match get_flag flags id with
    | String flag -> (flag :> flag_type_string)
    | _ -> failwith "Flags.get_string_flag"

let get_flag_equality (flags: flag list) (id: flag_id) : flag_type_equality =
  match get_flag flags id with
    | Equality flag -> (flag :> flag_type_equality)
    | _ -> failwith "Flags.get_flag_equality"

let get_flag_backtracking (flags: flag list) (id: flag_id) : flag_type_backtracking =
  match get_flag flags id with
    | Backtracking flag -> (flag :> flag_type_backtracking)
    | _ -> failwith "Flags.get_flag_backtracking"

let get_flag_iterative_deepening (flags: flag list) (id: flag_id) : flag_type_iterative_deepening =
  match get_flag flags id with
    | IterativeDeepening flag -> (flag :> flag_type_iterative_deepening)
    | _ -> failwith "Flags.get_flag_iterative_deepening"

let get_flag_restart (flags: flag list) (id: flag_id) : flag_type_restart =
  match get_flag flags id with
    | Restart flag -> (flag :> flag_type_restart)
    | _ -> failwith "Flags.get_flag_restart"

let get_flag_neg_assert_candidates (flags: flag list) (id: flag_id) : flag_type_neg_assert_candidates =
  match get_flag flags id with
    | HornCandidates flag -> (flag :> flag_type_neg_assert_candidates)
    | _ -> failwith "Flags.get_flag_neg_assert_candidates"

let get_flag_lemma (flags: flag list) (id: flag_id) : flag_type_lemma =
  match get_flag flags id with
    | Lemma flag -> (flag :> flag_type_lemma)
    | _ -> failwith "Flags.get_flag_lemma"

let get_flag_preprocess_split (flags: flag list) (id: flag_id) : flag_type_preprocess_split =
  match get_flag flags id with
    | PreprocessSplit flag -> (flag :> flag_type_preprocess_split)
    | _ -> failwith "Flags.get_flag_preprocess_split"

let get_flag_input_format (flags: flag list) (id: flag_id) : flag_type_input_format =
  match get_flag flags id with
    | InputFormat flag -> (flag :> flag_type_input_format)
    | _ -> failwith "Flags.get_flag_input_format"

let get_flag_print_fd_problem (flags: flag list) (id: flag_id) : flag_type_print_fd_problem =
  match get_flag flags id with
    | PrintFDProblem flag -> (flag :> flag_type_print_fd_problem)
    | _ -> failwith "Flags.get_flag_print_fd_problem"


(* flag access by name *)

let resolve (flags: flags) = get_flag_bool flags.flags FL_Resolve
let subsume (flags: flags) = get_flag_bool flags.flags FL_Subsume
let compact (flags: flags) = get_flag_bool flags.flags FL_Compact
let productivity (flags: flags) = get_flag_bool flags.flags FL_Productivity
let mixed_literals (flags: flags) = get_flag_bool flags.flags FL_MixedLiterals
let finite_domain (flags: flags) = get_flag_bool flags.flags FL_FiniteDomain
let finite_domain_functionality (flags: flags) = get_flag_bool flags.flags FL_FDFunctionality
let unique_name_assumption (flags: flags) = get_flag_bool flags.flags FL_UniqueNameAssumption
let lemma (flags: flags) = get_flag_lemma flags.flags FL_Lemma
let lemma_min (flags: flags) = get_flag_int flags.flags FL_LemmaMin
let lemma_max (flags: flags) = get_flag_int flags.flags FL_LemmaMax
let lemma_uip (flags: flags) = get_flag_bool flags.flags FL_LemmaUIP
let lemma_parametric_assert (flags: flags) = get_flag_int flags.flags FL_LemmaParametricAssert


let equality (flags: flags) = get_flag_equality flags.flags FL_Equality
let plus_v (flags: flags) = get_flag_bool flags.flags FL_PlusV
let backtracking (flags: flags) = get_flag_backtracking flags.flags FL_Backtracking
let iterative_deepening (flags: flags) = get_flag_iterative_deepening flags.flags FL_IterativeDeepening
let deepening_bound (flags: flags) = get_flag_int flags.flags FL_DepthBound
let max_deepening_bound (flags: flags) = get_flag_int flags.flags FL_MaxDepthBound
let lookahead_exceeding (flags: flags) = get_flag_bool flags.flags FL_LookaheadExceeding
let restart (flags: flags) = get_flag_restart flags.flags FL_Restart
let jumping (flags: flags) = get_flag_bool flags.flags FL_Jumping
let neg_assert_candidates (flags: flags) = get_flag_neg_assert_candidates flags.flags FL_NegAssertCandidates
let iterative_deepening (flags: flags) = get_flag_iterative_deepening flags.flags FL_IterativeDeepening
let eprover (flags: flags) = get_flag_string flags.flags FL_EProver
let preprocess_split (flags: flags) = get_flag_preprocess_split flags.flags FL_PreprocessSplit
let preprocess_unit (flags: flags) = get_flag_bool flags.flags FL_PreprocessUnit
let preprocess_pure (flags: flags) = get_flag_bool flags.flags FL_PreprocessPure
let preprocess_resolution (flags: flags) = get_flag_bool flags.flags FL_PreprocessResolution
let preprocess_equality (flags: flags) = get_flag_bool flags.flags FL_PreprocessEquality
let preprocess_clausifier (flags: flags) = get_flag_bool flags.flags FL_PreprocessClausifier
let input_format (flags: flags) = get_flag_input_format flags.flags FL_InputFormat

let time_out_CPU (flags: flags) = get_flag_float flags.flags FL_TimeOutCPU
let time_out_WC (flags: flags) = get_flag_float flags.flags FL_TimeOutWC
let memory_limit (flags: flags) = get_flag_int flags.flags FL_MemoryLimit

let print_level (flags: flags) = get_flag_int flags.flags FL_PrintLevel
let print_preprocess_split (flags: flags) = get_flag_bool flags.flags FL_PrintPreprocessSplit
let print_preprocess_unit (flags: flags) = get_flag_bool flags.flags FL_PrintPreprocessUnit
let print_preprocess_pure (flags: flags) = get_flag_bool flags.flags FL_PrintPreprocessPure
let print_preprocess_resolution (flags: flags) = get_flag_bool flags.flags FL_PrintPreprocessResolution
let print_preprocess_equality (flags: flags) = get_flag_bool flags.flags FL_PrintPreprocessEquality
let print_configuration (flags: flags) = get_flag_bool flags.flags FL_PrintConfiguration
let print_equality_axioms (flags: flags) = get_flag_bool flags.flags FL_PrintEqualityAxioms
let print_finite_domain_sorts (flags: flags) = get_flag_bool flags.flags FL_PrintFiniteDomainSorts
let print_finite_domain_transformation (flags: flags) = get_flag_bool flags.flags FL_PrintFiniteDomainTransformation
let print_finite_domain_axioms (flags: flags) = get_flag_bool flags.flags FL_PrintFiniteDomainAxioms
let print_finite_domain_problem (flags: flags) = get_flag_print_fd_problem flags.flags FL_PrintFiniteDomainProblem
let print_lemmas (flags: flags) = get_flag_bool flags.flags FL_PrintLemmas
let print_statistics (flags: flags) = get_flag_bool flags.flags FL_PrintStatistics
let print_model_context (flags: flags) = get_flag_bool flags.flags FL_PrintModelContext
let print_model_context_file (flags: flags) = get_flag_string flags.flags FL_PrintModelContextFile
let print_model_DIG (flags: flags) = get_flag_bool flags.flags FL_PrintModelDIG
let print_model_DIG_file (flags: flags) = get_flag_string flags.flags FL_PrintModelDIGFile
let print_model_ARM (flags: flags) = get_flag_bool flags.flags FL_PrintModelARM
let print_model_finite (flags: flags) = get_flag_bool flags.flags FL_PrintModelFinite
let print_model_tptp (flags: flags) = get_flag_bool flags.flags FL_PrintModelTPTP
let print_model_tptp_file (flags: flags) = get_flag_string flags.flags FL_PrintModelTPTPFile

let print_derivation_online (flags: flags) = get_flag_bool flags.flags FL_PrintDerivationOnline
(*
let print_derivation (flags: flags) = get_flag_bool flags.flags FL_PrintDerivation
let print_derivation_pruned (flags: flags) = get_flag_bool flags.flags FL_PrintDerivationPruned
let print_derivation_file (flags: flags) = get_flag_string flags.flags FL_PrintDerivationFile
let print_derivation_pruned_file (flags: flags) = get_flag_string flags.flags FL_PrintDerivationPrunedFile
let print_derivation_dot (flags: flags) = get_flag_string flags.flags FL_PrintDerivationDot
let print_derivation_dot_pruned (flags: flags) = get_flag_string flags.flags FL_PrintDerivationDotPruned
*)
let print_derivation_context_unifier (flags: flags) = get_flag_bool flags.flags FL_PrintDerivationContextUnifier

let print_assert_candidates (flags: flags) = get_flag_bool flags.flags FL_PrintAssertCandidates
let print_split_candidates (flags: flags) = get_flag_bool flags.flags FL_PrintSplitCandidates
let print_constants (flags: flags) = get_flag_bool flags.flags FL_PrintConstants

(* disable zip support *)
let zipped_file_name (flags: flags) =
  if Zip_wrapper.enabled then
    (get_flag_string flags.flags FL_ZippedSource)#value
  else
    ""
      

let problem_file_name (flags: flags) = flags.problem_file_name





(*** another flag type, needs some of the flag access functions above ***)

class flag_verbosity
  (__id: flag_id)
  (__short_name: string) (__long_name: string)
  (__default: int) (__flags: flag list) =

object (self)
  inherit flag_int __id __short_name __long_name [] __default

  val _print_flag_ids = [
  FL_PrintConfiguration;
  FL_PrintPreprocessSplit;
  FL_PrintPreprocessUnit;
  FL_PrintPreprocessPure;
  FL_PrintPreprocessResolution;
  FL_PrintPreprocessEquality;
  FL_PrintEqualityAxioms;
  FL_PrintFiniteDomainSorts;
  FL_PrintFiniteDomainTransformation;
  FL_PrintFiniteDomainAxioms;
  FL_PrintLemmas;
  FL_PrintStatistics;
  FL_PrintModelContext;
  FL_PrintModelDIG;
  FL_PrintModelTPTP;
  FL_PrintDerivationContextUnifier;
  FL_PrintAssertCandidates;
  FL_PrintSplitCandidates;
  ]

  val _levels =
    [|
      ([ ],
       "show the derivation result");
      ([ FL_PrintConfiguration; FL_PrintStatistics ],
       "... the configuration and statistic");
      ([ FL_PrintModelDIG ],
       "... the found model");
      ([ FL_PrintDerivationOnline ],
       "... the derivation steps");
      ([ FL_PrintDerivationContextUnifier ],
       "... the context unifiers underlying the derivation steps");
    |]

  val mutable _flags = __flags

  (* flags do not exists when this object is constructed,
     so they have to be given later on *)
  method set_flags flags = 
    _flags <- flags;
    (* propagate verbosity settings to the new flags *)
    self#update _value


  method flag_ids_for_level level : flag_id list =
    let rec do_flag_ids_for_level i acc =
      if i <= level then
	let (flags, _) =
	  _levels.(i)
	in
	  do_flag_ids_for_level (i + 1) (acc @ flags)
      else
	acc      
    in
      do_flag_ids_for_level 0 []
    
  method description =
    let level_descriptions =
      Array.mapi
	(fun i (flag_ids, description) ->
	   let short_names =
	     List.fold_left
	       (fun acc flag_id ->
		  try
		    short_name_of_flag (get_flag _flags flag_id) :: acc
		  with
		    | Not_found ->
			acc
	       )
	       []
	       flag_ids
	   in
	     "  " ^ string_of_int i ^ " -> "
	     ^ description
	     ^ " (" ^ String.concat "," (List.rev short_names) ^ ")"
	)
	_levels
    in
    let description =
      Array.fold_left
	(fun acc description ->
	   description :: acc
	)
	["Specify the verbosity:"]
	level_descriptions
    in
      List.rev description


  method check (value: int) : bool =
    (value >= 0)
    &&
    (value < Array.length _levels)

  method opt_to_string =
    Array.to_list (
      Array.mapi
        (fun i _ ->
	   string_of_int i
        )
        _levels
    )


  method update (value: int) =
    (* set level *)
    _value <- value;

    (* unset all print flags *)
    List.iter
      (fun id ->
	 try
	   (get_flag_bool _flags id)#set false
	 with
	   | Not_found ->
	       (* option deactivated *)
	       ()
      )
      _print_flag_ids;

    (* set the used print flags *)
    List.iter
      (fun id ->
	 try
	   (get_flag_bool _flags id)#set true
	 with
	   | Not_found ->
	       (* option deactivated *)
	       ()
      )
      (self#flag_ids_for_level value)

  (* check that the initial default is valid *)
  initializer self#set _value
end












(*** flags creation ***)


let create (problem_file_name: string) : flags =
  (* verbosity needs access to the flags -
     which are not created yet.
     so first create and keep it here with type flag_verbosity,
     store it coerced to the basic flag type in flags,
     and after the flag creation update it *)
  let verbosity =
    new flag_verbosity
      FL_PrintLevel
      "-pl"
      "--print-level"
      1
      []
  in

  let flags =
    Bool (
      new flag_bool
	    FL_Resolve
	    "-ar"
	    "--resolve"
	    ["Apply the Resolve inference rule."]
	    true
    )
    ::
    Bool (
      new flag_bool
	    FL_Subsume
	    "-as"
	    "--subsume"
	    ["Apply the Subsume inference rule."]
	    true
    )
    ::
    Bool (
      new flag_bool
	    FL_Compact
	    "-ac"
	    "--compact"
	    ["Apply the Compact inference rule."]
	    true
    )
    ::
    Bool (
      new flag_bool
	    FL_Productivity
	    "-pr"
	    "--productivity"
	    ["Split only on productive remainders."]
	    true
    )
    ::
    Bool (
      new flag_bool
	    FL_MixedLiterals
	    "-ml"
	    "--mixed-literals"
	    ["Context literals may contain both universal and parametric variables."]
	    true
    )
    ::
    Bool (
      new flag_bool
	    FL_FiniteDomain
	    "-fd"
	    "--finite-domain"
	    ["Search for finite domain models (not refutational complete)."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_FDFunctionality
	    "-fdf"
	    "--finite-domain-functionality"
	    ["Add functionality axioms in --finite-domain mode.";
	     "Done by default if --print-model-tptp is true to ensure consistency.";
	    ]
	    true
    )
    ::
    Bool (
      new flag_bool
	    FL_UniqueNameAssumption
	    "-una"
	    "--unique-name-assumption"
	    ["Assumes that all constants are pairwise disequal (only used in finite domain mode)."]
	    false
    )
    ::
    Lemma (
      new flag_lemma
	    FL_Lemma
	    "-lm"
	    "--lemma"
	    ["Computes and uses lemmas, i.e. clauses entailed by the input clause set.";
	     " - 'None'         : compute no lemmas.";
             " - 'Grounded'     : start lemma computation from grounded closing clause.";
             " - 'Lifted'       : start lemma computation from closing clause.";
             " - 'Propositional': use a propositional abstraction of the involved context literals.";
	     "'Lifted' computes more constraining lemmas in general,";
	     "but has a much higher computation overhead.";
	    ]
	    LM_Grounded
    )
    ::
    Int (
      new flag_int
	   FL_LemmaMin
	   "-lmmin"
	   "--lemma-min"
	   ["See --lemma-max.";
	    "Must be > 0 and <= --lemma-max if --lemma-max is > 0, 0 otherwise."]
	   1000
    )
    ::
    Int (
      new flag_int
	   FL_LemmaMax
	   "-lmmax"
	   "--lemma-max"
	   ["The maximum number of lemmas to store.";
	    "If exceeded, the worst lemmas are removed s.t. only --lemma-min lemmas remain.";
	    "If 0 there is no limit on the number of stored lemmas.";
	   ]
	   5000
    )
    ::
    Bool (
      new flag_bool
	    FL_LemmaUIP
	    "-lmuip"
	    "--lemma-uip"
	    ["Stops regression for computing a lemma at the first UIP.";
	    ]
	    false
    )
    ::
    Int (
      new flag_int
	    FL_LemmaParametricAssert
	    "-lmpa"
	    "--lemma-parametric-assert"
	    ["By default Assert of literals containing parameters is not performed.";
	     "If a clause is learned at least n times as a lemma,";
	     "then parametric assert is activated for that clause.";
	     "If 0 then parametric assert is never performed."
	    ]
	    0
    )
    ::
    Bool (
      new flag_bool
	    FL_PlusV
	    "-pv"
	    "--plus-v"
	    ["Use +v instead of -v for the initial interpretation."]
	    false
    )
    ::
    Equality (
      new flag_equality
	   FL_Equality
	   "-eq"
	   "--equality"
	   ["Specifies if the predicate '=' is interpreted as equality";
	    "(e.g. as in (p(a) = p(b)) ), and how equality is handled.";
	    " - 'None'   : ignore equality";
	    " - 'Axioms' : add the standard equality axioms,";
	    "    if the problem contains equality";
	   ]
	   EQ_Axioms
    )
    ::
    Backtracking (
      new flag_backtracking
	    FL_Backtracking
	    "-bt"
	    "--backtracking"
	    ["Specifies the backtracking method.";
	     "Note: For Horn problems naive backtracking is always used";
	     "as no backtracking occurs.";
	    ]
	    BT_Backjumping
    )
    ::
    IterativeDeepening (
      new flag_iterative_deepening
	   FL_IterativeDeepening
	   "-id"
	   "--iterative-deepening"
	   ["Specifies the criterion for the iterative deepening."]
	   IT_TermDepth
    )
    ::
    Int (
      new flag_int
	   FL_DepthBound
	   "-db"
	   "--depth-bound"
	   ["The initial iterative deepening bound."]
	   2
    )
    ::
    Int (
      new flag_int
	   FL_MaxDepthBound
	   "-mdb"
	   "--max-depth-bound"
	   ["The maximum iterative deepening bound to be tried (0 = unlimited)."]
	   0
    )
    ::
    Bool (
      new flag_bool
	    FL_LookaheadExceeding
	    "-lx"
	    "--lookahead-all"
	    ["Apply the Close lookahead to all Assert candidate literals,";
	     "instead of only to the candidates within the current depth bound."]
	    false
    )
    ::

    Restart (
      new flag_restart
	   FL_Restart
	   "-rs"
	   "--restart"
	   ["Specifies how restarting is done when a branch is exhausted";
	    "within the current depth bound.";
	    "If a candidate exceeding the depth bound has been computed:";
	    " - 'Eager' restarts immediately with an increased depth bound.";
	    " - 'Lazy' marks the current choice point as incomplete.";
	    "   When finding an exhausted branch it backtracks to the first choice point";
	    "   where a candidate has been dropped and continues the derivation there.";
	    " - 'Delayed' ignores the candidate for the time being.";
	    "   When finding an exhausted branch all exceeding candidates";
	    "   are recomputed and checked for applicability.";
	    "   For an incomplete branch backtracking is then done like in 'Lazy'";
	    "'Eager' is best for unsatisfiable problems, 'Delayed' for satisfiable ones,";
	    "and 'Lazy' is best in general.";
	   ]
	   RS_Lazy
    )
      (* not effective, and not tested well enough.
    ::
    Bool (
      new flag_bool
	    FL_Jumping
	    "-jp"
	    "--jumping"
   	    ["In order to increase the likelihood of finding a model quickly";
	     "the search space (tree) is not explored 'in order'.";
	     "Instead, from time to time (based on a given CPU timeout)";
	     "a jump to a different position in the search space is done.";
	     "The search remains complete, as the search is eventually continued";
	     "at all such left positions, if no model is found previously."
	    ]
	    false
    ) *)
    ::
    HornCandidates (
      new flag_neg_assert_candidates
	   FL_NegAssertCandidates
	   "-nac"
	   "--neg-assert-candidates"
	   ["Specifies how negative Assert candidates are treated,";
	    "they can be ignored without loss of completeness.";
	    " - 'Ignore' completely ignores negative candidates.";
	    " - 'Lookahead' uses negative candidates only for the Close lookahead.";
	    " - 'Use' uses negative candidates for all purposes.";
	    " 'Ignore' seems to be best for Horn, 'Use' for non-Horn problems."
	   ]
	   NAC_Use
    )
    ::
    String (
      new flag_string
	    FL_EProver
	    "-eprover"
	    "--eprover"
   	    ["Specify path to the eprover (www.eprover.org).";
	     "For a first-order tptp input eprover will be used as the clausifier.";
	     "If not specified, then 'eprover' will be searched for";
	     "in darwin's directory and the path.";
	    ]
	    ""
    )
    ::
    PreprocessSplit (
      new flag_preprocess_split
	   FL_PreprocessSplit
	   "-pps"
	   "--preprocess-split"
	   ["Preprocess input clauses by splitting.";
	    " - 'None' does no splitting.";
	    " - 'Ground' splits variable disjoint clause parts.";
	    " - 'NonGround' splits clause parts by a non-ground connection literal";
	    "    as long as this reduces the number of variables per part.";
	    "    (this implies 'Ground')";
	    "In general 'Ground' performs best."
	   ]
	   PPS_Ground
    )
    ::
    Bool (
      new flag_bool
	   FL_PreprocessUnit
	   "-ppu"
	   "--preprocess-unit"
	   ["Preprocess input clauses by removing clauses (clause literals)";
	    "which are subsumed by (negated) unit clauses, up to fix point.";
	   ]
	   true
    )
    ::
    Bool (
      new flag_bool
	   FL_PreprocessPure
	   "-ppp"
	   "--preprocess-pure"
	   ["Preprocess input clauses by removing clauses containing pure literals."]
	   true
    )
    ::
    Bool (
      new flag_bool
	   FL_PreprocessResolution
	   "-ppr"
	   "--preprocess-resolution"
	   ["Preprocess input clauses by adding resolvents of length <= 3.";
	   ]
	   false
    )
    ::
    Bool (
      new flag_bool
	   FL_PreprocessEquality
	   "-ppe"
	   "--preprocess-equality"
	   ["Preprocess input clauses by simplifying trivial (dis)equalities.";
	   ]
	   true
    )
    ::
    Bool (
      new flag_bool
	   FL_PreprocessClausifier
	   "-ppc"
	   "--preprocess-clausfier"
	   ["The clausifier (eprover) can do some simplification.";
	    (*"This might remove symbols from the input and a generated model."*)
	   ]
	   true
    )
    ::
    InputFormat (
      new flag_input_format
	   FL_InputFormat
	   "-if"
	   "--input-format"
	   ["Specifies the default format used to parse the input.";
	    "This setting is ignored if the format can be deduced from the file extension.";
	   ]
	   FI_TPTP
    )
    ::


    Float (
      new flag_float
	    FL_TimeOutCPU
	    "-to"
	    "--timeout-cpu"
	    ["Terminates the prover after n seconds of used processor time.";
	     "A value less than or equal to 0 disables the timeout."]
	    0.0
    )
    ::
    Float (
      new flag_float
	    FL_TimeOutWC
	    "-tow"
	    "--timeout-wc"
	    ["Terminates the prover after n seconds of used real (wall clock) time.";
	     "A value less than or equal to 0 disables the timeout."]
	    0.0
    )
    ::
    Int (
      new flag_int
	    FL_MemoryLimit
	    "-mem"
	    "--memory-limit"
	    ["Terminates the prover if it occupies more than n MB of RAM.";
	     "A value less than or equal to 0 disables the limit."]
	    0
    )
    ::
    (* printing *)

    Int (verbosity :> flag_int)
    ::
    Bool (
      new flag_bool
	    FL_PrintConfiguration
	    "-pc"
	    "--print-configuration"
	    ["Print the system configuration."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintPreprocessSplit
	    "-ppps"
	    "--print-preprocess-split"
	    ["Print how input clauses are split in preprocessing."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintPreprocessUnit
	    "-pppu"
	    "--print-preprocess-unit"
	    ["Print simplifications done by --preprocess-unit."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintPreprocessPure
	    "-pppp"
	    "--print-preprocess-resolution"
	    ["Print simplifications done by --preprocess-pure."]
	    true
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintPreprocessResolution
	    "-pppr"
	    "--print-preprocess-pure"
	    ["Print simplifications done by --preprocess-resolution."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintPreprocessEquality
	    "-pppe"
	    "--print-preprocess-equality"
	    ["Print simplifications done by --preprocess-equality."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintEqualityAxioms
	    "-peq"
	    "--print-equality-axioms"
	    ["Print the axioms added to an equality problem, if any."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintLemmas
	    "-plm"
	    "--print-lemmas"
	    ["Prints each newly learned lemma, and lemmas kept over a restart.";
	    ]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintFiniteDomainSorts
	    "-pfds"
	    "--print-fd-sorts"
	    ["Prints the sorts inferred from the unsorted first order problem.";
	    ]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintFiniteDomainTransformation
	    "-pfdt"
	    "--print-fd-transformation"
	    ["Prints the transformation of the clause set generated in --finite-domain mode.";
	    ]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintFiniteDomainAxioms
	    "-pfda"
	    "--print-fd-axioms"
	    ["Prints the axiomatization generated in --finite-domain mode.";
	    ]
	    false
    )
    ::
    PrintFDProblem (
      new flag_print_fd_problem
	    FL_PrintFiniteDomainProblem
	    "-pfdp"
	    "--print-fd-problem"
	    ["Prints in--finite-domain mode the problem generated";
	     "for each entered domain size, using tptp cnf format.";
	     " - 'Silent' does nothing.";
	     " - 'Print' prints the problem.";
	     " - 'Exit' prints the problem and exits.";
	     "So, 'Print' can be used in a derivation to print the problem for each size,";
	     "and 'Exit' with --pl 0 to just generate and print the problem.";
	    ]
	    PFD_Silent
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintStatistics
	    "-ps"
	    "--print-statistics"
	    ["Print statistics after the derivation terminates."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintModelContext
	    "-pmc"
	    "--print-context"
	    ["Print a found model in the context format."]
	    false
    )
    ::
    String (
      new flag_file
	    FL_PrintModelContextFile
	    "-pmcf"
	    "--print-context-file"
	    ["Print a found model in the context format to a file."]
	    ""
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintModelDIG
	    "-pmd"
	    "--print-DIG"
	    ["Print a found model in the DIG (Disjunctions of Implicit Generalizations) format."]
	    false
    )
    ::
    String (
      new flag_file
	    FL_PrintModelDIGFile
	    "-pmdf"
	    "--print-DIG-file"
	    ["Print a found model in the DIG format to a file."]
	    ""
    )
    ::
    (* :TODO: *)
    (*
    Bool (
      new flag_bool
	    FL_PrintModelARM
	    "-pmc"
	    "--print-model-ARM"
	    ["Print a found model in the ARM (Atomic Representation of Models) format.";
	    "This is (theoretically) not always possible, then a maximal reduced DIG representation is printed."]
	    false
    )
      ::
    *)
    Bool (
      new flag_bool
	    FL_PrintModelFinite
	    "-pmfd"
	    "--print-model-finite"
	    ["Print the multiplication tables of a finite domain model."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintModelTPTP
	    "-pmtptp"
	    "--print-model-tptp"
	    ["Print a model in TPTP format."]
	    false
    )
    ::
    String (
      new flag_file
	    FL_PrintModelTPTPFile
	    "-pmtptpf"
	    "--print-model-tptp-file"
	    ["Print a found model in the TPTP format to a file."]
	    ""
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintDerivationOnline
	    "-pdo"
	    "--print-derivation-online"
	    ["Print the derivation (Assert, Split, Close, ...) in real-time.";
	    (* "This also includes restarts,";
	     "while all other methods only log the derivation from the last restart on."*)
	    ]
	    false
    )
    ::
(*
    Bool (
      new flag_bool
	    FL_PrintDerivation
	    "-pd"
	    "--print-derivation"
	    ["Print the derivation after it is finished."]
	    false
    )
  ::
    String (
      new flag_file
	   FL_PrintDerivationFile
	   "-pdf"
	   "--print-derivation-file"
	   ["Print the derivation to a file."]
	   ""
    )
  ::

    Bool (
      new flag_bool
	    FL_PrintDerivationPruned
	    "-pdp"
	    "--print-derivation-pruned"
	    ["Print the pruned derivation after it is finished.";
	     "i.e. split decisions without any impact on the derivation are omitted."]
	    false
    )
  ::
    String (
      new flag_file
	   FL_PrintDerivationPrunedFile
	   "-pdpf"
	   "--print-derivation-pruned-file"
	   ["Print the pruned derivation to a file."]
	   ""
    )
  ::
    String (
      new flag_file
	   FL_PrintDerivationDot
	   "-pdot"
	   "--print-derivation-dot"
	   ["Print the derivation in the graphviz dot format to a file."]
	   ""
    )
  ::

    String (
      new flag_file
	   FL_PrintDerivationDotPruned
	   "-pdotp"
	   "--print-derivation-dot-pruned"
	   ["Print the pruned derivation in the graphviz dot format to a file."]
	   ""
    )
  ::
*)
    Bool (
      new flag_bool
	    FL_PrintDerivationContextUnifier
	    "-pdcu"
	    "--print-context-unifier"
	    ["Modifies any derivation output by additionally printing the context unifier";
	     "for each Assert, Split, and Close application."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintAssertCandidates
	    "-pac"
	    "--print-assert-candidates"
	    ["Print each applicable Assert candidate."]
	    false
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintSplitCandidates
	    "-psc"
	    "--print-split-candidates"
	    ["Print each applicable Split candidate."]
	    false
    )
    ::
    Unit ((
      new flag_help_short
	FL_Help
	"-h"
	"--help"
	["Print a short description of the program usage and options."]
	  ) :> flag_unit
    )
    ::
    Unit ((
      new flag_help_long
	FL_Help
	"-help"
	"--more-help"
	["Print a description of the program usage and options."]
	  ) :> flag_unit
    )
    ::
    Unit ((
      new flag_version
	FL_Version
	"-v"
	"--version"
	["Print the program version."]
	  ) :> flag_unit
    )
    ::
    Bool (
      new flag_bool
	    FL_PrintConstants
	    "-pct"
	    "--print-constants"
	    ["Print settings of internal constants."]
	    false
    )
    ::
    
    if Zip_wrapper.enabled then begin
      (* enable zip support*)
      String (
	new flag_file
	  FL_ZippedSource
	  "-zip"
	  "--zipped-source"
	  ["Get the input file from this zipped archive (also jar)."]
	  ""
      )
      ::
      []
    end
    else begin
      []
    end
  in
    verbosity#set_flags flags;

    {
      flags = flags;
      problem_file_name = problem_file_name;
    }












(*** creation of the specification used by the Arg module to evaluate the command line ***)

(* map a flag type to the appropriate setting function *)
let create_spec (flag: flag) : Arg.spec =
  match flag with
    | Int flag ->
	Arg.Int flag#set

    | Float flag ->
	Arg.Float flag#set

    | Bool flag ->
	Arg.Bool flag#set

    | String flag ->
	Arg.String flag#set

    | Unit flag ->
	Arg.Unit flag#set

    | Equality flag ->
	Arg.String flag#set

    | Backtracking flag ->
	Arg.String flag#set

    | IterativeDeepening flag ->
	Arg.String flag#set

    | Restart flag ->
	Arg.String flag#set

    | HornCandidates flag ->
	Arg.String flag#set

    | Lemma flag ->
	Arg.String flag#set

    | PreprocessSplit flag ->
	Arg.String flag#set

    | InputFormat flag ->
	Arg.String flag#set

    | PrintFDProblem flag ->
	Arg.String flag#set

let create_spec_entry (flag: ('a, 'b) flag_type) (name: string) (update: Arg.spec) =
  (name, update, String.concat "\n" flag#description)

(* traverse all flags and create for each a spec bound to its short and long name,
   e.g. to -v and --version *)
let create_spec_list (flags: flag list) : (Arg.key * Arg.spec * Arg.doc) list =
  let create_spec_entry_short update = {
    apply = fun flag -> create_spec_entry flag flag#short_name update
  }
  in
  let create_spec_entry_long update = {
    apply = fun flag -> create_spec_entry flag flag#long_name update
  }
  in
    List.fold_left
      (fun acc flag ->
	 let spec =
	   create_spec flag
	 in
	   process_flag flag (create_spec_entry_short spec)
	   ::
	   process_flag flag (create_spec_entry_long spec)
	   ::
	   acc
      )
      []
      (List.rev flags)








(*** help messages ***)


(* creates the help for one flag *)
let create_help_for_flag_short (flag: ('a, 'b) flag_type) (signature: string) (default: string) : string =
  " " ^ Print.right_pad flag#short_name 8
  ^ " " ^ Print.right_pad flag#long_name 30
  ^ " " ^ Print.right_pad default 15
  ^ " " ^ signature
  ^ "\n"

let create_help_for_flag_long (flag: ('a, 'b) flag_type) (signature: string) (default: string) : string =
  create_help_for_flag_short flag signature default
  ^ String.concat "\n" (List.map (fun line -> String.make 8 ' ' ^ line) flag#description)
  ^ "\n"
  ^ "\n"

(* creates the help for all flags *)
let create_help (flags: flags) (apply: string apply_to_flag) : string =
  let help_header =
    Const.version
    ^ "\n"
    ^ "\n"
    ^ "Usage: darwin [options] file\n"
    ^ "\n"
  in

  let help_flags =
    List.fold_left
      (fun acc flag ->
         acc ^ process_flag flag apply
      )
      ""
      flags.flags
  in
    help_header ^ help_flags




(*** flags creation and setting by evaluation of the command line *)

(* some sanity checks and changes of default settings *)
let sanity_checks (flags: flags) =

  let silent =
    (print_level flags)#value = 0
  in

  if not (finite_domain flags)#value
    &&
    (unique_name_assumption flags)#value
  then begin
    if not silent then
      print_endline ("WARNING: Unique name assumption is only used in finite domain mode.");
    (unique_name_assumption flags)#set_opt false
  end;

  (* finite model finding always starts with depth bound 1 *)
  if (finite_domain flags)#value
    &&
    (deepening_bound flags)#is_default
    &&
    (deepening_bound flags)#value != 1
  then begin
    if not silent then
      print_endline ("Finite Domain: setting initial domain size to 1.");
    (deepening_bound flags)#set_opt 1;
  end;

  (* finite model finding prefers non-ground splitting *)
  if (finite_domain flags)#value
    &&
    (preprocess_split flags)#is_default
    &&
    (preprocess_split flags)#value != PPS_NonGround
  then begin
    if not silent then
      print_endline ("Finite Domain: using non-ground splitting in preprocessing.");
    (preprocess_split flags)#set_opt PPS_NonGround;
  end;

  (* DIG model makes no sense with finite domains. *)
  if (finite_domain flags)#value
    &&
    (print_model_DIG flags)#value
  then begin
    if (print_model_finite flags)#value then begin
      if not silent then
	print_endline ("Finite Domain: will not print DIG model representation.");
      (print_model_DIG flags)#set_opt false;
    end
    else begin
      if not silent then
	print_endline ("Finite Domain: will print multiplication tables instead of DIG representation.");
      (print_model_finite flags)#set_opt true;
      (print_model_DIG flags)#set_opt false;
    end
  end;

  (* multiplication table model makes no sense without finite domains. *)
  if not (finite_domain flags)#value then begin
    if (print_model_finite flags)#value then begin
      (print_model_finite flags)#set_opt false;
      if not silent then
	print_endline ("WARNING: finite model can only be printed in finite domain mode.");
    end;
  end;
  (*
  (* pureness preprocessing not working with finite domains.
    ok, should work now, but only based on predicate symbols,
    not actual terms, like equality. see preprocessing module. *)
  if (finite_domain flags)#value then begin
    if (preprocess_pure flags)#value then begin
      (preprocess_pure flags)#set_opt false;
      if not silent then
	print_endline ("Finite Domain: pureness preprocessing not available in finite domain mode.");
    end;
  end;
  *)
  (* tptp output not working without finite domains. *)
  if not (finite_domain flags)#value then begin
    if (print_model_tptp flags)#value || (print_model_tptp_file flags)#value <> "" then begin
      (print_model_tptp flags)#set_opt false;
      (print_model_tptp_file flags)#set_opt "";
      if not silent then
	print_endline ("Darwin: tptp model can only be printed in finite domain mode.");
    end;
  end;

  (* need functionality axioms for tptp model with finite domains. *)
  if
    (finite_domain flags)#value
    &&
    ((print_model_tptp flags)#value || (print_model_tptp_file flags)#value <> "")
    &&
    not (finite_domain_functionality flags)#value
  then begin
    if (finite_domain_functionality flags)#is_default then begin
      if not silent then
	print_endline ("Finite Domain: adding functionality axioms to make tptp model consistent.");
      (finite_domain_functionality flags)#set_opt true;
    end
    else if not silent then begin
      print_endline ("WARNING: functionality axioms not added, tptp model might be inconsistent");
      print_endline ("         as right-uniqueness of function symbols is not ensured.");
    end
  end;


  (* lemma forgetting: sensible limits *)
  if (lemma_max flags)#value < 0 then begin
    if not silent then
      print_endline ("Lemma Max.: interpretting " ^ string_of_int (lemma_max flags)#value
		    ^ " as 0 (unlimited).");
    (lemma_max flags)#set_opt 0;
  end;
  if (lemma_min flags)#value < 0 then begin
    if not silent then
      print_endline ("Lemma Min.: interpretting " ^ string_of_int (lemma_min flags)#value
		    ^ " as 0.");
    (lemma_min flags)#set_opt 0;
  end;
  if (lemma_min flags)#value > (lemma_max flags)#value then begin
    if not silent then
      print_endline ("Lemma Min.: setting " ^ string_of_int (lemma_min flags)#value
		    ^ " to Lemma Max. value: " ^ string_of_int (lemma_max flags)#value);
    (lemma_min flags)#set_opt (lemma_max flags)#value;
  end;
  
  if not silent then
    print_newline ()



let eval_command_line () : flags =
  let flags =
    create ""
  in
  let spec_list =
    create_spec_list flags.flags
  in
    begin
      try
	Arg.parse_argv
	  Sys.argv
	  spec_list
	  (fun name -> flags.problem_file_name <- name)
	  "";

	if (print_constants flags)#value then begin
	  Const.print ();
	  exit 0;
	end;

	if flags.problem_file_name = "" then begin
	  raise (Arg.Bad ("problem file not specified"));
	end;

	if (print_level flags)#value > 0 then begin
	  print_endline (Const.version);
	  print_newline ();
	end;

	sanity_checks flags;

	flags

      with
	| VERSION ->
	    print_endline Const.version;
	    exit 0

	| Arg.Help help ->
	    if help = help_short then
	      let apply = {
		apply = fun flag ->
		  create_help_for_flag_short flag (flag#signature) (flag#value_to_string flag#value)
	      }
	      in
		print_endline (create_help flags apply)

	    else if help = help_long then
	      let apply = {
		apply = fun flag ->
		  create_help_for_flag_long flag (flag#signature) (flag#value_to_string flag#value)
	      }
	      in
		print_endline (create_help flags apply)
		  
	    else
	      failwith "Flags.Arg.Help";

	    exit 0

	| Arg.Bad error_msg ->
	    (* only print the first line of the error msg,
	       the rest is a flag description *)
	    let first_line =
	      try
		let eol =
		  String.index error_msg '\n'
		in
		  String.sub error_msg 0 eol
	      with
		| Not_found ->
		    error_msg
	    in
	      print_endline first_line;
	      print_newline ();
	      print_endline ("Try darwin -h for further information.");
	      exit 0;
    end
