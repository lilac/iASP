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


type symbol = Symbol.symbol
type term = Term.term
type literal = Term.literal
type clause = Term.clause

type arities =
    (int * (symbol list)) list



(* only to be called on a symbol not contained in arities *)
let update_arities (arities: arities) (symbol : symbol) : arities =
  let symbol_arity =
    Symbol.arity symbol
  in
  let rec get_arities' acc =
    match acc with
      | [] ->
	  [ (symbol_arity, [symbol]) ]
	    
      | ((arity, symbols) as head) :: tail ->
	  (* add to other symbols of same arity *)
	  if arity == symbol_arity then
	    (arity, (symbol :: symbols)) :: tail
	      
	  (* ordered - so first symbol of this arity *)
	  else if arity > symbol_arity then
	    (symbol_arity, (symbol :: [])) :: acc
	      
	  (* continue search *)
	  else
	    head :: get_arities' tail		 
  in
    get_arities' arities


let rec get_arity (arities: arities) (arity: int) : symbol list =
  match arities with
    | [] ->
	[]

    | (arity', symbols) :: tail ->
	if arity' == arity then
	  symbols

	else if arity' > arity then
	  []

	else
	  get_arity tail arity


class type problem =
object
  method addClauses: clause list -> unit
  method addClause: clause -> unit

  method addToInitialInterpretation: literal list -> unit

  method getClauses: clause list
  method getInitialInterpretation: literal list

  method getPredicateSymbols: symbol list
  method getFunctionSymbols: symbol list
  method getConstantSymbols: symbol list

  method getPredicateArities: arities
  method getFunctionArities: arities

  method getAllPredicateSymbols: symbol list
  method getAllFunctionSymbols: symbol list
  method getAllConstantSymbols: symbol list

  method getAllPredicateArities: arities
  method getAllFunctionArities: arities

  method getMaxClauseLength: int

  method containsEquality: bool
  method isHorn: bool
  method isBS: bool
end


module SymbolTable = Symbol.SymbolTable


class i_problem ~(equality: bool) ~(horn: bool)
  (_clauses: clause list) (_simplified: clause list list) (_literals: literal list) =
object (self)

  initializer
    self#addClauses _clauses;
    self#addToInitialInterpretation _literals;

  val all : (problem Lazy.t) option =
    match _simplified with
      | [] ->
	  None
      | _  ->
	  let p =
	    lazy (new i_problem ~equality:equality ~horn:horn
		    ((List.concat _simplified) @ _clauses) [] _literals :> problem)
	  in
	    Some p

  val known_symbols : unit SymbolTable.t =
    SymbolTable.create 32

  val mutable clauses : clause list = []
  val mutable initial_interpretation : literal list = []

  val mutable predicate_symbols : symbol list = []
  val mutable function_symbols : symbol list = []
  val mutable constant_symbols : symbol list = []

  val mutable predicate_arities : arities = []
  val mutable function_arities : arities = []

  val mutable all_predicate_symbols : symbol list = []
  val mutable all_function_symbols : symbol list = []
  val mutable all_constant_symbols : symbol list = []

  val mutable all_predicate_arities : arities = []
  val mutable all_function_arities : arities = []

  val mutable max_clause_length : int = 0

  val mutable contains_equality : bool = equality
  val mutable is_Horn : bool = horn
  val mutable is_BS : bool = true


  method private getAll : problem =
    match all with
      | None         -> (self :> problem)
      | Some problem -> Lazy.force problem

  method getClauses : clause list = clauses
  method getInitialInterpretation : literal list = initial_interpretation

  method getPredicateSymbols : symbol list = predicate_symbols
  method getFunctionSymbols : symbol list = function_symbols
  method getConstantSymbols : symbol list = constant_symbols

  method getPredicateArities : arities = predicate_arities
  method getFunctionArities : arities = function_arities

  method getAllPredicateSymbols : symbol list = self#getAll#getPredicateSymbols
  method getAllFunctionSymbols : symbol list = self#getAll#getFunctionSymbols
  method getAllConstantSymbols : symbol list = self#getAll#getConstantSymbols

  method getAllPredicateArities : arities = self#getAll#getPredicateArities
  method getAllFunctionArities : arities = self#getAll#getFunctionArities

  method getMaxClauseLength : int = max_clause_length

  method containsEquality = contains_equality
  method isHorn = is_Horn
  method isBS = is_BS


  method addClauses (clauses: clause list) : unit =
    List.iter self#addClause (List.rev clauses)

  method addClause (clause: clause) : unit =
    clauses <- clause :: clauses;
    max_clause_length <- Tools.max_int max_clause_length (List.length clause);
    is_BS <- is_BS && Term.is_BS clause;
    is_Horn <- is_Horn && Term.is_Horn clause;
    List.iter self#processLiteral clause


  method addToInitialInterpretation (literals: literal list) : unit =
    initial_interpretation <- literals @ initial_interpretation;
    List.iter self#processLiteral (List.rev literals)
    
    

  method processLiteral (literal: literal) : unit =
    match literal.Term.atom with
      | Term.Var _ ->
	  failwith "Problem.addSymbol: Literal consists only of a variable."

      | Term.Const symbol ->
	  self#addPredicateSymbol symbol;

      | Term.Func func ->
	  (* filter the equality symbol *)
	  if Symbol.equal Symbol.equality func.Term.symbol then
	    contains_equality <- true
	  else
	    self#addPredicateSymbol func.Term.symbol;

	  Array.iter self#processTerm func.Term.subterms
	  
  method processTerm (term: term) : unit =
    match term with
      | Term.Var _ ->
	  ()

      | Term.Const symbol ->
	  self#addConstantSymbol symbol;
	    
      | Term.Func func ->
	  self#addFunctionSymbol func.Term.symbol;
	  Array.iter self#processTerm func.Term.subterms


  method addPredicateSymbol (symbol: symbol) : unit =
    if not (SymbolTable.mem known_symbols symbol) then begin
      SymbolTable.add known_symbols symbol ();
      predicate_symbols <- symbol :: predicate_symbols;
      predicate_arities <- update_arities predicate_arities symbol
    end;

  method addFunctionSymbol (symbol: symbol) : unit =
    if not (SymbolTable.mem known_symbols symbol) then begin
      SymbolTable.add known_symbols symbol ();
      function_symbols <- symbol :: function_symbols;
      function_arities <- update_arities function_arities symbol
    end;

  method addConstantSymbol (symbol: symbol) : unit =
    if not (SymbolTable.mem known_symbols symbol) then begin
      SymbolTable.add known_symbols symbol ();
      constant_symbols <- symbol :: constant_symbols;
    end

end




let create ?(equality: bool = false) ?(horn: bool = true)
    (clauseSet: clause list) (simplified: clause list list)
    (initialInterpretation: literal list) =
  (new i_problem ~equality:equality ~horn:horn clauseSet simplified initialInterpretation :> problem)

