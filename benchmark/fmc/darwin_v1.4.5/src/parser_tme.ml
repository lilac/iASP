type token =
  | SYMBOL of (string)
  | VARIABLE of (string)
  | LEFT_PARENTHESIS
  | RIGHT_PARENTHESIS
  | LEFT_BRACE
  | RIGHT_BRACE
  | IMPLICATION
  | CLAUSE_END
  | NEGATION
  | TRUE
  | FALSE
  | SEMICOLON
  | COMMA
  | EQUALITY
  | EOI

open Parsing;;
let _ = parse_error;;
# 24 "src/parser_tme.mly"
  
  type var = Var.var
  type symbol = Symbol.symbol
  type term = Term.term
  type literal = Term.literal
  type clause = Term.clause

  (* these are only valid for the current clause
     and have to be invalidated with init_clause for every new clause *)

  (* the variable id counter for the currently read term/clause *)
  let var_id_counter = 
    Counter.create_with 0
      
  (* mapping of the variable names (e.g. "X") of the currently read term/clause
     to variables. *)
  let (var_map: (string * Var.var) list ref) =
    ref []

  (* the literals of the currently read clause *)
  let (literals: literal list ref) =
    ref []


  (* reset everything in order to parse a new term/clause *)
  let init_clause () =
    Counter.set var_id_counter 0;
    var_map := [];
    literals := []


	
  (* gets the variables associated with a string from the variable mapping
     creates a new mapping for a new variable *)
  let get_var (var_name: string) =
    try 
      (* way faster than List.assoc *)
      snd (
	List.find
	  (fun (var_name', _) ->
             var_name = var_name'
	  )
	  !var_map
      )
    with
      | Not_found ->
	  let new_var = 
	    Var.create_universal (Counter.value var_id_counter)
	  in
	    Counter.inc var_id_counter;
	    var_map := (var_name, new_var) :: !var_map;
	    new_var
	      

# 76 "src/parser_tme.ml"
let yytransl_const = [|
  259 (* LEFT_PARENTHESIS *);
  260 (* RIGHT_PARENTHESIS *);
  261 (* LEFT_BRACE *);
  262 (* RIGHT_BRACE *);
  263 (* IMPLICATION *);
  264 (* CLAUSE_END *);
  265 (* NEGATION *);
  266 (* TRUE *);
  267 (* FALSE *);
  268 (* SEMICOLON *);
  269 (* COMMA *);
  270 (* EQUALITY *);
  271 (* EOI *);
    0|]

let yytransl_block = [|
  257 (* SYMBOL *);
  258 (* VARIABLE *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\001\000\001\000\003\000\003\000\004\000\004\000\
\005\000\005\000\005\000\006\000\006\000\007\000\007\000\007\000\
\007\000\007\000\010\000\008\000\008\000\011\000\013\000\009\000\
\009\000\014\000\014\000\014\000\015\000\012\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\002\000\002\000\004\000\
\001\000\003\000\003\000\001\000\003\000\001\000\001\000\002\000\
\001\000\005\000\001\000\001\000\001\000\004\000\004\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\015\000\
\004\000\031\000\000\000\000\000\000\000\017\000\020\000\000\000\
\002\000\032\000\000\000\000\000\030\000\029\000\000\000\000\000\
\026\000\000\000\027\000\016\000\003\000\000\000\007\000\000\000\
\000\000\000\000\001\000\006\000\019\000\000\000\000\000\000\000\
\000\000\000\000\010\000\011\000\000\000\000\000\000\000\024\000\
\008\000\000\000\022\000\018\000\023\000\013\000"

let yydgoto = "\003\000\
\010\000\018\000\019\000\020\000\012\000\041\000\013\000\014\000\
\023\000\038\000\015\000\016\000\025\000\026\000\027\000"

let yysindex = "\047\000\
\001\255\004\255\000\000\000\000\007\255\017\255\000\000\000\000\
\000\000\000\000\244\254\043\255\040\255\000\000\000\000\003\255\
\000\000\000\000\002\255\017\255\000\000\000\000\009\255\022\255\
\000\000\008\255\000\000\000\000\000\000\017\255\000\000\017\255\
\017\255\007\255\000\000\000\000\000\000\007\255\007\255\007\255\
\032\255\045\255\000\000\000\000\055\255\056\255\057\255\000\000\
\000\000\017\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\030\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\255\000\000\000\000\000\000\
\000\000\000\000\000\000\048\255\000\000\000\000\000\000\018\255\
\000\000\025\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\054\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\044\000\064\000\024\000\016\000\250\255\000\000\
\007\000\000\000\000\000\252\255\000\000\000\000\000\000"

let yytablesize = 66
let yytable = "\028\000\
\024\000\004\000\029\000\005\000\004\000\034\000\005\000\021\000\
\022\000\006\000\007\000\008\000\006\000\007\000\008\000\009\000\
\035\000\004\000\017\000\005\000\040\000\028\000\037\000\042\000\
\039\000\006\000\007\000\008\000\025\000\024\000\028\000\028\000\
\030\000\024\000\024\000\024\000\021\000\021\000\025\000\049\000\
\045\000\021\000\021\000\042\000\046\000\047\000\048\000\001\000\
\002\000\030\000\031\000\032\000\033\000\009\000\009\000\043\000\
\044\000\050\000\051\000\052\000\053\000\012\000\005\000\036\000\
\011\000\054\000"

let yycheck = "\006\000\
\005\000\001\001\015\001\003\001\001\001\003\001\003\001\001\001\
\002\001\009\001\010\001\011\001\009\001\010\001\011\001\015\001\
\015\001\001\001\015\001\003\001\013\001\004\001\014\001\030\000\
\003\001\009\001\010\001\011\001\004\001\034\000\013\001\014\001\
\003\001\038\000\039\000\040\000\007\001\008\001\014\001\008\001\
\034\000\012\001\013\001\050\000\038\000\039\000\040\000\001\000\
\002\000\007\001\008\001\012\001\013\001\007\001\008\001\032\000\
\033\000\013\001\004\001\004\001\004\001\008\001\015\001\020\000\
\001\000\050\000"

let yynames_const = "\
  LEFT_PARENTHESIS\000\
  RIGHT_PARENTHESIS\000\
  LEFT_BRACE\000\
  RIGHT_BRACE\000\
  IMPLICATION\000\
  CLAUSE_END\000\
  NEGATION\000\
  TRUE\000\
  FALSE\000\
  SEMICOLON\000\
  COMMA\000\
  EQUALITY\000\
  EOI\000\
  "

let yynames_block = "\
  SYMBOL\000\
  VARIABLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'file) in
    Obj.repr(
# 108 "src/parser_tme.mly"
      ( _1 )
# 196 "src/parser_tme.ml"
               : Term.clause list))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "src/parser_tme.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 202 "src/parser_tme.ml"
               : Term.clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    Obj.repr(
# 115 "src/parser_tme.mly"
      ( _1 )
# 209 "src/parser_tme.ml"
               : Term.clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "src/parser_tme.mly"
      ( [] )
# 215 "src/parser_tme.ml"
               : Term.clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 126 "src/parser_tme.mly"
      ( [_1] )
# 222 "src/parser_tme.ml"
               : 'file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'file) in
    Obj.repr(
# 129 "src/parser_tme.mly"
      ( _1 :: _2 )
# 230 "src/parser_tme.ml"
               : 'file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause_head) in
    Obj.repr(
# 133 "src/parser_tme.mly"
      (
	let clause = 
	  _1
	in
	  init_clause ();
	  clause
      )
# 243 "src/parser_tme.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'clause_head) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'clause_body) in
    Obj.repr(
# 142 "src/parser_tme.mly"
      (
	let clause = 
	  List.append _1 _3
	in
	  init_clause ();
	  clause
      )
# 257 "src/parser_tme.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 152 "src/parser_tme.mly"
      ( (* remove leading false *)
	if Term.literal_equal Term.false_literal _1 then
	  []
	else
	  [_1] 
      )
# 269 "src/parser_tme.ml"
               : 'clause_head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clause_head) in
    Obj.repr(
# 160 "src/parser_tme.mly"
      ( _1 :: _3)
# 277 "src/parser_tme.ml"
               : 'clause_head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clause_head) in
    Obj.repr(
# 163 "src/parser_tme.mly"
      ( _1 :: _3)
# 285 "src/parser_tme.ml"
               : 'clause_head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 167 "src/parser_tme.mly"
      ( (* remove true body *)
	if Term.literal_equal Term.true_literal _1 then
	  []
	else
	  [ Term.request_negated_literal _1 ] 
      )
# 297 "src/parser_tme.ml"
               : 'clause_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clause_body) in
    Obj.repr(
# 175 "src/parser_tme.mly"
      ( Term.request_negated_literal _1 :: _3 )
# 305 "src/parser_tme.ml"
               : 'clause_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "src/parser_tme.mly"
      ( print_endline "!!TRUE"; Term.true_literal )
# 311 "src/parser_tme.ml"
               : 'literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 182 "src/parser_tme.mly"
      ( Term.false_literal )
# 317 "src/parser_tme.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 185 "src/parser_tme.mly"
      (	Term.request_negated_literal _2 )
# 324 "src/parser_tme.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 188 "src/parser_tme.mly"
      ( Term.request_literal true _1 )
# 331 "src/parser_tme.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'sub_term_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'infix) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'sub_term_list) in
    Obj.repr(
# 191 "src/parser_tme.mly"
      ( Term.request_literal true
	  (Term.request_func (Symbol.create_predicate _3 2, Array.of_list (_2 @ _4)))
      )
# 342 "src/parser_tme.ml"
               : 'literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "src/parser_tme.mly"
      ( "=" )
# 348 "src/parser_tme.ml"
               : 'infix))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'top_func) in
    Obj.repr(
# 202 "src/parser_tme.mly"
      ( _1 )
# 355 "src/parser_tme.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 206 "src/parser_tme.mly"
      ( Term.request_const (Symbol.create_predicate _1 0) )
# 362 "src/parser_tme.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'constant) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'sub_term_list) in
    Obj.repr(
# 212 "src/parser_tme.mly"
      ( Term.request_func (Symbol.create_predicate _1 (List.length _3), Array.of_list _3) )
# 370 "src/parser_tme.ml"
               : 'top_func))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'constant) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'sub_term_list) in
    Obj.repr(
# 217 "src/parser_tme.mly"
      ( Term.request_func (Symbol.create_function _1 (List.length _3), Array.of_list _3) )
# 378 "src/parser_tme.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sub_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_term_list) in
    Obj.repr(
# 221 "src/parser_tme.mly"
      ( _1 :: _3 )
# 386 "src/parser_tme.ml"
               : 'sub_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_term) in
    Obj.repr(
# 223 "src/parser_tme.mly"
      ( [_1] )
# 393 "src/parser_tme.ml"
               : 'sub_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 227 "src/parser_tme.mly"
      ( _1 )
# 400 "src/parser_tme.ml"
               : 'sub_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 229 "src/parser_tme.mly"
      ( Term.request_var _1 )
# 407 "src/parser_tme.ml"
               : 'sub_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 231 "src/parser_tme.mly"
      ( Term.request_const (Symbol.create_function _1 0)  )
# 414 "src/parser_tme.ml"
               : 'sub_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 235 "src/parser_tme.mly"
      ( get_var _1 )
# 421 "src/parser_tme.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 239 "src/parser_tme.mly"
      ( _1 )
# 428 "src/parser_tme.ml"
               : 'constant))
(* Entry parse_clause *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry parse_file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse_clause (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Term.clause)
let parse_file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Term.clause list)
;;
