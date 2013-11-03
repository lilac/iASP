type token =
  | SYMBOL of (string)
  | VARIABLE of (string)
  | LEFT_PARENTHESIS
  | RIGHT_PARENTHESIS
  | LEFT_BRACE
  | RIGHT_BRACE
  | NEGATIVE
  | POSITIVE
  | TERM_SEP
  | EOI

open Parsing;;
let _ = parse_error;;
# 24 "src/parser_darwin.mly"
  
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

      
  (* reset everything in order to parse a new term/clause/file *)
  let init_file () =
    init_clause ()

	
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
	      if (String.sub var_name 0 1) <> "=" then
		Var.create_universal (Counter.value var_id_counter)
	      else
		Var.create_parametric (Counter.value var_id_counter)
	  in
	    Counter.inc var_id_counter;
	    var_map := (var_name, new_var) :: !var_map;
	    new_var
      
      
		
# 83 "src/parser_darwin.ml"
let yytransl_const = [|
  259 (* LEFT_PARENTHESIS *);
  260 (* RIGHT_PARENTHESIS *);
  261 (* LEFT_BRACE *);
  262 (* RIGHT_BRACE *);
  263 (* NEGATIVE *);
  264 (* POSITIVE *);
  265 (* TERM_SEP *);
  266 (* EOI *);
    0|]

let yytransl_block = [|
  257 (* SYMBOL *);
  258 (* VARIABLE *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\005\000\005\000\006\000\006\000\011\000\011\000\010\000\012\000\
\012\000\009\000\009\000\009\000\008\000\008\000\013\000\016\000\
\015\000\015\000\017\000\017\000\017\000\007\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\002\000\001\000\002\000\001\000\
\002\000\001\000\002\000\001\000\001\000\002\000\003\000\001\000\
\003\000\001\000\002\000\002\000\001\000\001\000\004\000\004\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\030\000\
\002\000\032\000\000\000\031\000\004\000\033\000\000\000\021\000\
\000\000\000\000\000\000\006\000\034\000\018\000\000\000\000\000\
\008\000\035\000\000\000\010\000\036\000\000\000\000\000\012\000\
\037\000\000\000\001\000\003\000\000\000\019\000\020\000\005\000\
\000\000\000\000\007\000\014\000\009\000\011\000\028\000\000\000\
\000\000\027\000\000\000\000\000\015\000\000\000\023\000\000\000\
\017\000\000\000\025\000\024\000"

let yydgoto = "\007\000\
\010\000\014\000\021\000\026\000\029\000\033\000\047\000\022\000\
\041\000\030\000\031\000\042\000\016\000\017\000\049\000\050\000\
\051\000"

let yysindex = "\024\000\
\004\255\002\255\001\255\026\255\027\255\028\255\000\000\000\000\
\000\000\000\000\250\254\000\000\000\000\000\000\253\254\000\000\
\016\255\022\255\022\255\000\000\000\000\000\000\025\255\009\255\
\000\000\000\000\035\255\000\000\000\000\041\255\037\255\000\000\
\000\000\038\255\000\000\000\000\040\255\000\000\000\000\000\000\
\042\255\043\255\000\000\000\000\000\000\000\000\000\000\047\255\
\048\255\000\000\044\255\009\255\000\000\040\255\000\000\040\255\
\000\000\050\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\255\000\000\000\000\000\000\000\000\000\000\000\000\030\255\
\000\000\000\000\052\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\057\000\003\000\
\056\000\058\000\250\255\008\000\000\000\220\255\215\255\000\000\
\000\000"

let yytablesize = 62
let yytable = "\034\000\
\048\000\012\000\012\000\035\000\015\000\008\000\036\000\018\000\
\019\000\012\000\020\000\013\000\058\000\009\000\059\000\018\000\
\019\000\048\000\037\000\048\000\038\000\039\000\012\000\044\000\
\001\000\002\000\003\000\004\000\005\000\006\000\024\000\024\000\
\024\000\029\000\040\000\025\000\028\000\032\000\029\000\022\000\
\012\000\008\000\022\000\022\000\043\000\024\000\045\000\046\000\
\053\000\054\000\052\000\055\000\056\000\060\000\013\000\026\000\
\016\000\011\000\023\000\057\000\000\000\027\000"

let yycheck = "\006\000\
\037\000\001\001\001\001\010\001\002\000\002\001\010\001\007\001\
\008\001\001\001\010\001\010\001\054\000\010\001\056\000\007\001\
\008\001\054\000\003\001\056\000\018\000\019\000\001\001\030\000\
\001\000\002\000\003\000\004\000\005\000\006\000\005\001\005\001\
\005\001\004\001\010\001\010\001\010\001\010\001\009\001\006\001\
\001\001\002\001\009\001\010\001\010\001\005\001\010\001\010\001\
\006\001\003\001\009\001\004\001\009\001\004\001\010\001\004\001\
\006\001\001\000\003\000\052\000\255\255\004\000"

let yynames_const = "\
  LEFT_PARENTHESIS\000\
  RIGHT_PARENTHESIS\000\
  LEFT_BRACE\000\
  RIGHT_BRACE\000\
  NEGATIVE\000\
  POSITIVE\000\
  TERM_SEP\000\
  EOI\000\
  "

let yynames_block = "\
  SYMBOL\000\
  VARIABLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    Obj.repr(
# 123 "src/parser_darwin.mly"
      ( init_file (); _1 )
# 198 "src/parser_darwin.ml"
               : Var.var))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "src/parser_darwin.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 204 "src/parser_darwin.ml"
               : Var.var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 130 "src/parser_darwin.mly"
      ( 
	let term = _1
	in 
	  init_file (); 
	  term 
      )
# 216 "src/parser_darwin.ml"
               : Term.term))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "src/parser_darwin.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 222 "src/parser_darwin.ml"
               : Term.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'literal) in
    Obj.repr(
# 142 "src/parser_darwin.mly"
      ( 
	let literal = List.hd !literals
	in
	  init_file();
	  literal
      )
# 234 "src/parser_darwin.ml"
               : Term.literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "src/parser_darwin.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 240 "src/parser_darwin.ml"
               : Term.literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    Obj.repr(
# 154 "src/parser_darwin.mly"
      ( init_file(); _1 )
# 247 "src/parser_darwin.ml"
               : Term.clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "src/parser_darwin.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 253 "src/parser_darwin.ml"
               : Term.clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clauses) in
    Obj.repr(
# 161 "src/parser_darwin.mly"
      (
	init_file ();
	_1
      )
# 263 "src/parser_darwin.ml"
               : Term.clause list))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "src/parser_darwin.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 269 "src/parser_darwin.ml"
               : Term.clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clauses) in
    Obj.repr(
# 171 "src/parser_darwin.mly"
      (
	init_file ();
	_1
      )
# 279 "src/parser_darwin.ml"
               : Term.clause list))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "src/parser_darwin.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 285 "src/parser_darwin.ml"
               : Term.clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 187 "src/parser_darwin.mly"
      ( [_1] )
# 292 "src/parser_darwin.ml"
               : 'clauses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clauses) in
    Obj.repr(
# 189 "src/parser_darwin.mly"
      ( _1 :: _2 )
# 300 "src/parser_darwin.ml"
               : 'clauses))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'literals) in
    Obj.repr(
# 193 "src/parser_darwin.mly"
      (
	let clause = 
	  List.rev !literals
	in
	  init_clause ();
	  clause
      )
# 313 "src/parser_darwin.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 203 "src/parser_darwin.mly"
      ( )
# 320 "src/parser_darwin.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'literals) in
    Obj.repr(
# 205 "src/parser_darwin.mly"
      ( )
# 328 "src/parser_darwin.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 209 "src/parser_darwin.mly"
      ( literals := (Term.request_literal true _1) :: !literals )
# 335 "src/parser_darwin.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 211 "src/parser_darwin.mly"
      (	literals := (Term.request_literal false _2) :: !literals )
# 342 "src/parser_darwin.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 213 "src/parser_darwin.mly"
      (	literals := (Term.request_literal true _2) :: !literals )
# 349 "src/parser_darwin.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'top_func) in
    Obj.repr(
# 218 "src/parser_darwin.mly"
      ( _1 )
# 356 "src/parser_darwin.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 222 "src/parser_darwin.mly"
      ( Term.request_const (Symbol.create_predicate _1 0) )
# 363 "src/parser_darwin.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'constant) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'sub_term_list) in
    Obj.repr(
# 227 "src/parser_darwin.mly"
      ( Term.request_func (Symbol.create_predicate _1 (List.length _3), Array.of_list _3) )
# 371 "src/parser_darwin.ml"
               : 'top_func))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'constant) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'sub_term_list) in
    Obj.repr(
# 232 "src/parser_darwin.mly"
      ( Term.request_func (Symbol.create_function _1 (List.length _3), Array.of_list _3) )
# 379 "src/parser_darwin.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sub_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_term_list) in
    Obj.repr(
# 236 "src/parser_darwin.mly"
      ( _1 :: _3 )
# 387 "src/parser_darwin.ml"
               : 'sub_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_term) in
    Obj.repr(
# 238 "src/parser_darwin.mly"
      ( [_1] )
# 394 "src/parser_darwin.ml"
               : 'sub_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 242 "src/parser_darwin.mly"
      ( _1 )
# 401 "src/parser_darwin.ml"
               : 'sub_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 244 "src/parser_darwin.mly"
      ( Term.request_var _1 )
# 408 "src/parser_darwin.ml"
               : 'sub_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 246 "src/parser_darwin.mly"
      ( Term.request_const (Symbol.create_function _1 0) )
# 415 "src/parser_darwin.ml"
               : 'sub_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 250 "src/parser_darwin.mly"
      ( get_var _1 )
# 422 "src/parser_darwin.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 254 "src/parser_darwin.mly"
      ( _1 )
# 429 "src/parser_darwin.ml"
               : 'constant))
(* Entry parse_var *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry parse_term *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry parse_literal *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry parse_clause *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry parse_clauses *)
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
let parse_var (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Var.var)
let parse_term (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Term.term)
let parse_literal (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Term.literal)
let parse_clause (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : Term.clause)
let parse_clauses (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 5 lexfun lexbuf : Term.clause list)
let parse_file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 6 lexfun lexbuf : Term.clause list)
;;
