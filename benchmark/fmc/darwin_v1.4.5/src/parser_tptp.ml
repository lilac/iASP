type token =
  | LEFT_PARENTHESIS
  | RIGHT_PARENTHESIS
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | DOT
  | NEGATION
  | COLON
  | COMMA
  | EQUALITY
  | DISEQUALITY
  | EOI
  | FOF
  | CNF
  | THF
  | INCLUDE
  | SINGLE_QUOTED of (string)
  | DOLLAR_WORD of (string)
  | DOLLAR_DOLLAR_WORD of (string)
  | DISTINCT_OBJECT of (string)
  | LOWER_WORD of (string)
  | UPPER_WORD of (string)
  | UNSIGNED_INTEGER of (string)
  | SIGNED_INTEGER of (string)
  | REAL of (string)
  | DOLLAR_TRUE
  | DOLLAR_FALSE
  | DOLLAR
  | AND
  | OR
  | FORALL
  | EXISTS
  | BIJECTION
  | LEFT_IMPLICATION
  | RIGHT_IMPLICATION
  | UNKNOWN

open Parsing;;
let _ = parse_error;;
# 22 "src/parser_tptp.mly"
  
  type var = Var.var
  type symbol = Symbol.symbol
  type term = Term.term
  type literal = Term.literal
  type clause = Term.clause


  (* includes from input *)
  let include_files: string list ref =
    ref []


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

  (* need to detect if input is fof and contains a conjecture *)
  (* is the input in fof format? *)
  let fof =
    ref false

  (* does the fof input contain a conjecture? *)
  let theorem =
    ref false

# 109 "src/parser_tptp.ml"
let yytransl_const = [|
  257 (* LEFT_PARENTHESIS *);
  258 (* RIGHT_PARENTHESIS *);
  259 (* LEFT_BRACKET *);
  260 (* RIGHT_BRACKET *);
  261 (* DOT *);
  262 (* NEGATION *);
  263 (* COLON *);
  264 (* COMMA *);
  265 (* EQUALITY *);
  266 (* DISEQUALITY *);
  267 (* EOI *);
  268 (* FOF *);
  269 (* CNF *);
  270 (* THF *);
  271 (* INCLUDE *);
  281 (* DOLLAR_TRUE *);
  282 (* DOLLAR_FALSE *);
  283 (* DOLLAR *);
  284 (* AND *);
  285 (* OR *);
  286 (* FORALL *);
  287 (* EXISTS *);
  288 (* BIJECTION *);
  289 (* LEFT_IMPLICATION *);
  290 (* RIGHT_IMPLICATION *);
  291 (* UNKNOWN *);
    0|]

let yytransl_block = [|
  272 (* SINGLE_QUOTED *);
  273 (* DOLLAR_WORD *);
  274 (* DOLLAR_DOLLAR_WORD *);
  275 (* DISTINCT_OBJECT *);
  276 (* LOWER_WORD *);
  277 (* UPPER_WORD *);
  278 (* UNSIGNED_INTEGER *);
  279 (* SIGNED_INTEGER *);
  280 (* REAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\008\000\006\000\011\000\011\000\013\000\013\000\015\000\
\017\000\017\000\017\000\017\000\017\000\017\000\016\000\016\000\
\018\000\020\000\020\000\019\000\021\000\021\000\014\000\014\000\
\014\000\014\000\022\000\025\000\025\000\026\000\026\000\023\000\
\028\000\007\000\010\000\012\000\012\000\029\000\029\000\033\000\
\033\000\034\000\034\000\024\000\024\000\024\000\035\000\039\000\
\039\000\036\000\036\000\036\000\036\000\037\000\040\000\040\000\
\042\000\042\000\042\000\038\000\038\000\043\000\043\000\046\000\
\047\000\044\000\044\000\041\000\041\000\045\000\045\000\051\000\
\050\000\027\000\031\000\032\000\032\000\054\000\005\000\057\000\
\057\000\058\000\058\000\053\000\053\000\053\000\059\000\059\000\
\059\000\059\000\061\000\061\000\060\000\060\000\055\000\055\000\
\009\000\009\000\048\000\048\000\052\000\049\000\049\000\049\000\
\056\000\030\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\001\000\001\000\001\000\001\000\
\001\000\010\000\010\000\001\000\001\000\001\000\001\000\003\000\
\001\000\001\000\001\000\001\000\002\000\002\000\001\000\001\000\
\003\000\001\000\003\000\003\000\001\000\003\000\001\000\001\000\
\003\000\001\000\006\000\001\000\001\000\001\000\003\000\002\000\
\001\000\010\000\001\000\001\000\003\000\003\000\001\000\001\000\
\003\000\001\000\002\000\001\000\001\000\001\000\001\000\001\000\
\003\000\001\000\001\000\003\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\004\000\001\000\004\000\001\000\
\001\000\001\000\001\000\001\000\004\000\001\000\004\000\001\000\
\001\000\001\000\001\000\002\000\001\000\001\000\006\000\004\000\
\001\000\001\000\003\000\001\000\003\000\001\000\001\000\004\000\
\001\000\001\000\001\000\003\000\002\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\115\000\
\000\000\000\000\005\000\006\000\007\000\008\000\009\000\000\000\
\000\000\000\000\000\000\001\000\004\000\108\000\107\000\106\000\
\000\000\105\000\000\000\000\000\113\000\000\000\000\000\000\000\
\000\000\000\000\089\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\087\000\
\000\000\041\000\109\000\075\000\082\000\112\000\111\000\110\000\
\058\000\059\000\036\000\037\000\000\000\012\000\000\000\014\000\
\015\000\023\000\024\000\031\000\032\000\034\000\000\000\064\000\
\000\000\052\000\053\000\054\000\055\000\000\000\062\000\063\000\
\065\000\066\000\067\000\000\000\000\000\000\000\074\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\047\000\000\000\
\000\000\000\000\088\000\000\000\000\000\000\000\044\000\000\000\
\000\000\000\000\017\000\018\000\019\000\020\000\000\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\051\000\000\000\
\000\000\000\000\091\000\033\000\098\000\000\000\000\000\000\000\
\097\000\083\000\000\000\094\000\000\000\022\000\021\000\000\000\
\028\000\000\000\025\000\016\000\000\000\000\000\060\000\070\000\
\000\000\078\000\000\000\061\000\000\000\000\000\000\000\046\000\
\000\000\049\000\000\000\101\000\000\000\000\000\000\000\085\000\
\045\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\000\010\000\000\000\
\102\000\084\000\086\000\000\000\000\000\093\000\030\000\027\000\
\000\000\039\000\000\000\000\000\057\000\104\000\000\000\096\000\
\035\000\071\000\079\000\100\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\012\000\013\000\014\000\015\000\
\046\000\038\000\061\000\102\000\062\000\063\000\064\000\065\000\
\111\000\066\000\067\000\139\000\137\000\068\000\069\000\070\000\
\071\000\141\000\072\000\073\000\094\000\103\000\127\000\161\000\
\095\000\096\000\074\000\075\000\076\000\077\000\149\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\157\000\178\000\158\000\030\000\
\036\000\047\000\131\000\132\000\181\000"

let yysindex = "\026\000\
\209\255\000\000\000\000\036\255\057\255\070\255\089\255\000\000\
\069\255\147\255\000\000\000\000\000\000\000\000\000\000\009\255\
\009\255\009\255\084\255\000\000\000\000\000\000\000\000\000\000\
\094\255\000\000\099\255\103\255\000\000\114\255\106\255\106\255\
\106\255\036\000\000\000\126\255\000\000\122\255\127\255\137\255\
\009\255\142\255\049\000\075\000\049\000\140\255\060\000\000\000\
\049\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\146\255\000\000\014\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\161\255\000\000\
\049\000\000\000\000\000\000\000\000\000\073\255\000\000\000\000\
\000\000\000\000\000\000\000\000\167\255\000\000\000\000\000\000\
\169\255\000\000\086\000\118\255\000\000\146\255\000\000\136\255\
\146\255\009\255\000\000\172\255\174\255\175\255\000\000\068\255\
\049\000\049\000\000\000\000\000\000\000\000\000\049\000\148\255\
\000\000\024\000\024\000\024\000\024\000\178\255\000\000\181\255\
\086\000\184\255\000\000\000\000\000\000\165\255\159\255\190\255\
\000\000\000\000\185\255\000\000\177\255\000\000\000\000\183\255\
\000\000\176\255\000\000\000\000\191\255\193\255\000\000\000\000\
\201\255\000\000\202\255\000\000\210\255\205\255\213\255\000\000\
\204\255\000\000\214\255\000\000\217\255\083\000\174\255\000\000\
\000\000\174\255\174\255\000\000\049\000\049\000\221\255\148\255\
\024\000\024\000\000\000\024\000\000\000\000\000\000\000\174\255\
\000\000\000\000\000\000\226\255\227\255\000\000\000\000\000\000\
\049\000\000\000\236\255\237\255\000\000\000\000\174\255\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\234\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\244\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\142\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\244\255\000\000\024\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\208\255\000\000\123\255\000\000\242\255\
\000\000\198\255\000\000\000\000\000\000\244\255\000\000\033\255\
\244\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\244\255\164\255\
\000\000\000\000\171\255\000\000\000\000\000\000\000\000\076\255\
\000\000\079\255\000\000\000\000\000\000\245\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\252\255\000\000\000\000\
\000\000\000\000\000\000\000\000\173\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\251\255\000\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\254\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\237\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\082\000\241\255\253\255\000\000\189\255\000\000\000\000\
\000\000\000\000\000\000\089\000\093\000\000\000\000\000\224\255\
\000\000\094\000\158\255\000\000\000\000\233\255\000\000\000\000\
\180\255\000\000\000\000\000\000\000\000\000\000\179\255\163\255\
\000\000\000\000\000\000\000\000\000\000\192\255\196\255\243\255\
\166\255\203\255\207\255\000\000\168\255\000\000\113\255\000\000\
\000\000\165\000\000\000\000\000\073\000"

let yytablesize = 368
let yytable = "\126\000\
\095\000\156\000\026\000\026\000\026\000\113\000\035\000\092\000\
\126\000\103\000\129\000\093\000\130\000\142\000\118\000\179\000\
\025\000\027\000\028\000\104\000\143\000\148\000\150\000\150\000\
\022\000\013\000\001\000\026\000\023\000\097\000\024\000\013\000\
\190\000\100\000\048\000\129\000\016\000\136\000\138\000\151\000\
\048\000\105\000\106\000\140\000\154\000\107\000\108\000\109\000\
\110\000\144\000\144\000\144\000\144\000\145\000\145\000\145\000\
\145\000\017\000\093\000\119\000\146\000\146\000\146\000\146\000\
\147\000\147\000\147\000\147\000\129\000\142\000\018\000\129\000\
\129\000\180\000\182\000\150\000\150\000\029\000\150\000\020\000\
\026\000\114\000\115\000\029\000\026\000\129\000\026\000\128\000\
\093\000\019\000\120\000\187\000\188\000\122\000\189\000\134\000\
\135\000\136\000\138\000\029\000\129\000\031\000\180\000\160\000\
\144\000\144\000\032\000\144\000\145\000\145\000\033\000\145\000\
\128\000\039\000\040\000\146\000\146\000\193\000\146\000\147\000\
\147\000\034\000\147\000\073\000\072\000\037\000\041\000\042\000\
\072\000\043\000\072\000\072\000\072\000\022\000\044\000\051\000\
\052\000\023\000\053\000\054\000\055\000\056\000\057\000\058\000\
\045\000\128\000\048\000\098\000\128\000\128\000\072\000\072\000\
\099\000\101\000\072\000\072\000\072\000\072\000\004\000\005\000\
\006\000\007\000\128\000\112\000\121\000\095\000\159\000\116\000\
\053\000\117\000\095\000\095\000\092\000\124\000\103\000\177\000\
\133\000\128\000\092\000\152\000\022\000\164\000\153\000\125\000\
\023\000\155\000\054\000\055\000\056\000\022\000\162\000\163\000\
\125\000\023\000\167\000\054\000\055\000\056\000\080\000\081\000\
\168\000\169\000\170\000\081\000\166\000\081\000\081\000\081\000\
\174\000\068\000\165\000\171\000\172\000\068\000\173\000\068\000\
\070\000\070\000\175\000\003\000\004\000\005\000\006\000\007\000\
\176\000\081\000\081\000\185\000\192\000\081\000\081\000\081\000\
\081\000\191\000\090\000\068\000\068\000\194\000\195\000\068\000\
\068\000\068\000\068\000\076\000\003\000\114\000\021\000\076\000\
\038\000\076\000\078\000\078\000\069\000\056\000\184\000\099\000\
\069\000\183\000\069\000\071\000\071\000\186\000\123\000\196\000\
\000\000\000\000\000\000\000\000\000\000\076\000\076\000\000\000\
\000\000\076\000\076\000\076\000\076\000\000\000\069\000\069\000\
\000\000\000\000\069\000\069\000\069\000\069\000\077\000\000\000\
\000\000\000\000\077\000\000\000\077\000\079\000\079\000\022\000\
\000\000\051\000\052\000\023\000\053\000\054\000\055\000\056\000\
\000\000\049\000\000\000\000\000\000\000\000\000\050\000\000\000\
\077\000\077\000\000\000\000\000\077\000\077\000\077\000\077\000\
\022\000\000\000\051\000\052\000\023\000\053\000\054\000\055\000\
\056\000\057\000\058\000\091\000\000\000\000\000\059\000\060\000\
\092\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\092\000\051\000\052\000\023\000\053\000\
\054\000\055\000\056\000\057\000\058\000\022\000\000\000\051\000\
\052\000\023\000\053\000\054\000\055\000\056\000\057\000\058\000"

let yycheck = "\091\000\
\093\000\093\000\016\000\017\000\018\000\073\000\030\000\093\000\
\091\000\093\000\101\000\044\000\101\000\112\000\091\000\159\000\
\016\000\017\000\018\000\006\001\114\000\115\000\116\000\117\000\
\016\001\002\001\001\000\041\000\020\001\045\000\022\001\008\001\
\176\000\049\000\002\001\126\000\001\001\105\000\106\000\117\000\
\008\001\028\001\029\001\111\000\121\000\032\001\033\001\034\001\
\035\001\114\000\115\000\116\000\117\000\114\000\115\000\116\000\
\117\000\001\001\091\000\092\000\114\000\115\000\116\000\117\000\
\114\000\115\000\116\000\117\000\159\000\168\000\001\001\162\000\
\163\000\162\000\163\000\169\000\170\000\002\001\172\000\011\001\
\002\001\009\001\010\001\008\001\098\000\176\000\008\001\101\000\
\121\000\001\001\094\000\169\000\170\000\097\000\172\000\028\001\
\029\001\165\000\166\000\016\001\191\000\008\001\191\000\127\000\
\169\000\170\000\008\001\172\000\169\000\170\000\008\001\172\000\
\126\000\032\000\033\000\169\000\170\000\185\000\172\000\169\000\
\170\000\008\001\172\000\001\001\002\001\020\001\091\000\002\001\
\006\001\008\001\008\001\009\001\010\001\016\001\008\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\008\001\159\000\005\001\008\001\162\000\163\000\028\001\029\001\
\093\000\008\001\032\001\033\001\034\001\035\001\012\001\013\001\
\014\001\015\001\176\000\003\001\029\001\002\001\008\001\001\001\
\021\001\001\001\007\001\008\001\002\001\002\001\002\001\093\000\
\002\001\191\000\008\001\002\001\016\001\005\001\002\001\019\001\
\020\001\002\001\022\001\023\001\024\001\016\001\001\001\007\001\
\019\001\020\001\004\001\022\001\023\001\024\001\001\001\002\001\
\008\001\001\001\001\001\006\001\029\001\008\001\009\001\010\001\
\005\001\002\001\028\001\002\001\008\001\006\001\002\001\008\001\
\009\001\010\001\005\001\011\001\012\001\013\001\014\001\015\001\
\008\001\028\001\029\001\007\001\002\001\032\001\033\001\034\001\
\035\001\008\001\093\000\028\001\029\001\002\001\002\001\032\001\
\033\001\034\001\035\001\002\001\011\001\002\001\010\000\006\001\
\004\001\008\001\009\001\010\001\002\001\002\001\166\000\002\001\
\006\001\165\000\008\001\009\001\010\001\168\000\098\000\191\000\
\255\255\255\255\255\255\255\255\255\255\028\001\029\001\255\255\
\255\255\032\001\033\001\034\001\035\001\255\255\028\001\029\001\
\255\255\255\255\032\001\033\001\034\001\035\001\002\001\255\255\
\255\255\255\255\006\001\255\255\008\001\009\001\010\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\255\255\001\001\255\255\255\255\255\255\255\255\006\001\255\255\
\028\001\029\001\255\255\255\255\032\001\033\001\034\001\035\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\001\001\255\255\255\255\030\001\031\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\016\001\006\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\016\001\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001"

let yynames_const = "\
  LEFT_PARENTHESIS\000\
  RIGHT_PARENTHESIS\000\
  LEFT_BRACKET\000\
  RIGHT_BRACKET\000\
  DOT\000\
  NEGATION\000\
  COLON\000\
  COMMA\000\
  EQUALITY\000\
  DISEQUALITY\000\
  EOI\000\
  FOF\000\
  CNF\000\
  THF\000\
  INCLUDE\000\
  DOLLAR_TRUE\000\
  DOLLAR_FALSE\000\
  DOLLAR\000\
  AND\000\
  OR\000\
  FORALL\000\
  EXISTS\000\
  BIJECTION\000\
  LEFT_IMPLICATION\000\
  RIGHT_IMPLICATION\000\
  UNKNOWN\000\
  "

let yynames_block = "\
  SINGLE_QUOTED\000\
  DOLLAR_WORD\000\
  DOLLAR_DOLLAR_WORD\000\
  DISTINCT_OBJECT\000\
  LOWER_WORD\000\
  UPPER_WORD\000\
  UNSIGNED_INTEGER\000\
  SIGNED_INTEGER\000\
  REAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'file) in
    Obj.repr(
# 137 "src/parser_tptp.mly"
      (
	let clauses = _1 in
        let includes = !include_files in
	let is_fof = !fof in
        let is_theorem = !theorem in

	(* reset for next parser run *)
        include_files := [];
	fof := false;
	theorem := false;
        
        if is_fof then
	  raise (Const.FOF is_theorem)

        else
          clauses, includes
      )
# 447 "src/parser_tptp.ml"
               : Term.clause list * string list))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "src/parser_tptp.mly"
      ( print_endline "empty problem specification"; raise Const.PARSE_ERROR )
# 453 "src/parser_tptp.ml"
               : Term.clause list * string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tptp_input) in
    Obj.repr(
# 165 "src/parser_tptp.mly"
      ( match _1 with
        | Some clause -> [clause]
        | None        -> []
      )
# 463 "src/parser_tptp.ml"
               : 'file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tptp_input) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'file) in
    Obj.repr(
# 171 "src/parser_tptp.mly"
      ( match _1 with
        | Some clause -> clause :: _2
        | None        -> _2
      )
# 474 "src/parser_tptp.ml"
               : 'file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'annotated_formula) in
    Obj.repr(
# 178 "src/parser_tptp.mly"
      ( Some _1 )
# 481 "src/parser_tptp.ml"
               : 'tptp_input))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'include_) in
    Obj.repr(
# 181 "src/parser_tptp.mly"
      ( None )
# 488 "src/parser_tptp.ml"
               : 'tptp_input))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fof_annotated) in
    Obj.repr(
# 187 "src/parser_tptp.mly"
      ( _1 )
# 495 "src/parser_tptp.ml"
               : 'annotated_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cnf_annotated) in
    Obj.repr(
# 190 "src/parser_tptp.mly"
      ( _1 )
# 502 "src/parser_tptp.ml"
               : 'annotated_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'thf_annotated) in
    Obj.repr(
# 193 "src/parser_tptp.mly"
      ( _1 )
# 509 "src/parser_tptp.ml"
               : 'annotated_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formula_role) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'fof_formula) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'annotations) in
    Obj.repr(
# 197 "src/parser_tptp.mly"
    ( failwith "Parser_tptp: tfh syntax not supported." )
# 519 "src/parser_tptp.ml"
               : 'thf_annotated))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formula_role) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'fof_formula) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'annotations) in
    Obj.repr(
# 201 "src/parser_tptp.mly"
    ( fof := true; [] )
# 529 "src/parser_tptp.ml"
               : 'fof_annotated))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binary_formula) in
    Obj.repr(
# 205 "src/parser_tptp.mly"
    ( "" )
# 536 "src/parser_tptp.ml"
               : 'fof_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unitary_formula) in
    Obj.repr(
# 208 "src/parser_tptp.mly"
    ( "" )
# 543 "src/parser_tptp.ml"
               : 'fof_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nonassoc_binary) in
    Obj.repr(
# 213 "src/parser_tptp.mly"
    ( "" )
# 550 "src/parser_tptp.ml"
               : 'binary_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assoc_binary) in
    Obj.repr(
# 216 "src/parser_tptp.mly"
    ( "" )
# 557 "src/parser_tptp.ml"
               : 'binary_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unitary_formula) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'binary_connective) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unitary_formula) in
    Obj.repr(
# 220 "src/parser_tptp.mly"
    ( "" )
# 566 "src/parser_tptp.ml"
               : 'nonassoc_binary))
; (fun __caml_parser_env ->
    Obj.repr(
# 224 "src/parser_tptp.mly"
    ( "" )
# 572 "src/parser_tptp.ml"
               : 'binary_connective))
; (fun __caml_parser_env ->
    Obj.repr(
# 226 "src/parser_tptp.mly"
    ( "" )
# 578 "src/parser_tptp.ml"
               : 'binary_connective))
; (fun __caml_parser_env ->
    Obj.repr(
# 228 "src/parser_tptp.mly"
    ( "" )
# 584 "src/parser_tptp.ml"
               : 'binary_connective))
; (fun __caml_parser_env ->
    Obj.repr(
# 230 "src/parser_tptp.mly"
    ( "" )
# 590 "src/parser_tptp.ml"
               : 'binary_connective))
; (fun __caml_parser_env ->
    Obj.repr(
# 232 "src/parser_tptp.mly"
    ( "" )
# 596 "src/parser_tptp.ml"
               : 'binary_connective))
; (fun __caml_parser_env ->
    Obj.repr(
# 234 "src/parser_tptp.mly"
    ( "" )
# 602 "src/parser_tptp.ml"
               : 'binary_connective))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_formula) in
    Obj.repr(
# 238 "src/parser_tptp.mly"
    ( "" )
# 609 "src/parser_tptp.ml"
               : 'assoc_binary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_formula) in
    Obj.repr(
# 240 "src/parser_tptp.mly"
    ( "" )
# 616 "src/parser_tptp.ml"
               : 'assoc_binary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unitary_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_or_formula) in
    Obj.repr(
# 244 "src/parser_tptp.mly"
    ( "" )
# 624 "src/parser_tptp.ml"
               : 'or_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unitary_formula) in
    Obj.repr(
# 248 "src/parser_tptp.mly"
    ( "" )
# 631 "src/parser_tptp.ml"
               : 'more_or_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unitary_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_or_formula) in
    Obj.repr(
# 250 "src/parser_tptp.mly"
    ( "" )
# 639 "src/parser_tptp.ml"
               : 'more_or_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unitary_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_and_formula) in
    Obj.repr(
# 254 "src/parser_tptp.mly"
    ( "" )
# 647 "src/parser_tptp.ml"
               : 'and_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unitary_formula) in
    Obj.repr(
# 258 "src/parser_tptp.mly"
    ( "" )
# 654 "src/parser_tptp.ml"
               : 'more_and_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unitary_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_and_formula) in
    Obj.repr(
# 260 "src/parser_tptp.mly"
    ( "" )
# 662 "src/parser_tptp.ml"
               : 'more_and_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'quantified_formula) in
    Obj.repr(
# 264 "src/parser_tptp.mly"
    ( "" )
# 669 "src/parser_tptp.ml"
               : 'unitary_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_formula) in
    Obj.repr(
# 266 "src/parser_tptp.mly"
    ( "" )
# 676 "src/parser_tptp.ml"
               : 'unitary_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fof_formula) in
    Obj.repr(
# 268 "src/parser_tptp.mly"
    ( "" )
# 683 "src/parser_tptp.ml"
               : 'unitary_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_formula) in
    Obj.repr(
# 270 "src/parser_tptp.mly"
    ( "" )
# 690 "src/parser_tptp.ml"
               : 'unitary_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'quantifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'variable_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'unitary_formula) in
    Obj.repr(
# 274 "src/parser_tptp.mly"
    ( "" )
# 699 "src/parser_tptp.ml"
               : 'quantified_formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 278 "src/parser_tptp.mly"
    ( "" )
# 705 "src/parser_tptp.ml"
               : 'quantifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 280 "src/parser_tptp.mly"
    ( "" )
# 711 "src/parser_tptp.ml"
               : 'quantifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 284 "src/parser_tptp.mly"
    ( "" )
# 718 "src/parser_tptp.ml"
               : 'variable_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'variable_list) in
    Obj.repr(
# 286 "src/parser_tptp.mly"
    ( "" )
# 726 "src/parser_tptp.ml"
               : 'variable_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unary_connective) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unitary_formula) in
    Obj.repr(
# 290 "src/parser_tptp.mly"
    ( "" )
# 734 "src/parser_tptp.ml"
               : 'unary_formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 294 "src/parser_tptp.mly"
    ( "" )
# 740 "src/parser_tptp.ml"
               : 'unary_connective))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formula_role) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'cnf_formula) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'annotations) in
    Obj.repr(
# 300 "src/parser_tptp.mly"
      (
	let clause = 
	  _7
	in
	  init_clause ();
	  clause
      )
# 756 "src/parser_tptp.ml"
               : 'cnf_annotated))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 310 "src/parser_tptp.mly"
    ( let role =
        _1
      in
        if role = "conjecture" then
          theorem := true;

        _1
    )
# 770 "src/parser_tptp.ml"
               : 'formula_role))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'null) in
    Obj.repr(
# 321 "src/parser_tptp.mly"
      ( "" )
# 777 "src/parser_tptp.ml"
               : 'annotations))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'source) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'optional_info) in
    Obj.repr(
# 324 "src/parser_tptp.mly"
      ( "" )
# 785 "src/parser_tptp.ml"
               : 'annotations))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'disjunction) in
    Obj.repr(
# 330 "src/parser_tptp.mly"
      ( _2 )
# 792 "src/parser_tptp.ml"
               : 'cnf_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'disjunction) in
    Obj.repr(
# 333 "src/parser_tptp.mly"
      ( _1 )
# 799 "src/parser_tptp.ml"
               : 'cnf_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 337 "src/parser_tptp.mly"
      ( [_1] )
# 806 "src/parser_tptp.ml"
               : 'disjunction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'disjunction) in
    Obj.repr(
# 340 "src/parser_tptp.mly"
      ( _1 :: _3 )
# 814 "src/parser_tptp.ml"
               : 'disjunction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_formula) in
    Obj.repr(
# 347 "src/parser_tptp.mly"
      ( _1 )
# 821 "src/parser_tptp.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_formula) in
    Obj.repr(
# 350 "src/parser_tptp.mly"
      ( Term.request_negated_literal _2 )
# 828 "src/parser_tptp.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plain_atom) in
    Obj.repr(
# 354 "src/parser_tptp.mly"
      ( _1 )
# 835 "src/parser_tptp.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'defined_atom) in
    Obj.repr(
# 357 "src/parser_tptp.mly"
      ( _1 )
# 842 "src/parser_tptp.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'system_atom) in
    Obj.repr(
# 360 "src/parser_tptp.mly"
      ( _1 )
# 849 "src/parser_tptp.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plain_term_top) in
    Obj.repr(
# 364 "src/parser_tptp.mly"
      ( Term.request_literal true _1 )
# 856 "src/parser_tptp.ml"
               : 'plain_atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 368 "src/parser_tptp.mly"
      ( [ _1 ] )
# 863 "src/parser_tptp.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 371 "src/parser_tptp.mly"
      ( _1 :: _3 )
# 871 "src/parser_tptp.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    Obj.repr(
# 375 "src/parser_tptp.mly"
      ( Term.true_literal )
# 877 "src/parser_tptp.ml"
               : 'defined_atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 378 "src/parser_tptp.mly"
      ( Term.false_literal )
# 883 "src/parser_tptp.ml"
               : 'defined_atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 381 "src/parser_tptp.mly"
      ( Term.request_literal true (Term.request_func (Symbol.equality, [| _1; _3|])) )
# 891 "src/parser_tptp.ml"
               : 'defined_atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 384 "src/parser_tptp.mly"
      ( Term.request_literal false (Term.request_func (Symbol.equality, [| _1; _3|])) )
# 899 "src/parser_tptp.ml"
               : 'defined_atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'system_term_top) in
    Obj.repr(
# 388 "src/parser_tptp.mly"
      ( Term.request_literal true _1 )
# 906 "src/parser_tptp.ml"
               : 'system_atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_term) in
    Obj.repr(
# 393 "src/parser_tptp.mly"
      ( _1 )
# 913 "src/parser_tptp.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 396 "src/parser_tptp.mly"
      ( Term.request_var _1 )
# 920 "src/parser_tptp.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plain_term) in
    Obj.repr(
# 400 "src/parser_tptp.mly"
      ( _1 )
# 927 "src/parser_tptp.ml"
               : 'function_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'defined_term) in
    Obj.repr(
# 403 "src/parser_tptp.mly"
      ( _1 )
# 934 "src/parser_tptp.ml"
               : 'function_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'system_term) in
    Obj.repr(
# 406 "src/parser_tptp.mly"
      ( _1 )
# 941 "src/parser_tptp.ml"
               : 'function_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 410 "src/parser_tptp.mly"
      ( Term.request_const (Symbol.create_predicate _1 0) )
# 948 "src/parser_tptp.ml"
               : 'plain_term_top))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'functor_) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 413 "src/parser_tptp.mly"
      ( let subterms = Array.of_list _3 in
	  Term.request_func (Symbol.create_predicate _1 (Array.length subterms), subterms)
      )
# 958 "src/parser_tptp.ml"
               : 'plain_term_top))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 419 "src/parser_tptp.mly"
      ( Term.request_const (Symbol.create_function _1 0) )
# 965 "src/parser_tptp.ml"
               : 'plain_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'functor_) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 422 "src/parser_tptp.mly"
      ( let subterms = Array.of_list _3 in
	  Term.request_func (Symbol.create_function _1 (Array.length subterms), subterms)
      )
# 975 "src/parser_tptp.ml"
               : 'plain_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_word) in
    Obj.repr(
# 428 "src/parser_tptp.mly"
      ( _1 )
# 982 "src/parser_tptp.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_word) in
    Obj.repr(
# 432 "src/parser_tptp.mly"
      ( _1 )
# 989 "src/parser_tptp.ml"
               : 'functor_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 436 "src/parser_tptp.mly"
      ( print_endline ("Parser_tptp: <defined_term: number> not supported: " ^ _1); raise Const.PARSE_ERROR )
# 996 "src/parser_tptp.ml"
               : 'defined_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 439 "src/parser_tptp.mly"
      ( print_endline ("Parser_tptp: <defined_term: distinct_object> not supported: " ^ _1); raise Const.PARSE_ERROR )
# 1003 "src/parser_tptp.ml"
               : 'defined_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'system_constant) in
    Obj.repr(
# 443 "src/parser_tptp.mly"
      ( Term.request_const (Symbol.create_predicate _1 0) )
# 1010 "src/parser_tptp.ml"
               : 'system_term_top))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'system_functor) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 446 "src/parser_tptp.mly"
      ( let subterms = Array.of_list _3 in
	  Term.request_func (Symbol.create_predicate _1 (Array.length subterms), subterms)
      )
# 1020 "src/parser_tptp.ml"
               : 'system_term_top))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'system_constant) in
    Obj.repr(
# 452 "src/parser_tptp.mly"
      ( Term.request_const (Symbol.create_function _1 0) )
# 1027 "src/parser_tptp.ml"
               : 'system_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'system_functor) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 455 "src/parser_tptp.mly"
      ( let subterms = Array.of_list _3 in
	  Term.request_func (Symbol.create_function _1 (Array.length subterms), subterms)
      )
# 1037 "src/parser_tptp.ml"
               : 'system_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_system_word) in
    Obj.repr(
# 461 "src/parser_tptp.mly"
      ( _1 )
# 1044 "src/parser_tptp.ml"
               : 'system_functor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_system_word) in
    Obj.repr(
# 465 "src/parser_tptp.mly"
      ( _1 )
# 1051 "src/parser_tptp.ml"
               : 'system_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 471 "src/parser_tptp.mly"
      ( get_var _1 )
# 1058 "src/parser_tptp.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'general_term) in
    Obj.repr(
# 477 "src/parser_tptp.mly"
      ( "" )
# 1065 "src/parser_tptp.ml"
               : 'source))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'useful_info) in
    Obj.repr(
# 481 "src/parser_tptp.mly"
      ( "" )
# 1072 "src/parser_tptp.ml"
               : 'optional_info))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'null) in
    Obj.repr(
# 484 "src/parser_tptp.mly"
      ( "" )
# 1079 "src/parser_tptp.ml"
               : 'optional_info))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'general_term_list) in
    Obj.repr(
# 488 "src/parser_tptp.mly"
      ( "" )
# 1086 "src/parser_tptp.ml"
               : 'useful_info))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'file_name) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'formula_selection) in
    Obj.repr(
# 493 "src/parser_tptp.mly"
      ( include_files := _3 :: !include_files )
# 1094 "src/parser_tptp.ml"
               : 'include_))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'name_list) in
    Obj.repr(
# 497 "src/parser_tptp.mly"
      ( _3 )
# 1101 "src/parser_tptp.ml"
               : 'formula_selection))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'null) in
    Obj.repr(
# 500 "src/parser_tptp.mly"
      ( [] )
# 1108 "src/parser_tptp.ml"
               : 'formula_selection))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 504 "src/parser_tptp.mly"
      ( [_1] )
# 1115 "src/parser_tptp.ml"
               : 'name_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name_list) in
    Obj.repr(
# 507 "src/parser_tptp.mly"
      ( _1 :: _3 )
# 1123 "src/parser_tptp.ml"
               : 'name_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'general_data) in
    Obj.repr(
# 513 "src/parser_tptp.mly"
      ( "" )
# 1130 "src/parser_tptp.ml"
               : 'general_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'general_data) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'general_term) in
    Obj.repr(
# 516 "src/parser_tptp.mly"
      ( "" )
# 1138 "src/parser_tptp.ml"
               : 'general_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'general_list) in
    Obj.repr(
# 519 "src/parser_tptp.mly"
      ( "" )
# 1145 "src/parser_tptp.ml"
               : 'general_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_word) in
    Obj.repr(
# 523 "src/parser_tptp.mly"
      ( "" )
# 1152 "src/parser_tptp.ml"
               : 'general_data))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atomic_word) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'general_arguments) in
    Obj.repr(
# 526 "src/parser_tptp.mly"
      ( "" )
# 1160 "src/parser_tptp.ml"
               : 'general_data))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 529 "src/parser_tptp.mly"
      ( "" )
# 1167 "src/parser_tptp.ml"
               : 'general_data))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 532 "src/parser_tptp.mly"
      ( "" )
# 1174 "src/parser_tptp.ml"
               : 'general_data))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'general_term) in
    Obj.repr(
# 536 "src/parser_tptp.mly"
      ( [_1] )
# 1181 "src/parser_tptp.ml"
               : 'general_arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'general_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'general_arguments) in
    Obj.repr(
# 539 "src/parser_tptp.mly"
      ( _1 :: _3 )
# 1189 "src/parser_tptp.ml"
               : 'general_arguments))
; (fun __caml_parser_env ->
    Obj.repr(
# 543 "src/parser_tptp.mly"
      ( [] )
# 1195 "src/parser_tptp.ml"
               : 'general_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'general_term_list) in
    Obj.repr(
# 546 "src/parser_tptp.mly"
      ( _2 )
# 1202 "src/parser_tptp.ml"
               : 'general_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'general_term) in
    Obj.repr(
# 550 "src/parser_tptp.mly"
      ( [_1] )
# 1209 "src/parser_tptp.ml"
               : 'general_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'general_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'general_term_list) in
    Obj.repr(
# 553 "src/parser_tptp.mly"
      ( _1 :: _3 )
# 1217 "src/parser_tptp.ml"
               : 'general_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_word) in
    Obj.repr(
# 558 "src/parser_tptp.mly"
      ( _1 )
# 1224 "src/parser_tptp.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 561 "src/parser_tptp.mly"
      ( _1 )
# 1231 "src/parser_tptp.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 565 "src/parser_tptp.mly"
      ( _1 )
# 1238 "src/parser_tptp.ml"
               : 'atomic_word))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 568 "src/parser_tptp.mly"
      ( _1 )
# 1245 "src/parser_tptp.ml"
               : 'atomic_word))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 572 "src/parser_tptp.mly"
      ( print_endline ("Parser_tptp: <$$word> not supported: " ^ _1); raise Const.PARSE_ERROR )
# 1252 "src/parser_tptp.ml"
               : 'atomic_system_word))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 576 "src/parser_tptp.mly"
      ( _1 )
# 1259 "src/parser_tptp.ml"
               : 'number))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 579 "src/parser_tptp.mly"
      ( _1 )
# 1266 "src/parser_tptp.ml"
               : 'number))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 582 "src/parser_tptp.mly"
      ( _1 )
# 1273 "src/parser_tptp.ml"
               : 'number))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 586 "src/parser_tptp.mly"
      ( let quoted = _1 in
        String.sub quoted 1 (String.length quoted - 2)
      )
# 1282 "src/parser_tptp.ml"
               : 'file_name))
; (fun __caml_parser_env ->
    Obj.repr(
# 591 "src/parser_tptp.mly"
      ( )
# 1288 "src/parser_tptp.ml"
               : 'null))
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
let parse_file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Term.clause list * string list)
;;
# 594 "src/parser_tptp.mly"




# 1318 "src/parser_tptp.ml"
