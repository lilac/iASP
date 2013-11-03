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

val parse_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.clause list * string list
