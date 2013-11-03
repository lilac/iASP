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

val parse_clause :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.clause
val parse_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.clause list
