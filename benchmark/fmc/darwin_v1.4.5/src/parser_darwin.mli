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

val parse_var :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Var.var
val parse_term :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.term
val parse_literal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.literal
val parse_clause :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.clause
val parse_clauses :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.clause list
val parse_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Term.clause list
