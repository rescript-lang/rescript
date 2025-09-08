module Token = Res_token
module Grammar = Res_grammar

type problem =
  | Unexpected of Token.t [@live]
  | Expected of {
      token: Token.t;
      pos: Lexing.position;
      context: Grammar.t option;
    } [@live]
  | Message of string [@live]
  | Uident [@live]
  | Lident [@live]
  | Unbalanced of Token.t [@live]

type parse_error = Lexing.position * problem
