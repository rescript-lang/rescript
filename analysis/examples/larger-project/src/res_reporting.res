module Token = Res_token
module Grammar = Res_grammar

type problem =
  | @live Unexpected(Token.t)
  | @live Expected({token: Token.t, pos: Lexing.position, context: option<Grammar.t>})
  | @live Message(string)
  | @live Uident
  | @live Lident
  | @live Unbalanced(Token.t)

type parseError = (Lexing.position, problem)

