import syllabos/[
  combinators,
  primitives,
  errors,
  core
]

template spaceOrTab*(): Parser[char] =
  anyOf({' ', '\t'}).label("space or tab")

template skipHorizontal*(): Parser[Unit] =
  spaceOrTab().many0() *> ().pure()

template lexeme*[T](p: Parser[T]): Parser[T] =
  p <* skipHorizontal()