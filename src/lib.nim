import std/[
  options
]

import syllabos/[
  indentation,
  combinators,
  primitives,
  precedence,
  trivia,
  errors,
  state,
  core
]

proc runParser*[T](
  p: Parser[T],
  input: string,
  state = UserState(),
  config = IndentConfig(mode: imAuto, tabWidth: 4)
): ParseResult[T] =
  p(newContext(input, state, config))

export
  indentation,
  combinators,
  primitives,
  precedence,
  trivia,
  errors,
  state,
  core

# Need std/options for the lib
export options