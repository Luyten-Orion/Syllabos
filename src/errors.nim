import std/[
  strutils,
  sets
]

import syllabos/[
  core
]

proc label*[T](p: Parser[T], msg: string): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    result = p(ctx)
    if not result.success:
      result.err.expected = expectedSet([msg])

proc withMessage*[T](p: Parser[T], msg: string): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    result = p(ctx)
    if not result.success:
      result.err.customMsg = msg

proc fatal*[T](p: Parser[T]): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    result = p(ctx)
    if not result.success:
      result.err.isFatal = true

proc commit*[T](p: Parser[T]): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    let pos = ctx.pos
    result = p(ctx)
    if not result.success and result.ctx.pos.offset > pos.offset:
      result.err.isFatal = true

proc formatError*(input: string, err: ParseError): string =
  result = "Error at line " & $err.pos.line & ", column " & $err.pos.col & ":\n"
  var lineStart = err.pos.offset
  while lineStart > 0 and input[lineStart-1] != '\n':
    lineStart -= 1
  var lineEnd = err.pos.offset
  while lineEnd < input.len and input[lineEnd] != '\n':
    lineEnd += 1
  let lineStr = input[lineStart .. lineEnd-1]
  result.add("  " & lineStr & "\n")
  let colInLine = err.pos.col - 1
  result.add("  " & " ".repeat(colInLine) & "^\n")
  if err.customMsg.len > 0:
    result.add("  Message: " & err.customMsg)
  elif err.expected.len > 0:
    var expList: seq[string]
    for e in err.expected:
      expList.add(e)
    result.add("  Expected: " & expList.join(", "))