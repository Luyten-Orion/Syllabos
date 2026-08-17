import std/[
  sugar
]

import syllabos/[
  core
]

proc advanceBytes(ctx: ParserContext, n: int): ParserContext =
  var
    newCtx = ctx
    i = 0
  while i < n:
    let offset = newCtx.pos.offset
    if offset >= newCtx.input.len:
      break
    let ch = newCtx.input[offset]
    if ch == '\r' and offset + 1 < newCtx.input.len and newCtx.input[offset + 1] == '\n':
      newCtx.pos.offset += 2
      newCtx.pos.line += 1
      newCtx.pos.col = 1
      newCtx.indentValid = false
      i += 2
    else:
      newCtx.pos.offset += 1
      if ch == '\n' or ch == '\r':
        newCtx.pos.line += 1
        newCtx.pos.col = 1
        newCtx.indentValid = false
      else:
        newCtx.pos.col += 1
      i += 1
  return newCtx


proc satisfy*(
  predicate: char -> bool,
  labelHint = ""
): Parser[char] =
  if predicate == nil:
    raise newException(ValueError, "`predicate` cannot be nil!")

  result = proc(ctx: ParserContext): ParseResult[char] =
    let idx = ctx.pos.offset
    if idx >= ctx.input.len:
      return ParseResult[char](
        ctx: ctx,
        success: false,
        err: newError(
          pos = ctx.pos,
          expected = if labelHint.len > 0: expectedSet([labelHint]) else: expectedSet(["character"])
        )
      )
    let ch = ctx.input[idx]
    if predicate(ch):
      let (newCtx, _) = advanceRune(ctx)
      ParseResult[char](ctx: newCtx, success: true, value: ch)
    else:
      ParseResult[char](ctx: ctx, success: false, err: newError(
        pos = ctx.pos,
        expected = if labelHint.len > 0: expectedSet([labelHint]) else: expectedSet(["character satisfying predicate"])
      ))

proc eof*(): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    if ctx.pos.offset >= ctx.input.len:
      ParseResult[Unit](ctx: ctx, success: true, value: ())
    else:
      ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, expected = expectedSet(["end of input"])))

proc pure*[T](val: T): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    ParseResult[T](ctx: ctx, success: true, value: val)

proc fail*[T](msg: string): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    ParseResult[T](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = msg))

proc getPosition*(): Parser[Position] =
  result = proc(ctx: ParserContext): ParseResult[Position] =
    ParseResult[Position](ctx: ctx, success: true, value: ctx.pos)

proc match*(s: string): Parser[string] =
  result = proc(ctx: ParserContext): ParseResult[string] =
    let
      start = ctx.pos.offset
      slen = s.len
    if start + slen > ctx.input.len:
      ParseResult[string](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, expected = expectedSet([s]))
      )
    elif ctx.input[start .. start + slen - 1] == s:
      let newCtx = advanceBytes(ctx, slen)
      ParseResult[string](ctx: newCtx, success: true, value: s)
    else:
      ParseResult[string](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, expected = expectedSet([s]))
      )

template charP*(c: char): Parser[char] = satisfy((ch) => ch == c)

template anyOf*(cs: set[char]): Parser[char] =
  satisfy((ch) => ch in cs, "one of " & $cs)

template text*(s: string): Parser[string] = match(s)