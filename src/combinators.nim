import std/[
  options,
  sugar
]

import syllabos/[
  primitives,
  core
]

proc andThen*[T, U](p: Parser[T], f: T -> Parser[U]): Parser[U] =
  result = proc(ctx: ParserContext): ParseResult[U] =
    let resT = p(ctx)
    if not resT.success:
      return ParseResult[U](ctx: resT.ctx, success: false, err: resT.err)
    let resU = f(resT.value)(resT.ctx)
    if not resU.success:
      ParseResult[U](ctx: resU.ctx, success: false, err: resU.err)
    else:
      resU

template `>>=`*[T, U](p: Parser[T], f: proc(v: T): Parser[U]): Parser[U] = p.andThen(f)

proc map*[T, U](p: Parser[T], f: T -> U): Parser[U] =
  p >>= proc(v: T): Parser[U] = f(v).pure()

proc keepLeft*[T, U](left: Parser[T], right: Parser[U]): Parser[T] =
  left >>= proc(v: T): Parser[T] = right.map(_ => v)

proc keepRight*[T, U](left: Parser[T], right: Parser[U]): Parser[U] =
  left >>= proc(_: T): Parser[U] = right

proc attempt*[T](p: Parser[T]): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    let
      saved = ctx.copy()
      res = p(ctx)
    if res.success: res
    else: ParseResult[T](ctx: saved, success: false, err: res.err)

template attempt*[T](p: Parser[T], ctx: ParserContext): ParseResult[T] =
  attempt(p)(ctx)

proc alt*[T](p1, p2: Parser[T]): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    let
      saved = ctx.copy()
      res1 = p1(ctx)
    if res1.success or ctx.pos.offset > saved.pos.offset or res1.err.isFatal:
      return res1
    let res2 = p2(saved)
    if res2.success: res2
    else: ParseResult[T](ctx: saved, success: false, err: mergeErrors(res1.err, res2.err))

proc notFollowedBy*[T](p: Parser[T]): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    let
      saved = ctx.copy()
      res = p(ctx)
    if res.success:
      ParseResult[Unit](ctx: saved, success: false, err: newError(pos = saved.pos, expected = expectedSet(["not followed by parser"])))
    else:
      ParseResult[Unit](ctx: saved, success: true, value: ())

template `<|>`*[T](p1, p2: Parser[T]): Parser[T] = alt(p1, p2)
template `*>`*[T, U](left: Parser[T], right: Parser[U]): Parser[U] = left.keepRight(right)
template `<*`*[T, U](left: Parser[T], right: Parser[U]): Parser[T] = left.keepLeft(right)

proc many0*[T](p: Parser[T]): Parser[seq[T]] =
  result = proc(ctx: ParserContext): ParseResult[seq[T]] =
    var
      curCtx = ctx
      acc: seq[T]
    while true:
      let
        saved = curCtx.copy()
        res = p(curCtx)
      if not res.success:
        curCtx = saved
        break
      acc.add(res.value)
      curCtx = res.ctx
    ParseResult[seq[T]](ctx: curCtx, success: true, value: acc)

template many1*[T](p: Parser[T]): Parser[seq[T]] =
  p >>= (proc(v: T): Parser[seq[T]] =
    many0(p).map(vs => v & vs)
  )

template skipMany0*[T](p: Parser[T]): Parser[Unit] =
  many0(p) *> ().pure()

template skipMany1*[T](p: Parser[T]): Parser[Unit] =
  many1(p) *> ().pure()

proc opt*[T](p: Parser[T]): Parser[Option[T]] =
  result = proc(ctx: ParserContext): ParseResult[Option[T]] =
    let
      saved = ctx.copy()
      res = attempt(p)(ctx)
    if res.success:
      ParseResult[Option[T]](ctx: res.ctx, success: true, value: some(res.value))
    else:
      ParseResult[Option[T]](ctx: saved, success: true, value: none(T))

proc sepBy*[T, S](p: Parser[T], sep: Parser[S]): Parser[seq[T]] =
  let first = p
  first >>= (proc(v: T): Parser[seq[T]] =
    many0(sep *> p).map(vs => v & vs)
  ) <|> pure(newSeq[T]())

proc sepBy1*[T, S](p: Parser[T], sep: Parser[S]): Parser[seq[T]] =
  p >>= (proc(v: T): Parser[seq[T]] =
    many0(sep *> p).map(vs => v & vs)
  )

proc manyTill*[T, U](p: Parser[T], terminator: Parser[U]): Parser[seq[T]] =
  result = proc(ctx: ParserContext): ParseResult[seq[T]] =
    var
      curCtx = ctx.copy()
      acc: seq[T]
    while true:
      let termRes = attempt(terminator)(curCtx)
      if termRes.success:
        curCtx = termRes.ctx
        break
      let pRes = attempt(p)(curCtx)
      if not pRes.success:
        return ParseResult[seq[T]](ctx: curCtx, success: false, err: pRes.err)
      acc.add(pRes.value)
      curCtx = pRes.ctx.copy()
    ParseResult[seq[T]](ctx: curCtx, success: true, value: acc)

proc between*[T, U, V](open: Parser[U], p: Parser[T], close: Parser[V]): Parser[T] =
  open *> p <* close

proc sepEndBy*[T, S](p: Parser[T], sep: Parser[S]): Parser[seq[T]] =
  sepBy(p, sep) >>= (proc(vals: seq[T]): Parser[seq[T]] =
    opt(sep).map(_ => vals)
  )

proc choice*[T](parsers: openArray[Parser[T]]): Parser[T] =
  if parsers.len == 0: return fail[T]("no alternatives")
  else:
    result = parsers[0]
    for p in parsers[1..^1]:
      result = result <|> p

template zip*[A, B](p: Parser[A], q: Parser[B]): Parser[(A, B)] =
  p >>= (proc(a: A): Parser[(A, B)] =
    q.map(b => (a, b))
  )

template zipWith*[A, B, C](p: Parser[A], q: Parser[B], f: proc(a: A, b: B): C): Parser[C] =
  zip(p, q).map(t => f(t[0], t[1]))

proc filter*[T](p: Parser[T], predicate: T -> bool): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    let res = p(ctx)
    if not res.success or predicate(res.value):
      res
    else:
      ParseResult[T](ctx: res.ctx, success: false, err: newError(pos = res.ctx.pos, customMsg = "filter predicate failed"))

proc newline*(): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    let idx = ctx.pos.offset
    if idx >= ctx.input.len:
      return ParseResult[Unit](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, expected = expectedSet(["newline"]))
      )

    let ch = ctx.input[idx]
    if ch == '\r' and idx + 1 < ctx.input.len and ctx.input[idx + 1] == '\n':
      let (newCtx, _) = advanceRune(ctx)
      ParseResult[Unit](ctx: newCtx, success: true, value: ())
    elif ch == '\n' or ch == '\r':
      let (newCtx, _) = advanceRune(ctx)
      ParseResult[Unit](ctx: newCtx, success: true, value: ())
    else:
      ParseResult[Unit](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, expected = expectedSet(["newline"]))
      )