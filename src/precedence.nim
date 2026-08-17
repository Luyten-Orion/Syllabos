import std/[
  options,
  sugar
]

import syllabos/[
  combinators,
  core
]

proc chainl1*[T](p: Parser[T], op: Parser[(T, T) -> T]): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    let first = p(ctx)
    if not first.success:
      return ParseResult[T](ctx: ctx, success: false, err: first.err)
    var
      accum = first.value
      curCtx = first.ctx
    while true:
      let
        saved = curCtx.copy()
        opRes = attempt(op)(curCtx)
      if not opRes.success:
        curCtx = saved
        break
      let rightRes = attempt(p)(opRes.ctx)
      if not rightRes.success:
        return ParseResult[T](ctx: rightRes.ctx, success: false, err: rightRes.err)
      accum = opRes.value(accum, rightRes.value)
      curCtx = rightRes.ctx
    ParseResult[T](ctx: curCtx, success: true, value: accum)

proc chainr1*[T](p: Parser[T], op: Parser[(T, T) -> T]): Parser[T] =
  result = proc(ctx: ParserContext): ParseResult[T] =
    var
      curCtx = ctx
      values: seq[T]
      ops: seq[(T, T) -> T]
    let first = p(curCtx)
    if not first.success:
      return ParseResult[T](ctx: ctx, success: false, err: first.err)
    values.add(first.value)
    curCtx = first.ctx
    while true:
      let
        saved = curCtx.copy()
        opRes = attempt(op)(curCtx)
      if not opRes.success:
        curCtx = saved
        break
      let rightRes = attempt(p)(opRes.ctx)
      if not rightRes.success:
        return ParseResult[T](ctx: rightRes.ctx, success: false, err: rightRes.err)
      ops.add(opRes.value)
      values.add(rightRes.value)
      curCtx = rightRes.ctx
    var accum = values[^1]
    for i in countdown(values.len - 2, 0):
      accum = ops[i](values[i], accum)
    result.ctx = curCtx
    result.success = true
    result.value = accum

template chainl*[T](p: Parser[T], op: Parser[(T, T) -> T]): Parser[Option[T]] =
  chainl1(p, op).opt()

template chainr*[T](p: Parser[T], op: Parser[(T, T) -> T]): Parser[Option[T]] =
  chainr1(p, op).opt()