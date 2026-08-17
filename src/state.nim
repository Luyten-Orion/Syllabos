import syllabos/[
  core
]

proc modifyState*(f: proc(s: UserState): UserState): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    var newCtx = ctx
    newCtx.state = f(newCtx.state)
    ParseResult[Unit](ctx: newCtx, success: true, value: ())

proc getState*[S: UserState](_: typedesc[S]): Parser[S] =
  result = proc(ctx: ParserContext): ParseResult[S] =
    if ctx.state of S:
      ParseResult[S](ctx: ctx, success: true, value: S(ctx.state))
    else:
      ParseResult[S](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, customMsg = "State type mismatch")
      )

proc setState*(s: UserState): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    var newCtx = ctx
    newCtx.state = s
    ParseResult[Unit](ctx: newCtx, success: true, value: ())