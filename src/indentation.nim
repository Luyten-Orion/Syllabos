import syllabos/[
  combinators,
  primitives,
  trivia,
  core
]

proc calculateIndent(ctx: ParserContext): (int, string) =
  if ctx.indentValid:
    return (ctx.cachedIndent, "")
  var idx = ctx.pos.offset
  while idx > 0 and ctx.input[idx-1] != '\n':
    idx -= 1
  var
    spaces = 0
    tabs = 0
  while idx < ctx.input.len and ctx.input[idx] in {' ', '\t'}:
    if ctx.input[idx] == ' ':
      spaces += 1
    else:
      tabs += 1
    idx += 1

  var effectiveMode = ctx.indentConfig.mode
  if effectiveMode == imAuto:
    if not ctx.indentHasDetected:
      if tabs > 0 and spaces == 0: effectiveMode = imTabs
      elif spaces > 0 and tabs == 0: effectiveMode = imSpaces
      else:
        if tabs > 0 and spaces > 0:
          return (0, "Mixed tabs and spaces in indentation")
        else:
          return (0, "")
    else:
      effectiveMode = ctx.indentDetectedMode

  if effectiveMode == imSpaces and tabs > 0:
    return (0, "Tabs are not allowed (spaces mode)")
  if effectiveMode == imTabs and spaces > 0:
    return (0, "Spaces are not allowed (tabs mode)")
  if tabs > 0 and spaces > 0:
    return (0, "Mixed tabs and spaces in indentation")

  let total = spaces + (tabs * ctx.indentConfig.tabWidth)
  return (total, "")

proc getIndent(ctx: ParserContext): (ParserContext, int, string) =
  let (indent, err) = calculateIndent(ctx)
  var newCtx = ctx
  if err.len == 0:
    newCtx.cachedIndent = indent
    newCtx.indentValid = true
  return (newCtx, indent, err)

proc sameIndent*(): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    let (newCtx, indent, err) = getIndent(ctx)
    if err.len > 0:
      return ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = "Indentation error: " & err))
    if newCtx.indentStack.len == 0:
      return ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = "Indentation stack empty"))
    if indent == newCtx.indentStack[^1]:
      ParseResult[Unit](ctx: newCtx, success: true, value: ())
    else:
      ParseResult[Unit](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, customMsg = "Indentation mismatch: expected " & $newCtx.indentStack[^1] & ", got " & $indent)
      )

proc indent*(): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    let (newCtx, indent, err) = getIndent(ctx)
    if err.len > 0:
      return ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = "Indentation error: " & err))
    if newCtx.indentStack.len == 0:
      return ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = "Indentation stack empty"))
    if indent > newCtx.indentStack[^1]:
      var newCtx2 = newCtx
      newCtx2.indentStack.add(indent)
      if newCtx2.indentConfig.mode == imAuto and not newCtx2.indentHasDetected:
        var idx = ctx.pos.offset
        while idx > 0 and ctx.input[idx-1] != '\n':
          idx -= 1
        var hasSpaces = false
        var hasTabs = false
        while idx < ctx.input.len and ctx.input[idx] in {' ', '\t'}:
          if ctx.input[idx] == ' ': hasSpaces = true
          else: hasTabs = true
          idx += 1
        if hasSpaces and hasTabs:
          return ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = "Mixed tabs and spaces in indentation"))
        if hasTabs and not hasSpaces:
          newCtx2.indentDetectedMode = imTabs
        elif hasSpaces and not hasTabs:
          newCtx2.indentDetectedMode = imSpaces
        else:
          newCtx2.indentDetectedMode = imSpaces
        newCtx2.indentHasDetected = true
      ParseResult[Unit](ctx: newCtx2, success: true, value: ())
    else:
      ParseResult[Unit](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, customMsg = "Expected greater indent than " & $newCtx.indentStack[^1] & ", got " & $indent)
      )

proc dedent*(): Parser[Unit] =
  result = proc(ctx: ParserContext): ParseResult[Unit] =
    let (newCtx, indent, err) = getIndent(ctx)
    if err.len > 0:
      ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = "Indentation error: " & err))
    elif newCtx.indentStack.len <= 1:
      ParseResult[Unit](ctx: ctx, success: false, err: newError(pos = ctx.pos, customMsg = "Cannot dedent below root level"))
    elif indent < newCtx.indentStack[^1]:
      var newCtx2 = newCtx
      discard newCtx2.indentStack.pop()
      ParseResult[Unit](ctx: newCtx2, success: true, value: ())
    else:
      ParseResult[Unit](
        ctx: ctx,
        success: false,
        err: newError(pos = ctx.pos, customMsg = "Expected dedent below " & $newCtx.indentStack[^1] & ", got " & $indent)
      )

proc line*[T](p: Parser[T]): Parser[T] =
  (sameIndent() *> p) <* opt(newline() *> skipHorizontal())

proc blockBody*[T](p: Parser[T]): Parser[seq[T]] =
  line(p).many0()

proc indentedBlock*[T](p: Parser[T]): Parser[seq[T]] =
  newline() *> skipHorizontal() *> indent() *> blockBody(p) <* dedent()