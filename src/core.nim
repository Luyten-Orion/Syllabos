import std/[
  unicode,
  sugar,
  sets
]

type
  Unit* = tuple[]

  Position* = object
    offset*: int
    line*: int
    col*: int

  UserState* = ref object of RootObj

  ParserContext* = object
    input*: string
    pos*: Position
    state*: UserState
    indentStack*: seq[int]
    indentConfig*: IndentConfig
    indentDetectedMode*: IndentMode
    indentHasDetected*: bool
    cachedIndent*: int
    indentValid*: bool

  IndentMode* = enum
    imSpaces
    imTabs
    imAuto

  IndentConfig* = object
    mode*: IndentMode
    tabWidth*: int

  ParseError* = object
    pos*: Position
    expected*: HashSet[string]
    customMsg*: string
    isFatal*: bool

  ParseResult*[T] = object
    ctx*: ParserContext
    case success*: bool
    of true:
      value*: T
    of false:
      err*: ParseError

  Parser*[T] = ParserContext -> ParseResult[T]

func newPosition*(offset, line, col: int): Position =
  Position(offset: offset, line: line, col: col)

func newContext*(
  input: string,
  state = UserState(),
  config = IndentConfig(mode: imAuto, tabWidth: 4)
): ParserContext =
  ParserContext(
    input: input,
    pos: newPosition(0, 1, 1),
    state: state,
    indentStack: @[0],
    indentConfig: config,
    indentDetectedMode: imSpaces,
    indentHasDetected: false,
    cachedIndent: 0,
    indentValid: false
  )

proc copy*(ctx: ParserContext): ParserContext {.inline.} =
  result = ctx
  result.state = deepCopy(ctx.state)

func newError*(
  pos: Position,
  expected = initHashSet[string](),
  customMsg = "",
  isFatal: bool = false
): ParseError =
  ParseError(pos: pos, expected: expected, customMsg: customMsg, isFatal: isFatal)

func expectedSet*(items: openArray[string]): HashSet[string] =
  result = initHashSet[string]()
  for item in items:
    result.incl(item)

func mergeErrors*(a, b: ParseError): ParseError =
  if a.pos.offset > b.pos.offset: return a
  elif b.pos.offset > a.pos.offset: return b
  else:
    result = a
    result.expected = a.expected + b.expected
    if a.customMsg.len == 0 and b.customMsg.len > 0:
      result.customMsg = b.customMsg
    elif a.customMsg.len > 0 and b.customMsg.len == 0:
      result.customMsg = a.customMsg
    elif a.customMsg.len > 0 and b.customMsg.len > 0 and a.customMsg != b.customMsg:
      result.customMsg = a.customMsg & " OR " & b.customMsg
    result.isFatal = a.isFatal or b.isFatal

proc advanceRune*(ctx: ParserContext): (ParserContext, char) =
  let idx = ctx.pos.offset
  if idx >= ctx.input.len:
    return (ctx, '\0')

  let ch = ctx.input[idx]
  var newCtx = ctx

  if ch == '\r' and idx + 1 < ctx.input.len and ctx.input[idx + 1] == '\n':
    newCtx.pos.offset = idx + 2
    newCtx.pos.line += 1
    newCtx.pos.col = 1
    newCtx.indentValid = false
    return (newCtx, '\n')

  elif ch == '\n' or ch == '\r':
    newCtx.pos.offset = idx + 1
    newCtx.pos.line += 1
    newCtx.pos.col = 1
    newCtx.indentValid = false
    return (newCtx, ch)

  else:
    var
      runeVal: Rune
      newIdx = idx
    fastRuneAt(ctx.input, newIdx, runeVal)
    newCtx.pos.offset = newIdx
    newCtx.pos.col += 1
    return (newCtx, ch)