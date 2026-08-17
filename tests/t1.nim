import std/[strutils, unittest, options, math, sets]
import syllabos

type
  MyState = ref object of UserState
    counter: int
    name: string

proc newMyState(counter: int = 0, name: string = ""): MyState =
  MyState(counter: counter, name: name)

let
  digit = satisfy(proc(c: char): bool = c in {'0'..'9'}, "digit")
  number = many1(digit).map(proc(ds: seq[char]): int = parseInt(ds.join))

suite "core":
  test "Position":
    let
      pos = newPosition(10, 3, 5)
    check pos.offset == 10
    check pos.line == 3
    check pos.col == 5

  test "newContext":
    let
      ctx = newContext("abc")
    check ctx.input == "abc"
    check ctx.pos.offset == 0
    check ctx.pos.line == 1
    check ctx.pos.col == 1
    check ctx.indentStack == @[0]
    check ctx.indentConfig.mode == imAuto
    check ctx.indentConfig.tabWidth == 4

  test "copy":
    let
      ctx = newContext("abc")
      cpy = ctx.copy()
    check cpy.input == ctx.input
    check cpy.pos == ctx.pos

  test "advanceRune":
    var
      ctx = newContext("abc\n")
      ctx1, ctx2, ctx3, ctx4, ctx5: ParserContext
      ch1, ch2, ch3, ch4, ch5: char
    (ctx1, ch1) = advanceRune(ctx)
    check ch1 == 'a'
    check ctx1.pos.offset == 1
    check ctx1.pos.line == 1
    check ctx1.pos.col == 2

    (ctx2, ch2) = advanceRune(ctx1)
    check ch2 == 'b'
    check ctx2.pos.offset == 2
    check ctx2.pos.col == 3

    (ctx3, ch3) = advanceRune(ctx2)
    check ch3 == 'c'
    check ctx3.pos.offset == 3
    check ctx3.pos.col == 4

    (ctx4, ch4) = advanceRune(ctx3)
    check ch4 == '\n'
    check ctx4.pos.offset == 4
    check ctx4.pos.line == 2
    check ctx4.pos.col == 1
    check not ctx4.indentValid

    (ctx5, ch5) = advanceRune(ctx4)
    check ch5 == '\0'
    check ctx5.pos.offset == 4

suite "primitives":
  test "satisfy":
    let
      digit = satisfy(proc(c: char): bool = c in {'0'..'9'}, "digit")
      res1 = runParser(digit, "5abc")
      res2 = runParser(digit, "abc")
    check res1.success
    check res1.value == '5'
    check res1.ctx.pos.offset == 1
    check not res2.success
    check "digit" in res2.err.expected
    check res2.err.pos.offset == 0

  test "eof":
    let
      p = eof()
      res1 = runParser(p, "")
      res2 = runParser(p, "x")
    check res1.success
    check not res2.success
    check "end of input" in res2.err.expected

  test "pure":
    let
      p = pure(42)
      res = runParser(p, "anything")
    check res.success
    check res.value == 42
    check res.ctx.pos.offset == 0

  test "fail":
    let
      p = fail[int]("oops")
      res = runParser(p, "input")
    check not res.success
    check res.err.customMsg == "oops"

  test "getPosition":
    let
      p = getPosition()
      res = runParser(p, "hello")
    check res.success
    check res.value.line == 1
    check res.value.col == 1
    check res.value.offset == 0

  test "match":
    let
      p1 = match("hello")
      res1 = runParser(p1, "hello world")
      res2 = runParser(p1, "goodbye")
    check res1.success
    check res1.value == "hello"
    check res1.ctx.pos.offset == 5
    check not res2.success
    check "hello" in res2.err.expected

suite "combinators":
  test "andThen (>>=)":
    let
      p1 = match("a")
      p2 = proc(s: string): Parser[int] = pure(s.len)
      combined = p1 >>= p2
      res = runParser(combined, "abc")
    check res.success
    check res.value == 1
    check res.ctx.pos.offset == 1

  test "map":
    let
      p = match("123").map(parseInt)
      res = runParser(p, "123")
    check res.success
    check res.value == 123

  test "keepLeft (<*)":
    let
      p = match("a") <* match("b")
      res = runParser(p, "ab")
    check res.success
    check res.value == "a"
    check res.ctx.pos.offset == 2

  test "keepRight (*>)":
    let
      p = match("a") *> match("b")
      res = runParser(p, "ab")
    check res.success
    check res.value == "b"

  test "attempt":
    let
      p = attempt(match("ab")) <|> match("a")
      res = runParser(p, "ac")
    check res.success
    check res.value == "a"
    check res.ctx.pos.offset == 1

  test "alt (<|>)":
    let
      p = match("a") <|> match("b")
      res1 = runParser(p, "a")
      res2 = runParser(p, "b")
      res3 = runParser(p, "c")
    check res1.success
    check res1.value == "a"
    check res2.success
    check res2.value == "b"
    check not res3.success
    check "a" in res3.err.expected
    check "b" in res3.err.expected

  test "notFollowedBy":
    let
      p = match("a") *> notFollowedBy(match("b"))
      res1 = runParser(p, "ac")
      res2 = runParser(p, "ab")
    check res1.success
    check not res2.success

  test "many0":
    let
      p = many0(match("a"))
      res1 = runParser(p, "aaa")
      res2 = runParser(p, "b")
    check res1.success
    check res1.value == @["a", "a", "a"]
    check res1.ctx.pos.offset == 3
    check res2.success
    check res2.value.len == 0

  test "many1":
    let
      p = many1(match("a"))
      res1 = runParser(p, "aaa")
      res2 = runParser(p, "b")
    check res1.success
    check res1.value == @["a", "a", "a"]
    check not res2.success

  test "skipMany0":
    let
      p = skipMany0(match("a"))
      res = runParser(p, "aaab")
    check res.success
    check res.ctx.pos.offset == 3

  test "skipMany1":
    let
      p = skipMany1(match("a"))
      res1 = runParser(p, "aaab")
      res2 = runParser(p, "b")
    check res1.success
    check not res2.success

  test "opt":
    let
      p = opt(match("a"))
      res1 = runParser(p, "a")
      res2 = runParser(p, "b")
    check res1.success
    check res1.value == some("a")
    check res2.success
    check res2.value.isNone

  test "sepBy":
    let
      p = sepBy(match("a"), match(","))
      res1 = runParser(p, "a,a,a")
      res2 = runParser(p, "")
    check res1.success
    check res1.value == @["a", "a", "a"]
    check res1.ctx.pos.offset == 5
    check res2.success
    check res2.value.len == 0

  test "sepBy1":
    let
      p = sepBy1(match("a"), match(","))
      res1 = runParser(p, "a,a,a")
      res2 = runParser(p, "")
    check res1.success
    check not res2.success

  test "manyTill":
    let
      p = manyTill(match("a"), match(";"))
      res1 = runParser(p, "aaa;")
      res2 = runParser(p, "aaa")
    check res1.success
    check res1.value == @["a", "a", "a"]
    check res1.ctx.pos.offset == 4
    check not res2.success

  test "between":
    let
      p = between(match("("), match("x"), match(")"))
      res = runParser(p, "(x)")
    check res.success
    check res.value == "x"

  test "sepEndBy":
    let
      p = sepEndBy(match("a"), match(","))
      res1 = runParser(p, "a,a,")
      res2 = runParser(p, "a,a")
    check res1.success
    check res1.value == @["a", "a"]
    check res2.success
    check res2.value == @["a", "a"]

  test "choice":
    let
      p = choice([match("a"), match("b"), match("c")])
      res1 = runParser(p, "b")
      res2 = runParser(p, "d")
      pEmpty = choice(newSeq[Parser[string]]())
      res3 = runParser(pEmpty, "a")
    check res1.success
    check res1.value == "b"
    check not res2.success
    check not res3.success
    check res3.err.customMsg == "no alternatives"

  test "zip":
    let
      p = zip(match("a"), match("b"))
      res = runParser(p, "ab")
    check res.success
    check res.value == ("a", "b")

  test "zipWith":
    let
      p = zipWith(match("a"), match("b"), proc(a: string, b: string): string = a & b)
      res = runParser(p, "ab")
    check res.success
    check res.value == "ab"

  test "filter":
    let
      p = match("a").filter(proc(s: string): bool = s == "a")
      res1 = runParser(p, "a")
      res2 = runParser(p, "b")
    check res1.success
    check not res2.success

  test "newline":
    let
      p = newline()
      res1 = runParser(p, "\n")
      res2 = runParser(p, "\r\n")
      res3 = runParser(p, " ")
    check res1.success
    check res2.success
    check not res3.success

suite "trivia":
  test "spaceOrTab":
    let
      p = spaceOrTab()
      res1 = runParser(p, " ")
      res2 = runParser(p, "\t")
      res3 = runParser(p, "a")
    check res1.success
    check res2.success
    check not res3.success

  test "skipHorizontal":
    let
      p = match("x") <* skipHorizontal()
      res = runParser(p, "x   ")
    check res.success
    check res.ctx.pos.offset == 4

  test "lexeme":
    let
      p = lexeme(match("x"))
      res = runParser(p, "x   ")
    check res.success
    check res.ctx.pos.offset == 4

suite "precedence":
  test "chainl1":
    let
      addOp = match("+").map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a+b))
      subOp = match("-").map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a-b))
      op = addOp <|> subOp
      p = chainl1(number, op)
      res1 = runParser(p, "1+2+3")
      res2 = runParser(p, "1-2+3")
    check res1.success
    check res1.value == 6
    check res2.success
    check res2.value == 2

  test "chainr1":
    let
      op = match("^").map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a^b))
      p = chainr1(number, op)
      res = runParser(p, "2^3^2")
    check res.success
    check res.value == 512

  test "chainl / chainr":
    let
      op = match("+").map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a+b))
      p1 = chainl(number, op)
      p2 = chainr(number, op)
      res1 = runParser(p1, "1+2")
      res2 = runParser(p2, "1+2")
    check res1.success
    check res1.value == some(3)
    check res2.success
    check res2.value == some(3)

suite "errors":
  test "label":
    let
      p = match("a").label("an 'a'")
      res = runParser(p, "b")
    check not res.success
    check "an 'a'" in res.err.expected

  test "withMessage":
    let
      p = match("a").withMessage("not an a")
      res = runParser(p, "b")
    check not res.success
    check res.err.customMsg == "not an a"

  test "fatal":
    let
      p = match("a") <|> fatal(match("b"))
      res = runParser(p, "c")
    check not res.success
    check res.err.isFatal

  test "commit":
    let
      left = commit(match("a") *> match("b"))
      p = left <|> match("a")
      res = runParser(p, "ac")
    check not res.success
    check res.err.isFatal

  test "formatError":
    let
      err = newError(pos = newPosition(3, 1, 4), expected = expectedSet(["foo"]), customMsg = "bar")
      fmt = formatError("line one\nabc", err)
    check fmt.contains("line 1, column 4")
    check fmt.contains("line one")
    check fmt.contains("^")
    check (fmt.contains("foo") or fmt.contains("bar"))

suite "state":
  test "modifyState, getState, setState":
    block:
      let
        initState = newMyState(counter=10, name="init")
        p = modifyState(proc(s: UserState): UserState =
          let ms = MyState(s)
          ms.counter += 5
          ms.name = "modified"
          ms)
        res = runParser(p, "", state=initState)
      check res.success
      let st = MyState(res.ctx.state)
      check st.counter == 15
      check st.name == "modified"

    block:
      let
        initState = newMyState(counter=10, name="init")
        p = getState(MyState)
        res = runParser(p, "", state=initState)
      check res.success
      check res.value.counter == 10
      check res.value.name == "init"

    block:
      let
        initState = newMyState(counter=10, name="init")
        newState = newMyState(99, "new")
        p = setState(newState)
        res = runParser(p, "", state=initState)
      check res.success
      let st = MyState(res.ctx.state)
      check st.counter == 99
      check st.name == "new"

    block:
      let
        p = getState(MyState)
        res = runParser(p, "", state=UserState())
      check not res.success
      check res.err.customMsg == "State type mismatch"

suite "indentation":
  test "sameIndent":
    let
      p = sameIndent()
      res = runParser(p, "a")
    check res.success

  test "indentedBlock":
    let
      p = indentedBlock(match("x"))
      res = runParser(p, "\n  x\n  x\n")
    check res.success
    check res.value == @["x", "x"]

  test "line":
    let
      p = line(match("x"))
      res = runParser(p, "x\n")
    check res.success
    check res.value == "x"
    check res.ctx.pos.offset == 2

suite "integration":
  test "arithmetic with whitespace":
    let
      num = lexeme(number)
      addOp = lexeme(match("+")).map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a+b))
      subOp = lexeme(match("-")).map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a-b))
      mulOp = lexeme(match("*")).map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a*b))
      divOp = lexeme(match("/")).map(proc(s: string): proc(a,b:int):int =
        (proc(a,b:int):int = a div b))
      op = addOp <|> subOp <|> mulOp <|> divOp
      expr = chainl1(num, op)
      res = runParser(expr, "1 + 2 * 3")
    check res.success
    check res.value == 9

  test "parenthesized expression":
    let
      num = lexeme(number)
      parenNum = between(match("("), num, match(")"))
      res = runParser(parenNum, "(1)")
    check res.success
    check res.value == 1