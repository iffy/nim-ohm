import
  strutils, lexbase, sugar, macros, strformat, streams, sequtils

#-------------------------------------------------------------------
# Ohm lexer
#-------------------------------------------------------------------
type
  TokKind* = enum    ## enumeration of all Ohm tokens
    tkError,        ## error sentinel
    tkEOF,          ## end of file reached
    tkIdentifier,   ## abc
    tkCurlyOpen,    ## '{'
    tkCurlyClose,   ## '}'
    tkEqual,        ## '='
    tkPipe,         ## '|'
    tkString,       ## "abc"
    tkDot,          ## '.'
    tkPlus,         ## '+'
    tkStar,         ## '*'
    tkParenOpen,    ## '('
    tkParenClose,   ## ')'
    tkQuestion,     ## '?'
    tkAngleOpen,    ## '<'
    tkAngleClose,   ## '>'
    tkComma,        ## ','
    tkAmpersand,    ## '&'
    tkTilde,        ## '~'
    tkHash,         ## '#'
    tkColon,        ## ':'
    tkDoubleHyphen, ## "--"
    tkNewline,      ## '\n'
    tkConcat,       ## concat operator
  
  OError* = enum
    errEOC_Expected,  ## ``*/`` expected
    errQuoteExpected, ## ``"`` expected
  
  Token = object
    kind: TokKind     # the type of the token
    literal: string   # the parsed (string) literal
  
  OLexer* = object of BaseLexer
    val*: string
    tok*: TokKind
    err*: OError
    filename: string

const
  ## synchronized with TokKind above
  tokKindToStr: array[TokKind, string] = [
    "error",
    "[EOF]",
    "identifier",
    "{",
    "}",
    "=",
    "|",
    "string",
    ".",
    "+",
    "*",
    "(",
    ")",
    "?",
    "<",
    ">",
    ",",
    "&",
    "~",
    "#",
    ":",
    "--",
    "newline",
    "concat"
  ]

  # https://github.com/harc/ohm/blob/master/doc/syntax-reference.md#built-in-rules
  reservedKeywords = @[
    "any",
    "letter",
    "lower",
    "upper",
    "digit",
    "hexDigit",
    "alnum",
    "space",
    "end",
    "caseInsensitive",
    "ListOf",
    "NonemptyListOf",
    "listOf",
  ]

proc close(my: var OLexer) =
  lexbase.close(my)

# proc handleCR(L: var OLexer, pos: int): int =
#   discard

# proc handleLF(L: var OLexer, pos: int): int =
#   discard

# proc handleRefillChar(L: var OLexer, pos: int): int =
#   discard

proc open(my: var OLexer, input: Stream) =
  lexbase.open(my, input)

proc getColumn(my: OLexer): int =
  ## Get the current column the parser has arrived at
  result = getColNumber(my, my.bufpos)

proc getLine(my: OLexer): int =
  ## Get the current line number
  result = my.lineNumber

proc parseString(my: var OLexer): TokKind =
  result = tkString
  var pos = my.bufpos + 1 # +1 is to skip the quote
  var buf = my.buf
  while true:
    case buf[pos]
    of '\0':
      my.err = errQuoteExpected
      result = tkError
    of '"':
      inc(pos)
      break
    of '\c':
      pos = my.handleCR(pos)
      buf = my.buf
      my.val.add('\c')
    of '\L':
      pos = my.handleLF(pos)
      buf = my.buf
      my.val.add('\L')
    of '\\':
      case buf[pos+1]
      # \" \\ \b \f \n \r \t \xFF \u0000
      of '\\', '"', '\'':
        my.val.add(buf[pos+1])
        inc(pos, 2)
      of 'b':
        my.val.add('\b')
        inc(pos, 2)
      of 'f':
        my.val.add('\f')
        inc(pos, 2)
      of 'n':
        my.val.add('\L')
        inc(pos, 2)
      of 'r':
        my.val.add('\C')
        inc(pos, 2)
      of 't':
        my.val.add('\t')
        inc(pos, 2)
      of 'x':
        # XXX Not supported yet
        result = tkError
        break
      of 'u':
        # XXX Not supported yet
        result = tkError
        break
      else:
        echo "else? ", buf[pos+1].repr
    else:
      my.val.add(buf[pos])
      inc(pos)
  my.bufpos = pos

proc parseName(my: var OLexer) =
  var pos = my.bufpos
  var buf = my.buf
  if buf[pos] in IdentStartChars:
    while buf[pos] in IdentChars:
      add(my.val, buf[pos])
      inc(pos)
  # echo "my.val: ", my.val.repr
  my.bufpos = pos

proc skip*(my: var OLexer) =
  ## Skip whitespace and comments
  var pos = my.bufpos
  var buf = my.buf
  while true:
    # echo "skipchar: ", buf[pos].repr
    case buf[pos]
    of '/':
      if buf[pos+1] == '/':
        # line comment
        inc(pos, 2)
        while not (buf[pos] in {'\c', '\L', lexbase.EndOfFile}): inc(pos)
      elif buf[pos+1] == '*':
        # multi-line comment
        inc(pos, 2)
        while true:
          case buf[pos]
          of '\0':
            my.err = errEOC_Expected
            break
          of '\c':
            pos = my.handleCR(pos)
            buf = my.buf
          of '\L':
            pos = my.handleLF(pos)
            buf = my.buf
          of '*':
            inc(pos)
            if buf[pos] == '/':
              inc(pos)
              break
          else:
            inc(pos)
      else:
        break
    of ' ', '\t':
      inc(pos)
    else:
      break
  my.bufpos = pos

proc getTok*(my: var OLexer): TokKind =
  ## Returns the next token from the stream
  my.val.setLen(0)
  skip(my)
  # echo "char: ", my.buf[my.bufpos].repr
  case my.buf[my.bufpos]
  of 'a'..'z', 'A'..'Z', '_':
    parseName(my)
    result = tkIdentifier
  of '\0':
    result = tkEOF
  of '"':
    result = parseString(my)
  of '\L', '\c':
    result = tkNewline
    inc(my.bufpos)
  of '{':
    result = tkCurlyOpen
    inc(my.bufpos)
  of '}':
    result = tkCurlyClose
    inc(my.bufpos)
  of '=':
    result = tkEqual
    inc(my.bufpos)
  of '|':
    result = tkPipe
    inc(my.bufpos)
  of '.':
    result = tkDot
    inc(my.bufpos)
  of '+':
    result = tkPlus
    inc(my.bufpos)
  of '*':
    result = tkStar
    inc(my.bufpos)
  of '(':
    result = tkParenOpen
    inc(my.bufpos)
  of ')':
    result = tkParenClose
    inc(my.bufpos)
  of '?':
    result = tkQuestion
    inc(my.bufpos)
  of '<':
    result = tkAngleOpen
    inc(my.bufpos)
  of '>':
    result = tkAngleClose
    inc(my.bufpos)
  of ',':
    result = tkComma
    inc(my.bufpos)
  of '&':
    result = tkAmpersand
    inc(my.bufpos)
  of '~':
    result = tkTilde
    inc(my.bufpos)
  of '#':
    result = tkHash
    inc(my.bufpos)
  of ':':
    result = tkColon
    inc(my.bufpos)
  of '-':
    if my.buf[my.bufpos+1] == '-':
      result = tkDoubleHyphen
      inc(my.bufpos, 2)
    else:
      result = tkError
      inc(my.bufpos, 1)
  else:
    inc(my.bufpos)
    result = tkError
  my.tok = result

proc initLexer*(s:string): OLexer =
  let stream = newStringStream(s)
  result = OLexer()
  result.open(stream)

proc newToken(kind:TokKind, literal:string = "") : Token =
  result = Token()
  result.kind = kind
  result.literal = literal

iterator tokens*(my: var OLexer): Token =
  while my.tok != tkEOF:
    discard my.getTok()
    yield newToken(my.tok, my.val)
    if my.tok == tkError:
      break

#-------------------------------------------------------------------
# Ohm parser
#-------------------------------------------------------------------

type
  OGrammar* = object
    name*: string
    tree*: OGrammarNode
  
  OGrammarNodeKind* = enum
    OGrammarKind,
    ORuleDef,
    # Expression nodes
    OTerm,
    OIdentifier,
    ORepetition,
    ONamedNode,
    OConcat,
    OAlternation,

  OGrammarNode* = ref OGrammarNodeObj
  OGrammarNodeObj* {.acyclic.} = object
    parent*: OGrammarNode
    children*: seq[OGrammarNode]
    case kind*: OGrammarNodeKind
    of OGrammarKind:
      grammar_name*: string
    of ORuleDef:
      rule_name*: string
    of ORepetition:
      repetition_kind*: ORepetitionKind
    of OTerm:
      term*: string
    of ONamedNode, OIdentifier:
      identifier*: string
    else:
      discard

  OTreeBuilder* = object of RootObj
    resultstack*: seq[OGrammarNode]
    operatorstack*: seq[Token]
  
  OExprBuilder* = object of OTreeBuilder
    lasttoken: TokKind
  
  ORepetitionKind* = enum
    OZeroOrMore,
    OOneOrMore,
    OZeroOrOne,

# proc throwup(kind:TokKind, val:string) =
#   raise newException(CatchableError, &"Unexpected token: {kind} {val.repr}")

# proc newEmpty(): OGrammarNode =
#   new(result)
#   result.kind = OEmpty

# proc newGenericGroup(children: seq[OGrammarNode] = @[]): OGrammarNode =
#   new(result)
#   result.kind = OGenericGroup
#   result.children = children

proc newConcat(children: seq[OGrammarNode] = @[]): OGrammarNode =
  new(result)
  result.kind = OConcat
  result.children = children

proc newAlternation(children: seq[OGrammarNode] = @[]): OGrammarNode =
  new(result)
  result.kind = OAlternation
  result.children = children

proc newIdentifier(name: string): OGrammarNode =
  new(result)
  result.kind = OIdentifier
  result.identifier = name

proc newTerm*(val: string): OGrammarNode =
  new(result)
  result.kind = OTerm
  result.term = val

proc newRepetition(kind: ORepetitionKind, child: OGrammarNode): OGrammarNode =
  new(result)
  result.kind = ORepetition
  result.repetition_kind = kind
  result.children = @[child]

proc newNamedNode(name: string, child: OGrammarNode): OGrammarNode =
  new(result)
  result.kind = ONamedNode
  result.identifier = name
  result.children = @[child]

proc newExprBuilder*(): OExprBuilder =
  result = OExprBuilder(resultstack: @[], operatorstack: @[])
  result.lasttoken = tkError

proc `$`*(node: OGrammarNode): string =
  case node.kind
  of OTerm:
    # XXX do this right
    result = &"\"{node.term}\""
  of OConcat:
    for child in node.children:
      if result.len > 0:
        result.add(" ")
      if child.children.len > 1:
        result.add(&"({child})")
      else:
        result.add(&"{child}")
  of OAlternation:
    for child in node.children:
      if result.len > 0:
        result.add(" | ")
      result.add($child)
  of ORepetition:
    var suffix = ""
    case node.repetition_kind
    of OOneOrMore:
      suffix = "+"
    of OZeroOrMore:
      suffix = "*"
    of OZeroOrOne:
      suffix = "?"
    if node.children[0].children.len > 0:
      result = &"({node.children[0]}){suffix}"
    else:
      result = &"{node.children[0]}{suffix}"
  of OIdentifier:
    result = node.identifier
  of ONamedNode:
    result = &"{node.children[0]} -- {node.identifier}"
  else:
    raise newException(CatchableError, &"Don't know how to display: {node.kind}")

proc remove*(group: var OGrammarNode, child: var OGrammarNode) =
  let idx = group.children.find(child)
  group.children.delete(idx)
  child.parent = nil

proc add*(group: var OGrammarNode, child: var OGrammarNode) =
  if child.parent != nil:
    child.parent.remove(child)
  group.children.add(child)
  child.parent = group

#--------------------------------------------------
# Expression Builder
#--------------------------------------------------

proc newNode(kind: TokKind, val: string): OGrammarNode =
  result = OGrammarNode()
  case kind:
  of tkString:
    result.kind = OTerm
    result.term = val
  else:
    raise newException(CatchableError, &"Unknown node type: {kind}")

proc printTree*(node: OGrammarNode): string;

proc root*(ex: OTreeBuilder): OGrammarNode =
  if ex.resultstack.len > 0:
    return ex.resultstack[0]
  else:
    return nil

proc `$`*(ex: OTreeBuilder): string =
  result = &"Tree: {ex.root}"

proc performOp(b: var OExprBuilder, op: Token) =
  case op.kind
  of tkConcat:
    var argb = b.resultstack.pop()
    var arga = b.resultstack.pop()
    if arga.kind == OConcat:
      arga.add(argb)
      b.resultstack.add(arga)
    elif argb.kind == OConcat:
      var cc = newConcat()
      cc.add(arga)
      while argb.children.len > 0:
        var child = argb.children[0]
        argb.remove(child)
        cc.add(child)
      b.resultstack.add(cc)
    else:
      b.resultstack.add(newConcat(@[arga, argb]))
  of tkPipe:
    let argb = b.resultstack.pop()
    let arga = b.resultstack.pop()
    b.resultstack.add(newAlternation(@[arga, argb]))
  of tkPlus:
    b.resultstack.add(newRepetition(OOneOrMore, b.resultstack.pop()))
  of tkStar:
    b.resultstack.add(newRepetition(OZeroOrMore, b.resultstack.pop()))
  of tkQuestion:
    b.resultstack.add(newRepetition(OZeroOrOne, b.resultstack.pop()))
  of tkDoubleHyphen:
    let name = b.resultstack.pop()
    let child = b.resultstack.pop()
    b.resultstack.add(newNamedNode(name.identifier, child))
  else:
    raise newException(CatchableError, &"TODO helicopter {op.kind}")

proc popOperatorsUntil*(b: var OExprBuilder, until: set[TokKind]) =
  while b.operatorstack.len > 0:
    let top = b.operatorstack[^1]
    if top.kind in until:
      break
    else:
      b.performOp(b.operatorstack.pop())

proc popOperatorsWhileTopIs*(b: var OExprBuilder, theset: set[TokKind]) =
  while b.operatorstack.len > 0:
    let top = b.operatorstack[^1]
    if top.kind in theset:
      b.performOp(b.operatorstack.pop())
    else:
      break

const tokensThatMakeConcatVals = {tkIdentifier, tkString}

proc add*(b: var OExprBuilder, token: Token) =
  # https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  case token.kind
  of tkIdentifier:
    if b.lasttoken != tkError and b.lasttoken in tokensThatMakeConcatVals:
      b.add(newToken(tkConcat))
    b.resultstack.add(newIdentifier(token.literal))
  of tkString:
    if b.lasttoken != tkError and b.lasttoken in tokensThatMakeConcatVals:
      b.add(newToken(tkConcat))
    b.resultstack.add(newTerm(token.literal))
  of tkConcat:
    b.operatorstack.add(token)
  of tkParenOpen:
    if b.lasttoken != tkError and b.lasttoken in tokensThatMakeConcatVals:
      b.add(newToken(tkConcat))
    b.operatorstack.add(token)
  of tkParenClose:
    b.popOperatorsUntil({tkParenOpen})
    discard b.operatorstack.pop()
  of tkPipe:
    b.popOperatorsWhileTopIs({tkConcat, tkDoubleHyphen})
    if b.resultstack.len > 0:
      b.operatorstack.add(token)
  of tkPlus, tkStar, tkQuestion:
    b.performOp(token)
  of tkDoubleHyphen:
    b.popOperatorsUntil({tkPipe})
    b.operatorstack.add(token)
  else:
    raise newException(CatchableError, &"TODO giraffe {token.kind}")
  b.lasttoken = token.kind

proc add*(builder: var OExprBuilder, kind: TokKind, val: string = "") =
  add(builder, newToken(kind, val))

proc finish*(builder: var OExprBuilder) =
  builder.popOperatorsUntil({})

proc nodes*(node: OGrammarNode, level: int = 0): seq[(int, OGrammarNode)] =
  result.add((level, node))
  for child in node.children:
    for subchild in child.nodes(level + 1):
      result.add(subchild)

proc complete*(builder: OTreeBuilder): bool =
  result = (builder.resultstack.len == 1 and builder.operatorstack.len == 0)

proc printTree*(node: OGrammarNode): string =
  for x in node.nodes():
    result.add("..".repeat(x[0]))
    result.add($x[1].kind)
    result.add(" ")
    result.add($x[1])
    result.add("\L")


# proc alternate(obj: var OGrammarNode) : OGrammarNode =
#   case obj.kind
#   of OAlternation
# proc add(obj: var OGrammarNode, operator: TokKind) : OGrammarNode =
#   result = obj
#   case operator:
#   of tkPipe:
#     case obj.kind:
#     of OAlternation:
#       discard
#     of OConcat:
#       discard
#     else:
#     discard
#   else:
#     raise newException(CatchableError, &"Can't add type: {operator}")

# proc add(obj: var OGrammarNode, thing: OGrammarNode) : OGrammarNode =
#   result = obj
#   case obj.kind
#   of OAlternation:
#     case thing.kind:
#     of OAlternation
#       raise newException(CatchableError, &"Can't add Alternation to Alternation")
#     obj.children.add(thing)
#   else:
#       raise newException(CatchableError, &"Node does not support multiple children: {obj.repr} while attempting to add {thing.repr}")

# proc parseGrammar*(s:string): OGrammar =
#   var lexer = initLexer(s)
#   result = OGrammar()
#   var rule: ORuleDef
#   var state: seq[ParserState] = @[stateBetweenGrammars]
#   var i = state.len-1
  
#   for kind, val in lexer.tokens:
#     echo state[i], " ", kind, ": ", val
#     if rule.name != "":
#       echo "rule: ", rule
#     case state[i]
#     of stateBetweenGrammars:
#       case kind
#       of tkIdentifier:
#         state[i] = stateHasGrammarName
#         result.name = val
#       of tkNewline:
#         discard
#       else:
#         throwup(kind, val)
#     of stateHasGrammarName:
#       case kind
#       of tkCurlyOpen:
#         state[i] = stateExpectRule
#       else:
#         throwup(kind, val)
#     of stateExpectRule:
#       case kind
#       of tkNewline:
#         discard
#       of tkIdentifier:
#         state[i] = stateHasRuleName
#         rule = newRule(val)
#         result.rules.add(rule)
#       else:
#         throwup(kind, val)
#     of stateHasRuleName:
#       case kind
#       of tkNewline:
#         discard
#       of tkEqual:
#         state[i] = stateExpectExpr
#       of tkParenOpen:
#         state[i] = stateInDescription
#       else:
#         throwup(kind, val)
#     of stateInDescription:
#       case kind
#       of tkParenClose:
#         state[i] = stateHasRuleName
#       else:
#         if rule.description.len > 0:
#           rule.description.add(" ")
#         rule.description.add(val)
#     of stateExpectExpr, stateExpectRuleOrExpr:
#       rule.expression.add(kind, val)
#       if rule.expression.complete():
#         state[i] = stateExpectRuleOrExpr
#       else:
#         state[i] = stateExpectExpr
#     of stateEOF:
#       if kind == tkEOF:
#         discard
#       else:
#         raise newException(CatchableError, "EOF Error")
    

#-------------------------------------------------------------------
# other grammar-makers
#-------------------------------------------------------------------

macro makeLexer*(name: string, grammardef: string): untyped =
  # NOTE: I could make a crummy parser here that would run at
  # compile time and produce speedy parsers :)
  echo &"makeLexer {name} start"
  result = newTree(nnkStmtList,
    newTree(nnkTypeSection,
      newTree(nnkTypeDef,
        # TokKind enum
        newIdentNode(&"{name}_TokKind"),
        newEmptyNode(),
        newTree(nnkEnumTy,
          newEmptyNode(),
          newIdentNode("First"),
          newIdentNode("Second"),
          newIdentNode("Third"),
        ),
      ),
    ),
    newTree(nnkConstSection,
      newTree(nnkConstDef,
        newIdentNode(&"{name}_reservedKeywords"),
        newEmptyNode(),
        newLit(@["foo", "bar"])
      ),
    ),
    newProc(newIdentNode(&"{name}Parse"), body = newTree(nnkStmtList,
      newCall(bindSym"writeLine", bindSym"stdout", name)
    )),
  )
  echo &"makeLexer {name} end"
  # (
  #   nnkTypeDef(
  #     nnkIdent("A"),
  #     nnkEmpty(),
  #     nnkEnumTy(
  #       nnkEmpty(),
  #       nnkIdent("First"),
  #     )
  #   )
  # )
  # var something = ""
  # result = newNimNode(nnkStmtList, n)
  # echo "grammardef", grammardef
  # result.add newCall(bindSym"writeLine", bindSym"stdout", toStrLit(grammardef))

# proc match*(grammar: Grammar, input: string): Match =
#   result = Match()

# proc ok*(match: Match): bool =
#   return true
