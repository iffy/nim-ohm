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
  
  OError* = enum
    errEOC_Expected,  ## ``*/`` expected
    errQuoteExpected, ## ``"`` expected
  
  Token = object
    kind: TokKind     # the type of the token
    literal: string   # the parsed (string) literal
    err: OError     # error, if any
  
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
    "newline"
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

iterator tokens*(my: var OLexer): (TokKind, string) =
  while my.tok != tkEOF:
    discard my.getTok()
    yield (my.tok, my.val)
    if my.tok == tkError:
      break

#-------------------------------------------------------------------
# Ohm parser
#-------------------------------------------------------------------

type
  OGrammar* = object
    name*: string
    rules*: seq[ORuleDef]
  
  ORuleDef* = object
    name*: string
    description*: string
    expression*: OExprBuilder
  
  OExprBuilder* = object
    stack*: OExprNode
    cursor*: OExprNode
    incomplete*: int

  OBuilderState* = enum
    stateInit,
    stateMaybeMore,

  OExprKind* = enum
    OTerminal,
    ORule,
    OGroup,
    OEmpty,
    ORepetition,
  
  OGroupKind* = enum
    OAlternation,
    OConcat,
  
  ORepetitionKind* = enum
    OZeroOrMore,
    OOneOrMore,
    OZeroOrOne,

  OExprNode* = ref OExprNodeObj
  OExprNodeObj* {.acyclic.} = object
    parent*: OExprNode
    case kind*: OExprKind
    of OTerminal:
      val*: string
    of ORule:
      rule*: string
    of OGroup:
      children*: seq[OExprNode]
      group_kind*: OGroupKind
    of ORepetition:
      child*: OExprNode
      repetition_kind*: ORepetitionKind
    of OEmpty:
      discard
  
  ParserState = enum
    stateBetweenGrammars,
    stateEOF,
    stateHasGrammarName,
    stateExpectRule,
    stateHasRuleName,
    stateInDescription,
    stateExpectExpr,
    stateExpectRuleOrExpr,

  Match* = object

proc throwup(kind:TokKind, val:string) =
  raise newException(CatchableError, &"Unexpected token: {kind} {val.repr}")

proc newEmpty(): OExprNode =
  new(result)
  result.kind = OEmpty

proc newConcat(children: seq[OExprNode] = @[]): OExprNode =
  new(result)
  result.kind = OGroup
  result.group_kind = OConcat
  result.children = children

proc newAlternation(children: seq[OExprNode] = @[]): OExprNode =
  new(result)
  result.kind = OGroup
  result.group_kind = OAlternation
  result.children = children

proc newRuleApplication(name: string): OExprNode =
  new(result)
  result.kind = ORule
  result.rule = name

proc newTerminal*(val: string): OExprNode =
  new(result)
  result.kind = OTerminal
  result.val = val

proc newRepetition(kind: ORepetitionKind, child: OExprNode): OExprNode =
  new(result)
  result.kind = ORepetition
  result.repetition_kind = kind
  result.child = child

proc newExprBuilder*(): OExprBuilder =
  result = OExprBuilder()
  result.cursor = newEmpty()
  result.incomplete = 1

proc newRule(name: string): ORuleDef =
  result = ORuleDef()
  result.name = name
  result.expression = newExprBuilder()

proc `$`*(node: OExprNode): string =
  case node.kind
  of OTerminal:
    # XXX do this right
    result = &"\"{node.val}\""
  of OGroup:
    var joiner = " "
    case node.group_kind
    of OConcat:
      joiner = " "
    of OAlternation:
      joiner = " | "
    for child in node.children:
      if result.len > 0:
        result.add(joiner)
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
    if node.child.kind == OGroup:
      result = &"({node.child}){suffix}"
    else:
      result = &"{node.child}{suffix}"
  of OEmpty:
    result = "<empty>"
  of ORule:
    result = node.rule

proc `$`*(rule: ORuleDef): string =
  result = &"{rule.name} = {$rule.expression}"

proc add*(group: var OExprNode, child: var OExprNode) =
  case group.kind
  of OGroup:
    group.children.add(child)
    child.parent = group
  of ORepetition:
    group.child = child
    child.parent = group
  else:
    raise newException(CatchableError, &"Can't add to {group.kind}")

proc remove*(group: var OExprNode, child: var OExprNode) =
  case group.kind
  of OGroup:
    let idx = group.children.find(child)
    group.children.delete(idx)
    child.parent = nil
  of ORepetition:
    group.child = nil
    child.parent = nil
  else:
    raise newException(CatchableError, &"Can't remove from {group.kind}")

proc replace(oldchild: var OExprNode, newchild: var OExprNode) =
  if oldchild.parent == nil:
    discard
  else:
    var parent = oldchild.parent
    parent.remove(oldchild)
    parent.add(newchild)

#--------------------------------------------------
# Expression Builder
#--------------------------------------------------

proc newNode(kind: TokKind, val: string): OExprNode =
  result = OExprNode()
  case kind:
  of tkString:
    result.kind = OTerminal
    result.val = val
  of tkIdentifier:
    result.kind = ORule
    result.rule = val
  else:
    raise newException(CatchableError, &"Unknown node type: {kind}")

proc printTree*(node: OExprNode): string;
proc root*(ex: OExprBuilder): OExprNode;

proc add*(builder: var OExprBuilder, kind: TokKind, val: string = "") =
  var a = builder.cursor
  # echo ""
  # echo "ADDING ", "+ ", kind, " ", val
  # echo ""
  # echo printTree(builder.root)
  # echo "a = ", a
  case a.kind
  of OEmpty:
    # empty + ...
    case kind
    of tkString, tkIdentifier:
      # ... terminal | rule
      builder.cursor = newNode(kind, val)
      a.replace(builder.cursor)
      dec(builder.incomplete) # for removed empty
    of tkParenOpen:
      # ... (
      builder.cursor = newEmpty()
      inc(builder.incomplete) # for the new empty
      var group = newConcat()
      group.add(builder.cursor)
      a.replace(group)
      dec(builder.incomplete) # for removed empty
      inc(builder.incomplete) # for the (
    of tkPipe, tkNewline:
      # ... pipe | \L
      discard
    else:
      raise newException(CatchableError, &"TODO 0 {kind}")
  of OTerminal, ORule, OGroup:
    # (terminal | rule) + ...
    case kind
    of tkString, tkIdentifier:
      # ... terminal | rule
      builder.cursor = newNode(kind, val)
      if a.parent == nil:
        var group = newConcat()
        group.add(a)
        group.add(builder.cursor)
      else:
        # has a parent already
        case a.parent.kind
        of OGroup:
          case a.parent.group_kind
          of OConcat:
            a.parent.add(builder.cursor)
          else:
            raise newException(CatchableError, &"TODO 1 {a.parent.group_kind}")
        else:
          raise newException(CatchableError, &"TODO 2 {a.parent.kind}")
    of tkPlus, tkStar, tkQuestion:
      # ... + | * | ?
      case kind
      of tkPlus:
        builder.cursor = newRepetition(OOneOrMore, a)
      of tkStar:
        builder.cursor = newRepetition(OZeroOrMore, a)
      of tkQuestion:
        builder.cursor = newRepetition(OZeroOrOne, a)
      else:
        discard
      a.replace(builder.cursor)
    of tkParenClose:
      # ... )
      builder.cursor = builder.cursor.parent 
      dec(builder.incomplete) # for )
    of tkParenOpen:
      # ... (
      builder.cursor = newEmpty()
      inc(builder.incomplete) # for the new empty
      var group = newConcat()
      group.add(builder.cursor)
      inc(builder.incomplete) # for the (
    of tkPipe:
      # ... |
      builder.cursor = newEmpty()
      inc(builder.incomplete) # for the new empty

      if a.parent == nil:
        var group = newAlternation()
        group.add(a)
        group.add(builder.cursor)
      else:
        discard
    else:
      raise newException(CatchableError, &"TODO 5 {kind}")
  else:
    raise newException(CatchableError, &"TODO 6 {a.kind}")
  # echo "builder.cursor: ", builder.cursor
  # echo "tree"
  # echo printTree(builder.root)

proc root*(ex: OExprBuilder): OExprNode =
  if ex.cursor == nil:
    result = nil
  else:
    result = ex.cursor
    while result.parent != nil:
      result = result.parent

proc nodes*(node: OExprNode, level: int = 0): seq[(int, OExprNode)] =
  result.add((level, node))
  case node.kind
  of OGroup:
    for child in node.children:
      for subchild in child.nodes(level + 1):
        result.add(subchild)
  of ORepetition:
    for subchild in node.child.nodes(level + 1):
      result.add(subchild)
  else:
    discard

proc complete*(builder: OExprBuilder): bool =
  result = builder.incomplete == 0

proc printTree*(node: OExprNode): string =
  for x in node.nodes():
    result.add("...".repeat(x[0]))
    result.add($x[1].kind)
    result.add(" ")
    result.add($x[1])
    result.add("\L")

proc `$`*(ex: OExprBuilder): string =
  result = $(ex.root)
# proc alternate(obj: var OExprNode) : OExprNode =
#   case obj.kind
#   of OAlternation
# proc add(obj: var OExprNode, operator: TokKind) : OExprNode =
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

# proc add(obj: var OExprNode, thing: OExprNode) : OExprNode =
#   result = obj
#   case obj.kind
#   of OAlternation:
#     case thing.kind:
#     of OAlternation
#       raise newException(CatchableError, &"Can't add Alternation to Alternation")
#     obj.children.add(thing)
#   else:
#       raise newException(CatchableError, &"Node does not support multiple children: {obj.repr} while attempting to add {thing.repr}")

proc parseGrammar*(s:string): OGrammar =
  var lexer = initLexer(s)
  result = OGrammar()
  var rule: ORuleDef
  var state: seq[ParserState] = @[stateBetweenGrammars]
  var i = state.len-1
  
  for kind, val in lexer.tokens:
    echo state[i], " ", kind, ": ", val
    if rule.name != "":
      echo "rule: ", rule
    case state[i]
    of stateBetweenGrammars:
      case kind
      of tkIdentifier:
        state[i] = stateHasGrammarName
        result.name = val
      of tkNewline:
        discard
      else:
        throwup(kind, val)
    of stateHasGrammarName:
      case kind
      of tkCurlyOpen:
        state[i] = stateExpectRule
      else:
        throwup(kind, val)
    of stateExpectRule:
      case kind
      of tkNewline:
        discard
      of tkIdentifier:
        state[i] = stateHasRuleName
        rule = newRule(val)
        result.rules.add(rule)
      else:
        throwup(kind, val)
    of stateHasRuleName:
      case kind
      of tkNewline:
        discard
      of tkEqual:
        state[i] = stateExpectExpr
      of tkParenOpen:
        state[i] = stateInDescription
      else:
        throwup(kind, val)
    of stateInDescription:
      case kind
      of tkParenClose:
        state[i] = stateHasRuleName
      else:
        if rule.description.len > 0:
          rule.description.add(" ")
        rule.description.add(val)
    of stateExpectExpr, stateExpectRuleOrExpr:
      rule.expression.add(kind, val)
      if rule.expression.complete():
        state[i] = stateExpectRuleOrExpr
      else:
        state[i] = stateExpectExpr
    of stateEOF:
      if kind == tkEOF:
        discard
      else:
        raise newException(CatchableError, "EOF Error")
    

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
