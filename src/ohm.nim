import
  strutils, lexbase

# ------------------- scanner -------------------------------------------------
type
  TokKind = enum    ## enumeration of all Ohm tokens
    tkInvalid,      ## invalid token
    tkEof,          ## end of file reached
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
  
  Token = object
    kind: TokKind     # the type of the token
    literal: string   # the parsed (string) literal
  
  OhmLexer* = object of BaseLexer
    filename: string

const
  ## synchronized with TokKind above
  tokKindToStr: array[TokKind, string] = [
    "invalid",
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

proc close(L: var OhmLexer) =
  lexbase.close(L)

proc getColumn(L: OhmLexer): int =
  ## get the current column the parse has arrived at
  result = getColNumber(L, L.bufpos)

type
  Grammar* = object

  Match* = object



proc ohm*(grammardef:string): Grammar =
  result = Grammar()

proc match*(grammar: Grammar, input: string): Match =
  result = Match()

proc ok*(match: Match): bool =
  return true
