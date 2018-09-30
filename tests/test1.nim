import sugar
import unittest
import strformat
import ohm

suite "treebuilder":
  var b : OExprBuilder
  setup:
    b = newExprBuilder()
  
  test "OTerm":
    b.add(tkString, "foo")
    check b.root.kind == OTerm
    check b.root.term == "foo"
    check $b.root == "\"foo\""
    check b.complete()
  
  test "OIdentifier":
    b.add(tkIdentifier, "hello")
    check b.root.kind == OIdentifier
    check b.root.identifier == "hello"
    check $b.root == "hello"
    check b.complete()
  
  test "(":
    b.add(tkParenOpen)
    check b.complete() == false
  
  test "|":
    b.add(tkPipe)
    check b.complete() == false
  
  # test "newline":
  #   b.add(tkNewline)
  #   check b.complete() == false
  
  test "term term":
    b.add(tkString, "foo")
    b.add(tkString, "bar")
    b.finish()
    check b.root.kind == OConcat
    check $b.root == "\"foo\" \"bar\""
    check b.complete()
  
  test "term rule":
    b.add(tkString, "foo")
    b.add(tkIdentifier, "hey")
    b.finish()
    check b.root.kind == OConcat
    check $b.root == "\"foo\" hey"
    check b.complete()

  test "term term +":
    b.add(tkString, "foo")
    b.add(tkString, "bar")
    b.add(tkPlus)
    b.finish()
    check b.root.kind == OConcat
    check $b.root == "\"foo\" \"bar\"+"
    check b.root.children.len == 2
    check b.root.children[1].kind == ORepetition
    check b.root.children[1].repetition_kind == OOneOrMore
    check b.complete()
  
  test "rule *":
    b.add(tkIdentifier, "rule")
    b.add(tkStar)
    b.finish()
    check b.root.kind == ORepetition
    check b.root.repetition_kind == OZeroOrMore
    check b.root.children[0].kind == OIdentifier
    check b.root.children[0].identifier == "rule"
    check $b.root == "rule*"
  
  test "term ?":
    b.add(tkString, "foo")
    b.add(tkQuestion)
    b.finish()
    check b.root.kind == ORepetition
    check b.root.repetition_kind == OZeroOrOne
    check b.root.children[0].kind == OTerm
    check b.root.children[0].term == "foo"
    check $b.root == "\"foo\"?"
  
  test "(term term)+":
    b.add(tkParenOpen, "")
    check b.complete() == false
    b.add(tkString, "a")
    check b.complete() == false
    b.add(tkString, "b")
    check b.complete() == false
    b.add(tkParenClose)
    check b.complete()
    b.add(tkPlus)
    b.finish()
    check b.complete()
    check b.root.kind == ORepetition
    check b.root.repetition_kind == OOneOrMore
    check $b.root == "(\"a\" \"b\")+"
    check b.root.children[0].kind == OConcat
  
  test "rule rule rule rule":
    b.add(tkIdentifier, "a")
    b.add(tkIdentifier, "a")
    b.add(tkIdentifier, "a")
    b.add(tkIdentifier, "a")
    b.finish()
    check b.complete()
    check b.root.kind == OConcat
    check b.root.children.len == 4

  test "rule rule | rule":
    b.add(tkIdentifier, "a")
    b.add(tkIdentifier, "b")
    b.add(tkPipe)
    b.add(tkIdentifier, "c")
    b.finish()
    check b.complete
    check b.root.kind == OAlternation
    check b.root.children[0].kind == OConcat
    check b.root.children[0].children.len == 2
    check b.root.children[1].kind == OIdentifier
    check $b.root == "a b | c"

  test "term (rule | term)":
    b.add(tkString, "a")
    b.add(tkParenOpen)
    check b.complete() == false
    b.add(tkIdentifier, "b")
    check b.complete() == false
    b.add(tkPipe)
    check b.complete() == false
    b.add(tkString, "c")
    check b.complete == false
    b.add(tkParenClose)
    b.finish()
    check b.complete == true

    check b.root.kind == OConcat
    check b.root.children.len == 2
    check b.root.children[0].term == "a"
    check b.root.children[1].kind == OAlternation
    check b.root.children[1].children.len == 2
    check b.root.children[1].children[0].identifier == "b"
    check b.root.children[1].children[1].term == "c"
    check $b.root == "\"a\" (b | \"c\")"
  
  test "term | term":
    b.add(tkString, "a")
    check b.complete() == true
    b.add(tkPipe)
    check b.complete() == false
    b.add(tkString, "b")
    b.finish()

    check b.complete()
    check b.root.kind == OAlternation
    check $b.root == "\"a\" | \"b\""
    check b.root.children.len == 2
    check b.root.children[0].kind == OTerm
    check b.root.children[1].kind == OTerm

  test "| rule":
    b.add(tkPipe)
    check b.complete() == false
    b.add(tkIdentifier, "hello")
    b.finish()

    check b.complete()
    check b.root.kind == OIdentifier
    check b.root.identifier == "hello"
    check $b.root == "hello"
  
  test "rule -- foo | rule -- bar":
    b.add(tkIdentifier, "a")
    b.add(tkDoubleHyphen)
    b.add(tkIdentifier, "foo")
    b.add(tkPipe)
    b.add(tkIdentifier, "b")
    b.add(tkDoubleHyphen)
    b.add(tkIdentifier, "bar")
    b.finish()

    check b.complete()
    check $b.root == "a -- foo | b -- bar"



# test "match":
#   echo "test starting"
#   const grammar_string = """
# // Comment
# /* multi
# line comment
# */
# Laugh {
#   laugh = lol | "rofl"
#   // another comment
#   lol (a description)
#     =
#     | "l" "o"+ "l"
#     | "ha"
#     | ("foo" "bar")+
#     | ("a" | "b")+
# }
# """
#   # var lexer = initLexer(grammar_string)
#   # for kind, val in lexer.tokens():
#   #   echo kind, ": ", val.repr
  
#   let grammar = parseGrammar(grammar_string)

#   echo &"grammar: {grammar.repr}"
  
#   # makeLexer("laugh", grammar_string)
#   # echo laugh_TokKind.First
#   # echo "reserved: ", laugh_reservedKeywords
#   # echo "pre  laughParse"
#   # laughParse()
#   # echo "post laughParse"

#   # echo "A.first = ", A.First
#   # echo "A.second = ", A.Second
#   # echo "A.third = ", A.Third

#   # echo "something", something
#   # check grammar.match("lol").ok()
#   # check grammar.match("rofl").ok()
#   # check grammar.match("loooool").ok()
#   # check grammar.match("foo").ok() == false

test "eventual":
  const grammar = """
// JSON
// Based on https://github.com/antlr/grammars-v4/blob/master/json/JSON.g4
JSON {
  json = value

  value =
    | string
    | number
    | object
    | array
    | "true"
    | "false"
    | "null"
  
  ws =
    [ \t\n\r]+
  
  // Strings
  string = "\"" (esc | safecodepoint)* "\""
  esc = "\\" ( ["\\/bfnrt] | unicode )
  unicode = "u" hex hex hex hex
  hex = "0".."9" | "a".."f" | "A".."F"
  safecodepoint = ~ ("\"" | "\u0000".."\u001f" )

  // Numbers
  number = "-"? int ("." "0".."9" +)? exponent?
  int =
    | "0"
    | "1..9" "0".."9"*
  exponent =
    [eE] [+-]? int
  
  // Objects
  object ws:skip =
    | "{" pair ("," pair)* "}"
    | "{" "}"
  pair = string ":" value

  // Arrays
  array ws:skip =
    | "[" value ("," value)* "]"
    | "[" "]"
}
"""
