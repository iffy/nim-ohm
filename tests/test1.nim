import sugar
import unittest
import strformat
import ohm

suite "treebuilder":
  var b : OExprBuilder
  setup:
    b = newExprBuilder()
  
  test "empty to OTerminal":
    b.add(tkString, "foo")
    echo "b.cursor: ", b.cursor
    check b.root.kind == OTerminal
    check b.root.val == "foo"
    check $b.root == "\"foo\""
    check b.complete()
  
  test "empty to ORule":
    b.add(tkIdentifier, "hello")
    check b.root.kind == ORule
    check b.root.rule == "hello"
    check $b.root == "hello"
    check b.complete()
  
  test "empty to (":
    b.add(tkParenOpen)
    check b.complete() == false
  
  test "empty to |":
    b.add(tkPipe)
    check b.complete() == false
  
  test "empty to newline":
    b.add(tkNewline)
    check b.complete() == false
  
  test "term term":
    b.add(tkString, "foo")
    b.add(tkString, "bar")
    check b.root.kind == OConcat
    check $b.root == "\"foo\" \"bar\""
    check b.complete()
  
  test "term rule":
    b.add(tkString, "foo")
    b.add(tkIdentifier, "hey")
    check b.root.kind == OConcat
    check $b.root == "\"foo\" hey"
    check b.complete()

  test "term term +":
    b.add(tkString, "foo")
    b.add(tkString, "bar")
    b.add(tkPlus)
    check b.root.kind == OConcat
    check $b.root == "\"foo\" \"bar\"+"
    check b.root.children.len == 2
    check b.root.children[1].kind == ORepetition
    check b.root.children[1].repetition_kind == OOneOrMore
    check b.complete()
  
  test "rule *":
    b.add(tkIdentifier, "rule")
    b.add(tkStar)
    check b.root.kind == ORepetition
    check b.root.repetition_kind == OZeroOrMore
    check b.root.children[0].kind == ORule
    check b.root.children[0].rule == "rule"
    check $b.root == "rule*"
  
  test "term ?":
    b.add(tkString, "foo")
    b.add(tkQuestion)
    check b.root.kind == ORepetition
    check b.root.repetition_kind == OZeroOrOne
    check b.root.children[0].kind == OTerminal
    check b.root.children[0].val == "foo"
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
    check b.complete()
    check b.root.kind == ORepetition
    check b.root.repetition_kind == OOneOrMore
    check $b.root == "(\"a\" \"b\")+"
    check b.root.children[0].kind == OConcat
  
  test "rule rule | rule":
    b.add(tkIdentifier, "a")
    b.add(tkIdentifier, "b")
    b.add(tkPipe)
    b.add(tkIdentifier, "c")
    check b.complete
    check b.root.kind == OAlternation
    check b.root.children[0].kind == OConcat
    check b.root.children[0].children.len == 2
    check b.root.children[1].kind == ORule
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
    check b.complete == true

    check b.root.kind == OConcat
    check b.root.children.len == 2
    check b.root.children[0].val == "a"
    check b.root.children[1].kind == OAlternation
    check b.root.children[1].children.len == 2
    check b.root.children[1].children[0].rule == "b"
    check b.root.children[1].children[1].val == "c"
    check $b.root == "\"a\" (b | \"c\")"
  
  test "term | term":
    b.add(tkString, "a")
    check b.complete() == true
    b.add(tkPipe)
    check b.complete() == false
    b.add(tkString, "b")
    check b.complete()
    check b.root.kind == OAlternation
    check $b.root == "\"a\" | \"b\""
    check b.root.children.len == 2
    check b.root.children[0].kind == OTerminal
    check b.root.children[1].kind == OTerminal

  test "| rule":
    b.add(tkPipe)
    check b.complete() == false
    b.add(tkIdentifier, "hello")
    check b.complete()
    check b.root.kind == ORule
    check b.root.rule == "hello"
    check $b.root == "hello"
  
  test "rule -- foo | rule -- bar":
    b.add(tkIdentifier, "a")
    b.add(tkDoubleHyphen)
    b.add(tkIdentifier, "foo")
    b.add(tkPipe)
    b.add(tkIdentifier, "b")
    b.add(tkDoubleHyphen)
    b.add(tkIdentifier, "bar")



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
