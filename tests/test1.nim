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
  
  test "empty to ORuleApplication":
    b.add(tkIdentifier, "hello")
    check b.root.kind == ORuleApplication
    check b.root.rule == "hello"
    check $b.root == "hello"
    check b.complete()
  
  test "empty to (":
    b.add(tkParenOpen, "")
    check b.complete() == false
  
  test "empty to |":
    b.add(tkPipe, "")
    check b.complete() == false
  
  test "empty to \L":
    b.add(tkNewline, "\L")
    check b.complete() == false
  
  test "terminal terminal":
    b.add(tkString, "foo")
    b.add(tkString, "bar")
    check b.root.kind == OGroup
    check b.root.group_kind == OConcat
    check $b.root == "\"foo\" \"bar\""
    check b.complete()
  
  test "terminal terminal +":
    b.add(tkString, "foo")
    b.add(tkString, "bar")
    b.add(tkPlus, "")
    check b.root.kind == OGroup
    check b.root.group_kind == OConcat
    check $b.root == "\"foo\" \"bar\"+"
    check b.root.children.len == 2
    check b.root.children[1].kind == ORepetition
    check b.root.children[1].repetition_kind == OOneOrMore
    check b.complete()
  
  test "(terminal terminal)+":
    b.add(tkParenOpen, "")
    check b.complete() == false
    b.add(tkString, "a")
    check b.complete() == false
    b.add(tkString, "b")
    check b.complete() == false
    b.add(tkParenClose, "")
    check b.complete()
    b.add(tkPlus, "")
    check b.complete()
    check b.root.kind == ORepetition
    check b.root.repetition_kind == OOneOrMore
    check $b.root == "(\"a\" \"b\")+"
    echo "hey"
    check b.root.child.kind == OGroup
    echo "ho"
    check b.root.child.group_kind == OConcat
    echo "end"


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
