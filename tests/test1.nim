import unittest
import ohm

test "match":
  let grammar = ohm"""
  Laugh {
    laugh = lol | "rofl"
    lol = "l" "o"+ "l"
  }
  """
  check grammar.match("lol").ok()
  check grammar.match("rofl").ok()
  check grammar.match("loooool").ok()
  check grammar.match("foo").ok() == false

