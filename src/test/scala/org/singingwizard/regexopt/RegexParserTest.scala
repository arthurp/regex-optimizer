package org.singingwizard.regexopt

import org.scalatest._
import org.singingwizard.regexopt._
import java.io.IOException

class RegexParserTest extends FlatSpec with Matchers {
  "The regex parser" should "parse simple character sequences correctly" in {
    RegexParser.parse("a") should be (Character('a'))
    RegexParser.parse("ab") should be (Sequence(Vector(Character('a'), Character('b'))))
    RegexParser.parse("a\\*") should be (Sequence(Vector(Character('a'), Character('*'))))
    RegexParser.parse("a\\w") should be (Sequence(Vector(Character('a'), Escape('w'))))
  }

  it should "parse simple combinators" in {
    RegexParser.parse("a*") should be (Star(Character('a')))
    RegexParser.parse("a*?") should be (Star(Character('a'), false))
    RegexParser.parse("a??") should be (Optional(Character('a'), false))
    RegexParser.parse("a?") should be (Optional(Character('a')))
    RegexParser.parse("a|b") should be (Alternative(Character('a'), Character('b')))
    RegexParser.parse("a+a") should be (Sequence(Vector(Plus(Character('a')), Character('a'))))
    RegexParser.parse("ab*") should be (Sequence(Vector(Character('a'), Star(Character('b')))))
  }
  
  it should "parse special matchers" in {
    RegexParser.parse("^a*") should be (Sequence(Vector(StartOfString(), Star(Character('a')))))
    RegexParser.parse("a*$") should be (Sequence(Vector(Star(Character('a')), EndOfString())))
    RegexParser.parse("[a-z]") should be (CharacterSet(Set(CharacterUnparsed("a-z"))))
  }
  
  it should "parse extension groups" in {
    RegexParser.parse("(?abcd)") should be (Unparsed("(?abcd)"))
    RegexParser.parse("a(?abcd)*b") should be (Sequence(Vector(Character('a'), Star(Unparsed("(?abcd)")), Character('b'))))
  }
  /*
  it should "parse nested extension groups" in {
    RegexParser.parse("(?abcd(?abcd))") should be (Unparsed("(?abcd(?abcd))"))
  }
  */
  
  it should "parse simple groups" in {
    RegexParser.parse("(ab)*") should be (Star(Group(Sequence(Vector(Character('a'), Character('b'))), "")))
    RegexParser.parse("(a/)??") should be (Optional(Group(Sequence(Vector(Character('a'), Character('/'))), ""), false))
  }
  
  "The exact match parser" should "convert all strings into characters" in {
    RegexParser.parseExactMatch("a*") should be (Sequence(Vector(Character('a'), Character('*'))))
    RegexParser.parseExactMatch("ab*") should be (Sequence(Vector(Character('a'), Character('b'), Character('*'))))
  }
}