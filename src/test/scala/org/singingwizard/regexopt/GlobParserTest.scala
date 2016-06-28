package org.singingwizard.regexopt

import org.scalatest._
import org.singingwizard.regexopt._
import java.io.IOException
import scala.io.Source

class GlobParserTest extends FlatSpec with Matchers { 
  val star = Star(CharacterSet(Set(CharacterSingle('/')), true))
  val starstar = Star(AnyCharacter())
  
  "The glob parser" should "parse simple character sequences correctly" in {
    GlobParser.parse("a") should be (Character('a'))
    GlobParser.parse("ab") should be (Sequence(Vector(Character('a'), Character('b'))))
    GlobParser.parse("a.b") should be (Sequence(Vector(Character('a'), Character('.'), Character('b'))))
    GlobParser.parse("a\\*") should be (Sequence(Vector(Character('a'), Character('*'))))
    GlobParser.parse("a\\w") should be (Sequence(Vector(Character('a'), Escape('w'))))
  }

  it should "parse simple combinators" in {
    GlobParser.parse("a**") should be (Sequence(Character('a'), starstar))
    GlobParser.parse("a*") should be (Sequence(Character('a'), star))
    GlobParser.parse("a?") should be (Sequence(Character('a'), AnyCharacter()))
    GlobParser.parse("a??") should be (Sequence(Vector(Character('a'), AnyCharacter(), AnyCharacter())))
    GlobParser.parse("a+a") should be (Sequence(Vector(Character('a'), Character('+'), Character('a'))))
    GlobParser.parse("ab*") should be (Sequence(Vector(Character('a'), Character('b'), star)))
  }
  
  it should "parse special matchers" in {
    GlobParser.parse("[a-z]") should be (CharacterSet(Set(CharacterUnparsed("a-z"))))
  }
  
  it should "parse all lines of test file" in {
    for (line <- Source.fromInputStream(classOf[GlobParserTest].getResourceAsStream("test-input-ext.glob")).getLines()) {
      if (line != "")
        GlobParser.parse(line)
    }
  }
}