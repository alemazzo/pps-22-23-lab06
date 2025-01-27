package u06lab.code

import org.junit.Assert.*
import org.junit.Test
import u06lab.code.Parsers.charParser

class ParserTests:
  @Test
  def testBasicParser =
    assertTrue(parser.parseAll("aabc".toList))
    assertFalse(parser.parseAll("aabcdc".toList))
    assertTrue(parser.parseAll("".toList))

  def parser = new BasicParser(Set('a', 'b', 'c'))

  @Test
  def testNotEmptyParser =
    assertTrue(parserNE.parseAll("0101".toList))
    assertFalse(parserNE.parseAll("0123".toList))
    assertFalse(parserNE.parseAll(List()))

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))

  @Test
  def testNotTwoConsecutiveParser =
    assertTrue(parserNTC.parseAll("XYZ".toList))
    assertFalse(parserNTC.parseAll("XYYZ".toList))
    assertTrue(parserNTC.parseAll("".toList))

  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))

  @Test
  def testNotEmptyAndNotTwoConsecutiveParser =
    assertTrue(parserNTCNE.parseAll("XYZ".toList))
    assertFalse(parserNTCNE.parseAll("XYYZ".toList))
    assertFalse(parserNTCNE.parseAll("".toList))

  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]

  @Test
  def testStringParser =
    assertTrue(sparser.parseAll("aabc".toList))
    assertFalse(sparser.parseAll("aabcdc".toList))
    assertTrue(sparser.parseAll("".toList))

  def sparser: Parser[Char] = "abc".charParser()

  @Test
  def testShorterThanNParser =
    assertTrue(parserShorterThanN.parseAll("".toList))
    assertTrue(parserShorterThanN.parseAll("a".toList))
    assertTrue(parserShorterThanN.parseAll("ab".toList))
    assertTrue(parserShorterThanN.parseAll("abc".toList))
    assertFalse(parserShorterThanN.parseAll("aabc".toList))
    assertFalse(parserShorterThanN.parseAll("aabcdc".toList))

  def parserShorterThanN = new BasicParser(Set('a', 'b', 'c')) with ShortenThenN[Char](3)



