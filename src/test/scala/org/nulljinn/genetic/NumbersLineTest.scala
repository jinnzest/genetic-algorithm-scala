package org.nulljinn.genetic

import org.scalacheck.{Gen => SCGen}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalatestplus.scalacheck.Checkers
import org.nulljinn.genetic.Shared.{groupByIntAndBytePos, toStr}

class NumbersLineTest extends AnyWordSpec with Checkers {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = new PropertyCheckConfiguration(minSuccessful = 10000)

  "NumbersLine" when {
    "toString" should {
      "write 0xF to numbers line if 1111 is passed to apply" in {
        assert((NumbersLine("1111").getNumber(0) & 0xFF) == 0xF)
      }
      "write 0 to numbers line if 0000 is passed to apply" in {
        assert((NumbersLine("0000").getNumber(0) & 0xFF) == 0)
      }
      val charBitsGen = SCGen.listOfN(25 * longBitsAmount, SCGen.oneOf('0', '1'))
      "fromStr -> toStr should return original str" in forAll(charBitsGen) { chars =>
        val referenceStr = groupByIntAndBytePos(chars)
        val line = NumbersLine(referenceStr)
        val obtainedStr = line.toString
        assert(obtainedStr == referenceStr)
      }
    }
    "crossBits" should {
      val posGen = SCGen.choose(0, 5 * 5 * longBitsAmount)
      val amountGen = SCGen.choose(1, 5 * 5 * longBitsAmount - 1)
      val charBitsGen = SCGen.listOfN(5 * 5 * longBitsAmount, SCGen.oneOf('0', '1'))

      def splitChars(begin: Int, end: Int, str: String): (String, String, String) = {
        val (head, tail) = str.splitAt(begin)
        val (headOfTail, tailOfTail) = tail.splitAt(end)
        (head, headOfTail, tailOfTail)
      }

      def cross(begin: Int, end: Int, from: String, to: String) = {
        val (h1, _, t1) = splitChars(begin, end, to)
        val (_, m2, _) = splitChars(begin, end, from)
        h1 ++ m2 ++ t1
      }

      "replace defined amount of bits of destination line starting from specified position by bits from source line" in forAll(charBitsGen, charBitsGen, posGen, amountGen) { (charsTo: List[Char], charsFrom: List[Char], startBit, bitsAmount) =>

        val from = groupByIntAndBytePos(toStr(charsFrom))
        val to = groupByIntAndBytePos(toStr(charsTo))

        val crossedStrTo = groupByIntAndBytePos(cross(startBit, bitsAmount, from.reverse.replaceAll(" ", ""), to.reverse.replaceAll(" ", ""))).reverse
        val crossedStrFrom = groupByIntAndBytePos(cross(startBit, bitsAmount, to.reverse.replaceAll(" ", ""), from.reverse.replaceAll(" ", ""))).reverse

        val lineFrom = NumbersLine(from)
        val lineTo = NumbersLine(to)
        lineTo.crossBits(lineFrom, startBit, bitsAmount, bidirectional = true)
        val resultStrTo = lineTo.toString
        val resultStrFrom = lineFrom.toString
        assert(resultStrTo == crossedStrTo)
        assert(resultStrFrom == crossedStrFrom)
      }
    }
  }
}
