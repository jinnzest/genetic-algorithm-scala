package org.nulljinn.genetic

import org.scalacheck.{Gen => SCGen}

class NumbersLineTest extends TestsBase {

  override implicit val generatorDrivenConfig = new PropertyCheckConfiguration(minSuccessful = 10000)

  "NumbersLine" when {
    "toString" should {
      "write 0xF to numbers line if 1111 is passed to apply" in {
        val pools = AllPools(1, 1, 4)
        NumbersLine("1111", pools.numbers.obtainPos(), pools)
        pools.numbers(0) & 0xFF mustBe 0xF
      }
      "write 0 to numbers line if 0000 is passed to apply" in {
        val pools = AllPools(1,1,4)
        NumbersLine("0000", pools.numbers.obtainPos(), pools)
        pools.numbers(0) & 0xFF mustBe 0
      }
      val charBitsGen = SCGen.listOfN(25 * longBitsAmount, SCGen.oneOf('0', '1'))
      "fromStr -> toStr should return original str" in forAll(charBitsGen) { chars =>
        val referenceStr = groupByIntAndBytePos(chars)
        val pools = AllPools(1,5,longBitsAmount*5)
        val line = NumbersLine(referenceStr, pools.numbers.obtainPos(), pools)
        val obtainedStr = line.toString
        if(obtainedStr!=referenceStr){
          val m =1
        }
        obtainedStr mustBe referenceStr
      }
    }
    "crossBits" should {
      val posGen = SCGen.choose(0, 5 * 5 * longBitsAmount)
      val amountGen = SCGen.choose(1, 5 * 5 * longBitsAmount - 1)
      val charBitsGen = SCGen.listOfN(5 * 5 * longBitsAmount, SCGen.oneOf('0', '1'))
      val zeroBitsGen = SCGen.listOfN(5 * 5 * longBitsAmount, '0')
      val oneBitsGen = SCGen.listOfN(5 * 5 * longBitsAmount, '1')

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

        val allPools = AllPools(5, 5, longBitsAmount * 5)
        allPools.numbers.obtainPos()
        val pos = allPools.numbers.obtainPos()
        val lineFrom = NumbersLine(from, pos, allPools)
        val lineTo = NumbersLine(to, pos + allPools.numbers.numberLinesAmount, allPools)
        lineTo.crossBits(lineFrom, startBit, bitsAmount, true)
        val resultStrTo = lineTo.toString
        val resultStrFrom = lineFrom.toString
        resultStrTo mustBe crossedStrTo
        resultStrFrom mustBe crossedStrFrom
      }
    }
  }
}
