package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._
import org.scalacheck.{Gen => SCGen}

class ZygoteTest extends TestsBase {

  private val chromosomeGenesAmount = 100

  private def generateZygote() = {
    val pools = AllPools(1, 4, chromosomeGenesAmount)
    new RandomUtilsImpl(chromosomeGenesAmount, pools).generateZygote(pools.numbers.obtainPos())
  }

  "Zygote" when {
    val genGen = org.scalacheck.Gen.oneOf(D1, D0, R1, R0)
    "mutate" should {
      "not modify genes outside defined position" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zygote = generateZygote()
        zygote.mutate(mutPos, g)

        val initialGenesStr = normalizeGenesStr(zygote.toString)
        val mutatedGenesStr = normalizeGenesStr(zygote.toString)
        mutatedGenesStr.zip(initialGenesStr).foldLeft(initialGenesStr.length - 1) { (acc, pair) =>
          val (f, t) = pair
          if (acc != mutPos) f mustBe t
          acc - 1
        }
      }
      "modify gen defined by pos" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zygote = generateZygote()
        zygote.mutate(mutPos, g)

        val mutatedGenesStr = normalizeGenesStr(zygote.toString)
        mutatedGenesStr.foldLeft(mutatedGenesStr.length - 1) { (acc, t) =>
          if (acc == mutPos) t mustBe g.toChar
          acc - 1
        }
      }
      "keep size" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zgt = generateZygote()
        val crossedZygote = zgt.mutate(mutPos, g)

        zgt.toString.length mustBe crossedZygote.toString.length
      }
    }
    "cross" should {
      "keep size" in forAll(SCGen.posNum[Int], SCGen.posNum[Int]) { (bgnPos, endPos) =>
        val bgnPosition = bgnPos % chromosomeGenesAmount
        val endPosition = endPos % chromosomeGenesAmount
        val zgt = generateZygote()
        val zgt2 = generateZygote()
        val crossedZygote = zgt.cross(zgt2, bgnPosition, endPosition, false)

        zgt.toString.length mustBe crossedZygote.toString.length
      }
      "cross parts" in {
        val pools = AllPools(2, 1, 12)
        val zgt1 = Zygote("dddd dddd dddd", pools.numbers.obtainPos(), pools)
        val zgt2 = Zygote("rrrr rrrr rrrr", pools.numbers.obtainPos(), pools)
        val crossed = zgt1.cross(zgt2, 3, 4, false)
        crossed.toString mustBe "dddd drrr rddd"
      }
      "cross parts when end pos is bigger than size" in {
        val pools = AllPools(2, 1, 12)
        val zgt1 = Zygote("dddd dddd dddd", pools.numbers.obtainPos(), pools)
        val zgt2 = Zygote("rrrr rrrr rrrr", pools.numbers.obtainPos(), pools)
        zgt1.cross(zgt2, 3, 50, true).toString mustBe "rrrr rrrr rddd"
      }
    }
    "compose numbers from str" should {
      val pools = AllPools(1, 4, 1)
      Zygote("rrrr", pools.numbers.obtainPos(), pools)
      pools.numbers(pools.numbers.dominancePos(0)) & 0xF mustBe 0
      pools.numbers(pools.numbers.valuesPos(0)) & 0xF mustBe 0

      "map dddd to (0xF,0x0)" in {
        val pools = AllPools(1, 2, 2)
        val pos = pools.numbers.obtainPos()
        Zygote("dddd", pos, pools)
        pools.numbers(pools.numbers.dominancePos(pos)) & 0xF mustBe 0xF
        pools.numbers(pools.numbers.valuesPos(pos)) & 0xF mustBe 0
      }
      "map DDDD to (0xF,0xF)" in {
        val pools = AllPools(1, 1, 4)
        Zygote("DDDD", pools.numbers.obtainPos(), pools)
        pools.numbers(pools.numbers.dominancePos(0)) & 0xF mustBe 0xF
        pools.numbers(pools.numbers.valuesPos(0)) & 0xF mustBe 0xF
      }
      "map RRRR to (0x0,0xF)" in {
        val pools = AllPools(1, 1, 4)
        Zygote("RRRR", pools.numbers.obtainPos(), pools)
        pools.numbers(pools.numbers.dominancePos(0)) & 0xF mustBe 0
        pools.numbers(pools.numbers.valuesPos(0)) & 0xF mustBe 0xF
      }
      "map DDRRddrr to (0xCC,0xF0)" in {
        val pools = AllPools(1, 2, 4)
        Zygote("DDRR ddrr", pools.numbers.obtainPos(), pools)
        pools.numbers(pools.numbers.dominancePos(0)) & 0xFF mustBe 0xCC
        pools.numbers(pools.numbers.valuesPos(0)) & 0xFF mustBe 0xF0
      }
      "map DDRRddrr to and back" in {
        val pools = AllPools(1, 2, 4)
        Zygote("DDRR ddrr", pools.numbers.obtainPos(), pools).toString mustBe "DDRR ddrr"
      }
      "pass 70 random gens to and back without modifications" in forAll(SCGen.listOfN(72, SCGen.oneOf('D', 'd', 'R', 'r'))) { (chars: List[Char]) =>
        val pools = AllPools(1, 2, 36)
        val reference = groupByIntAndBytePos(chars)
        val restored = Zygote(reference, pools.numbers.obtainPos(), pools).toString
        restored mustBe reference
      }
    }
  }

  private def normalizeGenesStr(s: String) = s.filter(_ != ' ')
}