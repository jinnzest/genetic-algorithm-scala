package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._
import org.scalacheck.{Gen => SCGen}

class ZygoteTest extends TestsBase {

  private val chromosomeGenesAmount = 100

  private def generateZygote() = new RandomUtilsPerfImpl(chromosomeGenesAmount).generateZygote()

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
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        val crossed = zgt1.cross(zgt2, 3, 4, false)
        crossed.toString mustBe "dddd drrr rddd"
      }
      "cross parts when end pos bigger than size" in {
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        zgt1.cross(zgt2, 3, 50, true).toString mustBe "rrrr rrrr rddd"
      }
    }
    "compose numbers from str" should {
      "map rrrr to (0x0,0x0)" in {
        val zygote = Zygote("rrrr")
        zygote.dominance.getNumber(0) & 0xF mustBe 0x0
        zygote.values.getNumber(0) & 0xF mustBe 0x0
      }
      "map dddd to (0xF,0x0)" in {
        val zygote = Zygote("dddd")
        zygote.dominance.getNumber(0) & 0xF mustBe 0xF
        zygote.values.getNumber(0) & 0xF mustBe 0x0
      }
      "map DDDD to (0xF,0xF)" in {
        val zygote = Zygote("DDDD")
        zygote.dominance.getNumber(0) & 0xF mustBe 0xF
        zygote.values.getNumber(0) & 0xF mustBe 0xF
      }
      "map RRRR to (0x0,0xF)" in {
        val zygote = Zygote("RRRR")
        zygote.dominance.getNumber(0) & 0xF mustBe 0x0
        zygote.values.getNumber(0) & 0xF mustBe 0xF
      }
      "map DDRRddrr to (0xCC,0xF0)" in {
        val zygote = Zygote("DDRR ddrr")
        zygote.dominance.getNumber(0) & 0xFF mustBe 0xCC
        zygote.values.getNumber(0) & 0xFF mustBe 0xF0
      }
      "map DDRRddrr to and back" in {
        Zygote("DDRR ddrr").toString mustBe "DDRR ddrr"
      }
      "pass 70 random genes to and back without modifications" in forAll(SCGen.listOfN(72, SCGen.oneOf('D', 'd', 'R', 'r'))) { (chars: List[Char]) =>
        val reference = groupByIntAndBytePos(chars)
        val restored = Zygote(reference).toString
        restored mustBe reference
      }
    }
  }

  private def normalizeGenesStr(s: String) = s.filter(_ != ' ')
}