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
        val mutatedZygote = zygote.mutate(mutPos, g)

        val initialGenesStr = normalizeGenesStr(zygote.toString)
        val mutatedGenesStr = normalizeGenesStr(mutatedZygote.toString)
        initialGenesStr.zip(mutatedGenesStr).foldLeft(0) { (acc, pair) =>
          val (f, t) = pair
          if (acc != mutPos) f mustBe t
          acc + 1
        }
      }
      "modify gen defined by pos" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zygote = generateZygote()
        val mutatedZygote = zygote.mutate(mutPos, g)

        normalizeGenesStr(mutatedZygote.toString).foldLeft(0) { (acc, t) =>
          if (acc == mutPos) t mustBe g.toChar
          acc + 1
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
        val crossedZygote = zgt.cross(zgt2, bgnPosition, endPosition)

        zgt.toString.length mustBe crossedZygote.toString.length
      }
      "cross parts" in {
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        val crossed = zgt1.cross(zgt2, 3, 4)
        crossed.toString mustBe "dddd drrr rddd"
      }
      "cross parts when end pos bigger than size" in {
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        val crossed = zgt1.cross(zgt2, 3, 21)
        crossed.toString mustBe "rrrr rrrr rddd"
      }
    }
  }

  private def normalizeGenesStr(s: String) = s.filter(_ != ' ').reverse
}