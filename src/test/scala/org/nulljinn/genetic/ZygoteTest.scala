package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._
import org.scalacheck.{Gen => SCGen}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class ZygoteTest extends AnyWordSpec {

  private val chromosomeGenesAmount = 100

  private def generateZygote() = new RandomUtilsPerfImpl(chromosomeGenesAmount).generateZygote()

  private def normalizeGenesStr(s: String) = s.filter(_ != ' ').reverse

  "Zygote" when {
    val genGen = org.scalacheck.Gen.oneOf(D1, D0, R1, R0)
    "mutate" should {
      "not modify genes outside defined position" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zygote = generateZygote()
        val mutatedZygote = zygote.mutate(mutPos, g)

        val initialGenesStr = normalizeGenesStr(zygote.toString)
        val mutatedGenesStr = normalizeGenesStr(mutatedZygote.toString)
        assert(initialGenesStr.zip(mutatedGenesStr).zipWithIndex.forall { pair =>
          val ((initial, mutated), pos) = pair
          if (pos == mPos) {
            true
          } else {
            initial == mutated
          }
        })
      }
      "modify gen defined by pos" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zygote = generateZygote()
        val mutatedZygote = zygote.mutate(mutPos, g)

        assert(normalizeGenesStr(mutatedZygote.toString).zipWithIndex.forall { pair =>
          val (gen, pos) = pair
          if (pos == mutPos) {
            gen == g.toChar
          } else {
            true
          }
        })
      }
      "keep size" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zgt = generateZygote()
        val crossedZygote = zgt.mutate(mutPos, g)

        assert(zgt.toString.length == crossedZygote.toString.length)
      }
    }
    "cross" should {
      "keep size" in forAll(SCGen.posNum[Int], SCGen.posNum[Int]) { (bgnPos, endPos) =>
        val bgnPosition = bgnPos % chromosomeGenesAmount
        val endPosition = endPos % chromosomeGenesAmount
        val zgt = generateZygote()
        val zgt2 = generateZygote()
        val crossedZygote = zgt.cross(zgt2, bgnPosition, endPosition)

        assert(zgt.toString.length == crossedZygote.toString.length)
      }
      "cross parts" in {
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        val crossed = zgt1.cross(zgt2, 3, 4)
        assert(crossed.toString == "dddd drrr rddd")
      }
      "cross parts when end pos bigger than size" in {
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        val crossed = zgt1.cross(zgt2, 3, 21)
        assert(crossed.toString == "rrrr rrrr rddd")
      }
    }
  }
}