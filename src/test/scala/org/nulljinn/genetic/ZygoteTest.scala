package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._
import org.scalacheck.{Gen => SCGen}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.nulljinn.genetic.Shared.groupByIntAndBytePos

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
        val initialGenesStr = normalizeGenesStr(zygote.toString)

        zygote.mutate(mutPos, g)
        val mutatedGenesStr = normalizeGenesStr(zygote.toString)

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
        val initialGenesStr = normalizeGenesStr(zygote.toString)

        zygote.mutate(mutPos, g)
        val mutatedGenesStr = normalizeGenesStr(zygote.toString)

        assert(initialGenesStr.zip(mutatedGenesStr).zipWithIndex.forall { pair =>
          val ((initial, mutated), pos) = pair
          if (pos == mPos) {
            true
          } else {
            initial == mutated
          }
        })
        assert(normalizeGenesStr(zygote.toString).zipWithIndex.forall { pair =>
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
        val crossedZygote = zgt.cross(zgt2, bgnPosition, endPosition, false)

        assert(zgt.toString.length == crossedZygote.toString.length)
      }
      "cross parts" in {
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        val crossed = zgt1.cross(zgt2, 3, 4, false)
        assert(crossed.toString == "dddd drrr rddd")
      }
      "cross parts when end pos bigger than size" in {
        val zgt1 = Zygote("dddd dddd dddd")
        val zgt2 = Zygote("rrrr rrrr rrrr")
        assert(zgt1.cross(zgt2, 3, 50, true).toString == "rrrr rrrr rddd")
      }
    }
    "compose numbers from str" should {
      "map rrrr to (0x0,0x0)" in {
        val zygote = Zygote("rrrr")
        assert((zygote.dominance.getNumber(0) & 0xF) == 0x0)
        assert((zygote.values.getNumber(0) & 0xF) == 0x0)
      }
      "map dddd to (0xF,0x0)" in {
        val zygote = Zygote("dddd")
        assert((zygote.dominance.getNumber(0) & 0xF) == 0xF)
        assert((zygote.values.getNumber(0) & 0xF) == 0x0)
      }
      "map DDDD to (0xF,0xF)" in {
        val zygote = Zygote("DDDD")
        assert((zygote.dominance.getNumber(0) & 0xF) == 0xF)
        assert((zygote.values.getNumber(0) & 0xF) == 0xF)
      }
      "map RRRR to (0x0,0xF)" in {
        val zygote = Zygote("RRRR")
        assert((zygote.dominance.getNumber(0) & 0xF) == 0x0)
        assert((zygote.values.getNumber(0) & 0xF) == 0xF)
      }
      "map DDRRddrr to (0xCC,0xF0)" in {
        val zygote = Zygote("DDRR ddrr")
        assert((zygote.dominance.getNumber(0) & 0xFF) == 0xCC)
        assert((zygote.values.getNumber(0) & 0xFF) == 0xF0)
      }
      "map DDRRddrr to and back" in {
        assert(Zygote("DDRR ddrr").toString == "DDRR ddrr")
      }
      "pass 70 random genes to and back without modifications" in forAll(SCGen.listOfN(72, SCGen.oneOf('D', 'd', 'R', 'r'))) { (chars: List[Char]) =>
        val reference = groupByIntAndBytePos(chars)
        val restored = Zygote(reference).toString
        assert(restored == reference)
      }
    }
  }
}