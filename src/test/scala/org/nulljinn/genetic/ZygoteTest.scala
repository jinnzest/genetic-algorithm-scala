package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._
import org.scalacheck.{Gen => SCGen}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.nulljinn.genetic.Shared.groupByIntAndBytePos

class ZygoteTest extends AnyWordSpec {

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
        val initialGenesStr = normalizeGenesStr(zygote.toString)

        zygote.mutate(mutPos, g)
        val mutatedGenesStr = normalizeGenesStr(zygote.toString)

        assert(initialGenesStr.zip(mutatedGenesStr).reverse.zipWithIndex.forall { pair =>
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
        zygote.mutate(mutPos, g)

        val mutatedGenesStr = normalizeGenesStr(zygote.toString)

        assert(mutatedGenesStr.reverse.zipWithIndex.forall { pair =>
          val (gen, pos) = pair
          val res = if (pos == mutPos) {
            gen == g.toChar
          } else {
            true
          }
          res
        })
      }
      "keep size" in forAll(genGen, SCGen.posNum[Int]) { (g, mPos) =>
        val mutPos = mPos % chromosomeGenesAmount
        val zgt = generateZygote()
        val initialZygote = zgt.toString
        val crossedZygote = zgt.mutate(mutPos, g)

        assert(initialZygote.length == crossedZygote.toString.length)
      }
    }
    "cross" should {
      "keep size" in forAll(SCGen.posNum[Int], SCGen.posNum[Int]) { (bgnPos, endPos) =>
        val bgnPosition = bgnPos % chromosomeGenesAmount
        val endPosition = endPos % chromosomeGenesAmount
        val zgt = generateZygote()
        val zgt2 = generateZygote()
        val crossedZygote = zgt.cross(zgt2, bgnPosition, endPosition, bidirectional = false)

        assert(zgt.toString.length == crossedZygote.toString.length)
      }
      "cross parts" in {
        val pools = AllPools(2, 1, 12)
        val zgt1 = Zygote("dddd dddd dddd", pools.numbers.obtainPos(), pools)
        val zgt2 = Zygote("rrrr rrrr rrrr", pools.numbers.obtainPos(), pools)
        val crossed = zgt1.cross(zgt2, 3, 4, bidirectional = false)
        assert(crossed.toString == "dddd drrr rddd")
      }
      "cross parts when end pos is bigger than size" in {
        val pools = AllPools(2, 1, 12)
        val zgt1 = Zygote("dddd dddd dddd", pools.numbers.obtainPos(), pools)
        val zgt2 = Zygote("rrrr rrrr rrrr", pools.numbers.obtainPos(), pools)
        assert(zgt1.cross(zgt2, 3, 50, bidirectional = true).toString == "rrrr rrrr rddd")
      }
    }
    "compose numbers from str" should {
      val pools = AllPools(1, 4, 1)
      val zygote = Zygote("rrrr", pools.numbers.obtainPos(), pools)
      assert((zygote.dominance.getNumber(0) & 0xF) == 0x0)
      assert((zygote.values.getNumber(0) & 0xF) == 0x0)

      "map dddd to (0xF,0x0)" in {
        val pools = AllPools(1, 2, 2)
        val pos = pools.numbers.obtainPos()
        Zygote("dddd", pos, pools)
        assert((pools.numbers(pools.numbers.dominancePos(pos)) & 0xF) == 0xF)
        assert((pools.numbers(pools.numbers.valuesPos(pos)) & 0xF) == 0x0)
      }
      "map DDDD to (0xF,0xF)" in {
        val pools = AllPools(1, 1, 4)
        Zygote("DDDD", pools.numbers.obtainPos(), pools)
        assert((pools.numbers(pools.numbers.dominancePos(0)) & 0xF) == 0xF)
        assert((pools.numbers(pools.numbers.valuesPos(0)) & 0xF) == 0xF)
      }
      "map RRRR to (0x0,0xF)" in {
        val pools = AllPools(1, 1, 4)
        Zygote("RRRR", pools.numbers.obtainPos(), pools)
        assert((pools.numbers(pools.numbers.dominancePos(0)) & 0xF) == 0)
        assert((pools.numbers(pools.numbers.valuesPos(0)) & 0xF) == 0xF)
      }
      "map DDRRddrr to (0xCC,0xF0)" in {
        val pools = AllPools(1, 2, 4)
        Zygote("DDRR ddrr", pools.numbers.obtainPos(), pools)
        assert((pools.numbers(pools.numbers.dominancePos(0)) & 0xFF) == 0xCC)
        assert((pools.numbers(pools.numbers.valuesPos(0)) & 0xFF) == 0xF0)
      }
      "map DDRRddrr to and back" in {
        val pools = AllPools(1, 2, 4)
        assert(Zygote("DDRR ddrr", pools.numbers.obtainPos(), pools).toString == "DDRR ddrr")
      }
      "pass 70 random gens to and back without modifications" in forAll(SCGen.listOfN(72, SCGen.oneOf('D', 'd', 'R', 'r'))) { (chars: List[Char]) =>
        val pools = AllPools(1, 2, 36)
        val reference = groupByIntAndBytePos(chars)
        val restored = Zygote(reference, pools.numbers.obtainPos(), pools).toString
        assert(restored == reference)
      }
    }
  }

  private def normalizeGenesStr(s: String) = s.filter(_ != ' ')
}