package org.nulljinn.genetic

import org.scalacheck.{Gen => SCGen}
import org.scalatest.wordspec.AnyWordSpec

class UtilsTest extends AnyWordSpec {
  "genetic package methods" when {
    "calc gray code" should {
      "1000 -> 1111" in {
        val res = gray2bin(0x8)
        assert((res & 0xF) == 0xF)
      }
      "1111 -> 1010" in {
        val res = gray2bin(0xF)
        assert((res & 0xF) == 0xA)
      }
    }
    "normalize fitness" should {
      "0 in [0,0] == 1" in {
        assert(normalizeFitness(0, 0, 0) == 1)
      }
      "0 in [0,1] == 0" in {
        assert(normalizeFitness(0, 0, 1) == 0)
      }
      "1 in [1,1] == 1" in {
        assert(normalizeFitness(1, 1, 1) == 1)
      }
      "0.5 in [0,1] == 0.5" in {
        assert(normalizeFitness(0.5, 0, 1) == 0.5)
      }
      "1 in [1,2] == 0" in {
        assert(normalizeFitness(1, 1, 2) == 0)
      }
      "2 in [1,2] == 1" in {
        assert(normalizeFitness(2, 1, 2) == 1)
      }
      "-15 in [-20,-10] == 0.5" in {
        assert(normalizeFitness(-15, -20, -10) == 0.5)
      }
      "0 in [-10,10] == 0.5" in {
        assert(normalizeFitness(0, -10, 10) == 0.5)
      }
      "-1000000 in [-10,10] == 0" in {
        assert(normalizeFitness(-1000000, -10, 10) == 0)
      }
    }
    "decodeBitsToNumbers" should {
      "0000000000000000000000000000000000000000000000000000000000000010 be 3" in {
        assert(decodeBitsToNumbers(NumbersLine(
          "0000000000000000000000000000000000000000000000000000000000000010"
        ).numbers) sameElements (3 :: Nil))
      }
      "0000000000000000000000000000000000000000000000000000000010000000 be 255" in {
        assert(decodeBitsToNumbers(NumbersLine(
          "0000000000000000000000000000000000000000000000000000000010000000"
        ).numbers) sameElements (255 :: Nil))
      }
      "0000000000000000000000000000000000000000000000000000000000000010" +
        "0000000000000000000000000000000000000000000000000000000000000011 be 3, 2" in {
        assert(decodeBitsToNumbers(
          NumbersLine(
            "0000000000000000000000000000000000000000000000000000000000000010" +
              "0000000000000000000000000000000000000000000000000000000000000011"
          ).numbers.reverse
        ) sameElements (3 :: 2 :: Nil))
      }
    }

  }
}
