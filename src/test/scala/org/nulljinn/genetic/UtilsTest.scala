package org.nulljinn.genetic

import org.scalacheck.{Gen => SCGen}

class UtilsTest extends TestsBase {
  "genetic package methods" when {
    "calc gray code" should {
      "1000 -> 1111" in {
        val res = gray2bin(0x8)
        res & 0xF mustBe 0xF
      }
      "1111 -> 1010" in {
        val res = gray2bin(0xF)
        res & 0xF mustBe 0xA
      }
    }
    "normalize fitness" should {
      "0 in [0,0] == 1" in {
        normalizeFitness(0, 0, 0) mustBe 1
      }
      "0 in [0,1] == 0" in {
        normalizeFitness(0, 0, 1) mustBe 0
      }
      "1 in [1,1] == 1" in {
        normalizeFitness(1, 1, 1) mustBe 1
      }
      "0.5 in [0,1] == 0.5" in {
        normalizeFitness(0.5, 0, 1) mustBe 0.5
      }
      "1 in [1,2] == 0" in {
        normalizeFitness(1, 1, 2) mustBe 0
      }
      "2 in [1,2] == 1" in {
        normalizeFitness(2, 1, 2) mustBe 1
      }
      "-15 in [-20,-10] == 0.5" in {
        normalizeFitness(-15, -20, -10) mustBe 0.5
      }
      "0 in [-10,10] == 0.5" in {
        normalizeFitness(0, -10, 10) mustBe 0.5
      }
      "-1000000 in [-10,10] == 0" in {
        normalizeFitness(-1000000, -10, 10) mustBe 0
      }
    }
    "decodeBitsToNumbers" should {
      "0000000000000000000000000000000000000000000000000000000000000010 be 3" in {
        decodeBitsToNumbers(NumbersLine(
          "0000000000000000000000000000000000000000000000000000000000000010"
        ).numbers) mustBe (3 :: Nil).toArray
      }
      "0000000000000000000000000000000000000000000000000000000010000000 be 255" in {
        decodeBitsToNumbers(NumbersLine(
          "0000000000000000000000000000000000000000000000000000000010000000"
        ).numbers) mustBe (255 :: Nil).toArray
      }
      "0000000000000000000000000000000000000000000000000000000000000010" +
        "0000000000000000000000000000000000000000000000000000000000000011 be 3, 2" in {
        decodeBitsToNumbers(
          NumbersLine(
            "0000000000000000000000000000000000000000000000000000000000000010" +
              "0000000000000000000000000000000000000000000000000000000000000011"
          ).numbers.reverse
        ) mustBe (3 :: 2 :: Nil).toArray
      }
    }

  }
}
