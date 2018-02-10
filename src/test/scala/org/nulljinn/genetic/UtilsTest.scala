package org.nulljinn.genetic

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
        val pools = AllPools(1, 1, 4)
        val numbersLine = pools.numberLines.newObj()
        numbersLine(1) = true
        decodeBitsToNumbers(0, pools.numbers)
        pools.numbers(0) mustBe 3
      }
      "0000000000000000000000000000000000000000000000000000000010000000 be 255" in {
        val pools = AllPools(1, 1, 4)
        val numbersLine = pools.numberLines.newObj()
        numbersLine(7) = true
        decodeBitsToNumbers(0, pools.numbers)
        pools.numbers(0) mustBe 255
      }
      "0000000000000000000000000000000000000000000000000000000000000010" +
        "0000000000000000000000000000000000000000000000000000000000000011 be 3, 2" in {
        val pools = AllPools(1, 2, 64)
        val numbersLine = pools.numberLines.newObj()
        numbersLine(0) = true
        numbersLine(1) = true
        numbersLine(65) = true
        decodeBitsToNumbers(0, pools.numbers)
        pools.numbers(0) mustBe 2
        pools.numbers(1) mustBe 3
      }
    }

  }
}
