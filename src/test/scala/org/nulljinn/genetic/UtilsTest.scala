package org.nulljinn.genetic

import org.scalatest.{MustMatchers, WordSpec}

class UtilsTest extends WordSpec with MustMatchers {
  "genetic package methods" when {
    "calc gray code" should {
      "1000 -> 1111" in {
        val res = gray2bin((true :: false :: false :: false :: Nil).toArray)
        res mustBe true :: true :: true :: true :: Nil
      }
      "1111 -> 1010" in {
        val res = gray2bin((true :: true :: true :: true :: Nil).toArray)
        res mustBe true :: false :: true :: false :: Nil
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
        val listOf52False = (1 to 62).foldLeft(List.empty[Boolean])((acc, _) => false :: acc)
        decodeBitsToNumbers((listOf52False ::: true :: false :: Nil).toArray) mustBe (3 :: Nil).toArray
      }
      "0000000000000000000000000000000000000000000000000000000010000000 be 255" in {
        val listOf52False = (1 to 56).foldLeft(List.empty[Boolean])((acc, _) => false :: acc)
        val listOf7False = (1 to 7).foldLeft(List.empty[Boolean])((acc, _) => false :: acc)
        decodeBitsToNumbers((listOf52False ::: true :: listOf7False).toArray) mustBe (255 :: Nil).toArray
      }
      "0000000000000000000000000000001000000000000000000000000000000011 be 3, 2" in {
        val listOf52False = (1 to 62).foldLeft(List.empty[Boolean])((acc, _) => false :: acc)
        decodeBitsToNumbers(
          (listOf52False ::: true :: false :: listOf52False ::: true :: true :: Nil).toArray) mustBe (3 :: 2 :: Nil).toArray
      }
    }
    "toNumber" should {
      "11111111 -> 255" in {
        toNumber((true :: true :: true :: true :: true :: true :: true :: true :: Nil).toArray) mustBe 255
      }
      "00000000 -> 0" in {
        toNumber((false :: false :: false :: false :: false :: false :: false :: false :: Nil).toArray) mustBe 0
      }
    }
  }
}
