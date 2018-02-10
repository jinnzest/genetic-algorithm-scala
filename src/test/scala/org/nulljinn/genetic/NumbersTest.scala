package org.nulljinn.genetic

import org.scalatest.wordspec.AnyWordSpec

class NumbersTest extends AnyWordSpec {

  private def filterOutNotBits(str: String): String = {
    str.filter(c => c != ' ' && c != '\n' && c != '=')
  }

  "Numbers" when {
    "toString" should {
      "be the same length as internal numbers size * bits in int" in {
        val numbersArray = Numbers(5, 5 * 5)
        val str = numbersArray.toString
        val normalizedStr = filterOutNotBits(str)
        assert(normalizedStr.length == longBitsAmount * numbersArray.length)
      }
      "contain array filled with 0 bits by default" in {
        val numbersArray = Numbers(5, 5 * 5)
        val str = numbersArray.toString
        val normalizedStr = str.filter(c => c != ' ' && c != '\n' && c != '=')
        assert(normalizedStr.forall(_ == '0'))
      }
    }
  }
}
