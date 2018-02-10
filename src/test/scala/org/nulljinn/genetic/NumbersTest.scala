package org.nulljinn.genetic

class NumbersTest extends TestsBase {
  "Numbers" when {
    "toString" should {
      "be the same length as internal numbers size * bits in int" in {
        val numbersArray = Numbers(5, 5 * 5)
        val str = numbersArray.toString
        val normalizedStr = filterOutNotBits(str)
        normalizedStr.length mustBe longBitsAmount * numbersArray.length
      }
      "contain array filled with 0 bits by default" in {
        val numbersArray = Numbers(5, 5* 5)
        val str = numbersArray.toString
        val normalizedStr = str.filter(c => c != ' ' && c != '\n' && c != '=')
        normalizedStr.foreach(_ mustBe '0')
      }
    }
  }

  private def filterOutNotBits(str: String) = {
    str.filter(c => c != ' ' && c != '\n' && c != '=')
  }
}
