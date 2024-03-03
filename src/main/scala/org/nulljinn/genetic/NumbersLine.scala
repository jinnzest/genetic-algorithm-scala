package org.nulljinn.genetic

case class NumbersLine(numbers: Array[Long], size: Int) {
  def cloneLine(): NumbersLine = NumbersLine(numbers.clone(), size)

  def crossBits(from: NumbersLine, fromBit: Int, bitsAmount: Int, bidirectional: Boolean): Unit = {
    val nBitsAmount = if (size - fromBit < bitsAmount) size - fromBit else bitsAmount

    val shiftFrom = fromBit % longBitsAmount

    val bitsLeft = longBitsAmount - shiftFrom

    val firstBitsAmount = if (bitsLeft >= nBitsAmount) nBitsAmount else bitsLeft

    val afterLastBit = fromBit + nBitsAmount

    val pByteFrom = fromBit / longBitsAmount

    val shiftTo = afterLastBit % longBitsAmount

    val fullBytesFrom = if (shiftFrom > 0) pByteFrom + 1 else pByteFrom

    val pByteTo = (afterLastBit - 1) / longBitsAmount

    val fullBytesTo = if (shiftTo != 0) pByteTo - 1 else pByteTo

    val fullBytesPresent = (nBitsAmount + shiftFrom) / longBitsAmount > 0 || shiftFrom == 0 && nBitsAmount / longBitsAmount > 0

    val fromPresent = if (fullBytesPresent && shiftFrom == 0) false else shiftTo > 0 || shiftFrom > 0

    val toPresent = afterLastBit / longBitsAmount > 0 && shiftTo > 0 && pByteFrom != pByteTo

    if (fromPresent) crossFirstNumber(shiftFrom, pByteFrom, from.numbers, firstBitsAmount, bidirectional)

    if (fullBytesPresent) crossMiddleNumbers(from.numbers, fullBytesFrom, fullBytesTo, bidirectional)

    if (toPresent) crossLastNumber(shiftTo, pByteTo, from.numbers, bidirectional)
  }

  private def crossMiddleNumbers(numbersFrom: Array[Long], from: Int, to: Int, bidirectional: Boolean) =
    from to to foreach { pos =>
      val tmp = numbersFrom(pos)
      if (bidirectional) numbersFrom(pos) = numbers(pos)
      numbers(pos) = tmp
    }

  private def crossFirstNumber(fromBit: Int, pos: Int, numbersFrom: Array[Long], bitsAmount: Int, bidirectional: Boolean) = {
    val mask = (-1L >>> (longBitsAmount - bitsAmount)) << fromBit
    val numFrom = numbersFrom(pos)
    val numTo = numbers(pos)
    val maskedTo = numTo & mask
    val maskedFrom = numFrom & mask
    numbers(pos) = numTo & ~mask | maskedFrom
    if (bidirectional) numbersFrom(pos) = numFrom & ~mask | maskedTo
  }

  private def crossLastNumber(bitsAmount: Int, pos: Int, numbersFrom: Array[Long], bidirectional: Boolean) = {
    val mask = -1L >>> (longBitsAmount - bitsAmount)
    val numFrom = numbersFrom(pos)
    val numTo = numbers(pos)
    val lastMaskedFrom = numFrom & mask
    val lastMaskedTo = numTo & mask
    numbers(pos) = numTo & ~mask | lastMaskedFrom
    if (bidirectional) numbersFrom(pos) = numFrom & ~mask | lastMaskedTo
  }

  override def toString: String = {
    var p = numbers.length - 1
    var str = ""
    while (p >= 0) {
      str += toBinary(numbers(p)) + " "
      p -= 1
    }
    str.trim
  }

  def update(pos: Int, v: Boolean): Unit = if (v) setBit(pos) else clearBit(pos)

  def apply(pos: Int): Boolean = (numbers(pos / longBitsAmount) & numberMask(pos)) != 0

  def getNumber(pos: Int): Long = numbers(pos / longBitsAmount)

  private def setBit(pos: Int) = numbers(pos / longBitsAmount) |= numberMask(pos)

  private def clearBit(pos: Int) = numbers(pos / longBitsAmount) &= ~numberMask(pos)
}

object NumbersLine {
  def apply(str: String): NumbersLine = {
    val normalizedStr = str.filter(_ != ' ')
    val length = normalizedStr.length
    val len = length / longBitsAmount
    val nLen = len + (if (length % longBitsAmount > 0) 1 else 0)
    val line = NumbersLine(Array.fill(nLen)(0), length)
    val positions = 0 until length
    normalizedStr.zip(positions.reverse).foreach {
      case ('1', p) => line(p) = true
      case ('0', p) => line(p) = false
      case (c, p) => throw new IllegalArgumentException(s"Unexpected character: $c at position $p")
    }
    line
  }
}