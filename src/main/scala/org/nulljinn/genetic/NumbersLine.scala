package org.nulljinn.genetic

class NumbersLine(var posLine: Int, var allPools: AllPools) {

  def copyBits(from: NumbersLine): NumbersLine = {
    allPools.numbers.copy(from.posLine, posLine, allPools.numbers.numberLinesAmount)
    this
  }

  def crossBits(from: NumbersLine, fromBit: Int, bitsAmount: Int, bidirectional: Boolean): Unit = {
    val lineBitsAmount = from.allPools.numbers.lineBitsAmount - fromBit
    val nBitsAmount = if (lineBitsAmount < bitsAmount) lineBitsAmount else bitsAmount

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

    if (fromPresent) crossFirstNumber(shiftFrom, pByteFrom + posLine, pByteFrom + from.posLine, firstBitsAmount, bidirectional)

    if (fullBytesPresent) crossMiddleNumbers(posLine, from.posLine, fullBytesTo, fullBytesFrom, bidirectional)

    if (toPresent) crossLastNumber(shiftTo, pByteTo + posLine, pByteTo + from.posLine, bidirectional)
  }

  private def crossMiddleNumbers(posTo: Int, posFrom: Int, to: Int, from: Int, bidirectional: Boolean) = {
    var pos = from
    while (pos <= to) {
      val posFromPos = posFrom + pos
      val tmp = allPools.numbers(posFromPos)
      val posToPos = posTo + pos
      if (bidirectional) allPools.numbers(posFromPos) = allPools.numbers(posToPos)
      allPools.numbers(posToPos) = tmp
      pos += 1
    }
  }

  private def crossFirstNumber(fromBit: Int, posTo: Int, posFrom: Int, bitsAmount: Int, bidirectional: Boolean) = {
    val mask = (-1L >>> (longBitsAmount - bitsAmount)) << fromBit
    val numFrom = allPools.numbers(posFrom)
    val numTo = allPools.numbers(posTo)
    val maskedTo = numTo & mask
    val maskedFrom = numFrom & mask
    allPools.numbers(posTo) = numTo & ~mask | maskedFrom
    if (bidirectional) allPools.numbers(posFrom) = numFrom & ~mask | maskedTo
  }

  private def crossLastNumber(bitsAmount: Int, posTo: Int, posFrom: Int, bidirectional: Boolean) = {
    val mask = -1L >>> (longBitsAmount - bitsAmount)
    val numFrom = allPools.numbers(posFrom)
    val numTo = allPools.numbers(posTo)
    val lastMaskedFrom = numFrom & mask
    val lastMaskedTo = numTo & mask
    allPools.numbers(posTo) = numTo & ~mask | lastMaskedFrom
    if (bidirectional) allPools.numbers(posFrom) = numFrom & ~mask | lastMaskedTo
  }

  override def toString: String = {
    var p = allPools.numbers.numberLinesAmount - 1
    var str = ""
    while (p >= 0) {
      str += toBinary(allPools.numbers(posLine + p)) + " "
      p -= 1
    }
    str.trim
  }

  def update(pos: Int, v: Boolean): Unit = if (v) setBit(pos) else clearBit(pos)

  def apply(pos: Int): Boolean = (allPools.numbers(posLine + pos / longBitsAmount) & numberMask(pos)) != 0

  def getNumber(pos: Int): Long = allPools.numbers(pos / longBitsAmount)

  private def setBit(pos: Int) = allPools.numbers(posLine + pos / longBitsAmount) |= numberMask(pos)

  private def clearBit(pos: Int) = allPools.numbers(posLine + pos / longBitsAmount) &= ~numberMask(pos)
}

object NumbersLine {
  def apply(str: String, posLine: Int, allPools: AllPools): NumbersLine = {
    val normalizedStr = str.filter(_ != ' ')
    val line = allPools.numberLines.next()
    line.posLine = posLine
    val positions = 0 until normalizedStr.length
    normalizedStr.zip(positions.reverse).foreach {
      case ('1', p) => line(p) = true
      case ('0', p) => line(p) = false
      case (c, p) => throw new IllegalArgumentException(s"Unexpected character: $c at position $p")
    }
    line
  }
}