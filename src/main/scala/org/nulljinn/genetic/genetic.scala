package org.nulljinn

package object genetic {

  val longBitsAmount = 64

  def normalizeFitness(fitness: Double, minFitness: Double, maxFitness: Double): Double = {
    val basedFitness = fitness - minFitness
    val fitnessRange = maxFitness - minFitness
    if (fitness < minFitness) 0.0
    else if (fitnessRange != 0.0)
      basedFitness / fitnessRange
    else 1.0
  }

  def gray2bin(n: Array[Boolean]): Array[Boolean] = {
    var pos = -1
    n.map { v =>
      pos += 1
      xorUntilPos(n, pos)
    }
  }

  def decodeBitsToNumbers(bits: Array[Boolean]): Array[Long] = {
    val len = bits.length
    val numbersAmount = len / longBitsAmount + (if (len % longBitsAmount > 0) 1 else 0)
    var numbers = Array.fill(numbersAmount)(0L)
    var pos = numbersAmount - 1
    var shift = (numbersAmount - 1) * longBitsAmount
    val shiftSize = if (longBitsAmount > len) len else longBitsAmount
    while (pos >= 0) {
      val numBits = Array.fill(shiftSize)(false)
      Array.copy(bits, shift, numBits, 0, shiftSize)
      numbers = numbers.updated(pos, toNumber(gray2bin(numBits)))
      shift -= shiftSize
      pos -= 1
    }
    numbers
  }


  def toNumber(a: Array[Boolean]): Long = {
    var n = 0L
    val len = a.length
    var i = 0
    while (
      i < len
    ) {
      n = (n << 1) + (if (a(i)) 1 else 0)
      i += 1
    }
    n
  }

  private def xorUntilPos(n: Array[Boolean], posTo: Int) = {
    var value = false
    var pos = 0
    while (pos <= posTo) {
      value = value ^ n(pos)
      pos = pos + 1
    }
    value
  }
}
