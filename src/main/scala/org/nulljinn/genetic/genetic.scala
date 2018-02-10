package org.nulljinn

package object genetic {

  val genesPerGroup = 4
  val longBitsAmount = 64

  def normalizeFitness(fitness: Double, minFitness: Double, maxFitness: Double): Double = {
    val basedFitness = fitness - minFitness
    val fitnessRange = maxFitness - minFitness
    if (fitness < minFitness) 0.0
    else if (fitnessRange != 0.0)
      basedFitness / fitnessRange
    else 1.0
  }

  def gray2bin(num: Long): Long = {
    var r = (num >>> 32) ^ num
    r ^= r >>> 16
    r ^= r >>> 8
    r ^= r >>> 4
    r ^= r >>> 2
    r ^= r >>> 1
    r
  }

  def decodeBitsToNumbers(numbers: Array[Long]): Array[Long] = {
    var p = 0
    val decoded = numbers.clone()
    while (p <  numbers.length) {
      decoded(p) = gray2bin(numbers(p))
      p += 1
    }
    decoded
  }

  def toStr(numbers: Array[Long]): String = {
    var p = 0
    var str = ""
    while (p < numbers.length) {
      str += numbers(p) + " "
      p += 1
    }
    str
  }

  def toBinary(n: Long): String = {
    var pos = 0
    var m = 1L
    var str = ""
    while (pos < longBitsAmount) {
      val ch = if ((n & m) != 0) '1' else '0'
      str = ch + str
      m <<= 1L
      pos += 1
    }
    groupBy4(str)
  }

  private def groupBy4(str: String) = {
    str.grouped(genesPerGroup).foldLeft("") { (acc, v) =>
      acc + " " + v
    }
  }

  def numberMask(pos: Int): Long = 1L << pos % longBitsAmount
}
