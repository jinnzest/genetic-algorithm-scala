package org.nulljinn

package object genetic:

  val genesPerGroup = 4
  val longBitsAmount = 64

  def normalizeFitness(fitness: Double, minFitness: Double, maxFitness: Double): Double =
    val basedFitness = fitness - minFitness
    val fitnessRange = maxFitness - minFitness
    if fitnessRange == 0.0 then 1.0
    else basedFitness / fitnessRange

  def gray2bin(num: Long): Long =
    var r = num
    var mask = r >>> 1L
    while mask != 0 do
      r = r ^ mask
      mask = mask >>> 1L
    r

  def decodeBitsToNumbers(numbers: Array[Long]): Array[Long] =
    var p = 0
    val decoded = numbers.clone()
    while p < numbers.length do
      decoded(p) = gray2bin(numbers(p))
      p += 1
    decoded

  def toStr(numbers: Array[Long]): String =
    var p = 0
    var str = ""
    while p < numbers.length do
      str += numbers(p) + " "
      p += 1
    str

  def toBinary(n: Long): String =
    var pos = 0
    var m = 1L
    var str = ""
    while pos < longBitsAmount do
      val ch = if ((n & m) != 0) '1' else '0'
      str = ch + str
      m <<= 1L
      pos += 1
    groupBy4(str)

  private def groupBy4(str: String) =
    str.grouped(genesPerGroup).foldLeft(""): (acc, v) =>
      acc + " " + v

  def numberMask(pos: Int): Long = 1L << pos % longBitsAmount