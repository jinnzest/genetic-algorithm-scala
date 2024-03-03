package org.nulljinn.genetic

object Shared {
  def toStr(chars: List[Char]): String = chars.foldLeft("")(_ + _)

  def groupByIntAndBytePos(str: String): String =
    str.grouped(longBitsAmount).foldLeft("")(_ + " " + _.grouped(4).foldLeft("")(_ + " " + _)).trim

  def groupByIntAndBytePos(chars: List[Char]): String = groupByIntAndBytePos(chars.foldLeft("")(_ + _))
}
