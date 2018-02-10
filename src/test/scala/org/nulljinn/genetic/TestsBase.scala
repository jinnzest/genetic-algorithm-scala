package org.nulljinn.genetic

import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, WordSpec}

trait TestsBase extends WordSpec with MustMatchers with PropertyChecks {

  protected def toStr(chars: List[Char]): String = chars.foldLeft("")(_ + _)

  protected def groupByIntAndBytePos(str: String): String =
    str.grouped(longBitsAmount).foldLeft("")(_ + " " + _.grouped(4).foldLeft("")(_ + " " + _)).trim

  protected def groupByIntAndBytePos(chars: List[Char]): String = groupByIntAndBytePos(chars.foldLeft("")(_ + _))
}
