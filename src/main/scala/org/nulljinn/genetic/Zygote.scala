package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._


class Zygote(var dominance: NumbersLine, var values: NumbersLine, var zygotePos: Int, var allPools: AllPools) {

  def copyBits(from: Zygote): Unit = {
    dominance.copyBits(from.dominance)
    values.copyBits(from.values)
  }

  def mutate(pos: Int, newGen: Gen): Zygote = {
    this (pos) = newGen
    this
  }

  def cross(from: Zygote, fromBit: Int, bitsAmount: Int, bidirectional: Boolean): Zygote = {
    dominance.crossBits(from.dominance, fromBit, bitsAmount, bidirectional)
    values.crossBits(from.values, fromBit, bitsAmount, bidirectional)
    this
  }

  override def toString: String = {
    (0 until allPools.numbers.lineBitsAmount)
      .foldRight(new StringBuilder) { (pos, acc) =>
        acc.append((dominance(pos), values(pos)) match {
          case (true, true) => 'D'
          case (true, false) => 'd'
          case (false, true) => 'R'
          case (false, false) => 'r'
        })
      }
      .grouped(longBitsAmount).foldLeft(new StringBuilder) { (acc, v) =>
        acc.append(" ")
        acc.append(v.grouped(4).foldLeft(new StringBuilder) { (acc, v) =>
          acc.append(" ")
          acc.append(v)
          acc
        })
        acc
      }.toString.trim
  }

  def update(p: Int, g: Gen): Unit = {
    g match {
      case D1 => dominance(p) = true; values(p) = true
      case D0 => dominance(p) = true; values(p) = false
      case R1 => dominance(p) = false; values(p) = true
      case R0 => dominance(p) = false; values(p) = false
    }
  }

  def apply(p: Int): Gen = (dominance(p), values(p)) match {
    case (true, true) => D1
    case (true, false) => D0
    case (false, true) => R1
    case (false, false) => R0
  }
}

object Zygote {
  //being used in tests only
  def apply(s: String, pos: Int, allPools: AllPools): Zygote = {
    val zgt = allPools.zygotesArray.next()
    zgt.zygotePos = pos
    zgt.dominance = allPools.numberLines.next()
    zgt.dominance.posLine = allPools.numbers.dominantZygotePos(pos)
    zgt.values = allPools.numberLines.next()
    zgt.values.posLine = allPools.numbers.valuesPos(pos)
    val normalizedStr = s.filterNot(_ == ' ')
    var p = normalizedStr.length - 1
    normalizedStr.foreach { v =>
      v match {
        case 'D' => zgt.dominance(p) = true; zgt.values(p) = true
        case 'd' => zgt.dominance(p) = true; zgt.values(p) = false
        case 'r' => zgt.dominance(p) = false; zgt.values(p) = false
        case 'R' => zgt.dominance(p) = false; zgt.values(p) = true
      }
      p -= 1
    }
    zgt
  }

  def newDL(pos: Int, allPools: AllPools): NumbersLine = {
    val line = allPools.numberLines.next()
    line.posLine = allPools.numbers.dominancePos(pos)
    line.allPools = allPools
    line
  }

  def newVL(pos: Int, allPools: AllPools): NumbersLine = {
    val line = allPools.numberLines.next()
    line.posLine = allPools.numbers.valuesPos(pos)
    line.allPools = allPools
    line
  }
}
