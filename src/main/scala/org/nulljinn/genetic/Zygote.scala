package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._


case class Zygote(dominance: NumbersLine, values: NumbersLine) {

  def cloneZygote() = {
    Zygote(dominance.cloneLine(), values.cloneLine())
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
    (0 until dominance.size)
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

  def apply(s: String): Zygote = {
    val normalizedStr = s.filterNot(_ == ' ')
    val length = normalizedStr.length
    val len = length / longBitsAmount
    val nLen = len + 1
    val zgt = Zygote(new NumbersLine(Array.fill[Long](nLen)(0), length), new NumbersLine(Array.fill[Long](nLen)(0), length))
    var p = length - 1
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
}
