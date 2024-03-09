package org.nulljinn.genetic

enum Gen:
  case D1
  case D0
  case R1
  case R0

  override def toString: String = this match
    case Gen.D1 => "D"
    case Gen.D0 => "d"
    case Gen.R1 => "R"
    case Gen.R0 => "r"

  def toChar: Char = this match
    case Gen.D1 => 'D'
    case Gen.D0 => 'd'
    case Gen.R1 => 'R'
    case Gen.R0 => 'r'

object Gen:

  def apply(chr: Char): Gen = chr match
    case 'D' => Gen.D1
    case 'd' => Gen.D0
    case 'R' => Gen.R1
    case 'r' => Gen.R0
