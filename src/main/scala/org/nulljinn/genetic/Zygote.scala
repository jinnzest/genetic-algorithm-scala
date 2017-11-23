package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._

case class Zygote(genes: Array[Gen]) {

  def mutate(pos: Int, newGen: Gen): Zygote = {
    val newGenes:Array[Gen] = genes.updated(pos, newGen)
    Zygote(newGenes)
  }

  private def splitGenes(begin: Int, end: Int): (Array[Gen], Array[Gen], Array[Gen]) = {
    val (head, tail) = genes.splitAt(begin)
    val (headOfTail, tailOfTail) = tail.splitAt(end)
    (head, headOfTail, tailOfTail)
  }

  def cross(zgt: Zygote, begin: Int, end: Int): Zygote = {
    val (h1, _, t1) = splitGenes(begin, end)
    val (_, m2, _) = zgt.splitGenes(begin, end)
    new Zygote(h1 ++ m2 ++ t1)
  }

  override def toString: String = genes.foldLeft("") { (acc, g) =>
    acc + g.toString
  }.grouped(longBitsAmount).foldLeft("")(_ + " " + _.grouped(4).foldLeft("")(_ + " " + _)).trim.reverse
}

object Zygote {
  def apply(s: String): Zygote = Zygote(s.filterNot(_ == ' ').map {
    case 'D' => D1
    case 'd' => D0
    case 'r' => R0
    case 'R' => R1
  }.toArray[Gen].reverse)
}
