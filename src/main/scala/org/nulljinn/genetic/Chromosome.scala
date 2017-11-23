package org.nulljinn.genetic

import org.nulljinn.genetic.Gen._
import scala.collection.breakOut

case class Chromosome(dZygote: Zygote, rZygote: Zygote) {

  def decodeGenotype: Array[Boolean] = dZygote.genes.zip(rZygote.genes).map(
    Chromosome.decodingRules.apply
  )(breakOut)

  def crossZygotes(begin: Int, end: Int): Chromosome =
    Chromosome(dZygote.cross(rZygote, begin, end), rZygote.cross(dZygote, begin, end))

  def crossChromosomes(chr: Chromosome, begin: Int, end: Int): Chromosome =
    Chromosome(
      dZygote.cross(chr.dZygote, begin, end),
      rZygote.cross(chr.rZygote, begin, end)
    )

  def mutate(pos: Int, newGen: Gen): Chromosome =
    Chromosome(dZygote.mutate(pos, newGen), rZygote)

  override def toString: String = dZygote.toString + "\n" + rZygote.toString
}

object Chromosome {
  private val decodingRules = Map[(Gen, Gen), Boolean](
    (D1, D0) -> true,
    (D1, D1) -> true,
    (D1, R1) -> true,
    (D1, R0) -> true,

    (D0, D0) -> false,
    (D0, D1) -> false,
    (D0, R1) -> false,
    (D0, R0) -> false,

    (R1, D0) -> false,
    (R1, D1) -> true,
    (R1, R1) -> true,
    (R1, R0) -> true,

    (R0, D0) -> false,
    (R0, D1) -> true,
    (R0, R1) -> false,
    (R0, R0) -> false
  )

  def apply(z1: String, z2: String): Chromosome = Chromosome(Zygote(z1), Zygote(z2))
}

