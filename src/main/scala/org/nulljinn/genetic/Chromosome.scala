package org.nulljinn.genetic

case class Chromosome(dZygote: Zygote, rZygote: Zygote) {

  private lazy val length: Int = dZygote.dominance.numbers.length

  def decodeGenotype: Array[Long] = {
    var p = 0
    val decoded = Array.fill(length)(0L)
    while (p < decoded.length) {
      val dd = dZygote.dominance.numbers(p)
      val dv = dZygote.values.numbers(p)
      val rd = rZygote.dominance.numbers(p)
      val rv = rZygote.values.numbers(p)
      decoded(p) = dv & ~rd | rd & rv & ~dd | dd & dv
      p += 1
    }
    decoded
  }

  def crossZygotes(begin: Int, amount: Int): Chromosome = {
    val normalizedAmount = if (begin + amount < dZygote.values.size) amount else length - begin
    dZygote.cross(rZygote, begin, normalizedAmount, true)
    this
  }

  def crossChromosomes(chr: Chromosome, begin: Int, amount: Int): Chromosome = {
    val newChr = cloneChr()
    newChr.dZygote.cross(chr.dZygote, begin, amount, false)
    newChr.rZygote.cross(chr.rZygote, begin, amount, false)
    newChr
  }

  private def cloneChr(): Chromosome = {
    val newDZygote = dZygote.cloneZygote()
    val newRZygote = rZygote.cloneZygote()
    Chromosome(newDZygote, newRZygote)
  }

  def mutate(pos: Int, newGen: Gen): Chromosome = {
    dZygote.mutate(pos, newGen)
    this
  }

  override def toString: String = dZygote.toString + "\n" + rZygote.toString
}

object Chromosome {
  def apply(z1: String, z2: String): Chromosome =
    Chromosome(Zygote(z1), Zygote(z2))
}

