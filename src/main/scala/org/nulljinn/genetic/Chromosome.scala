package org.nulljinn.genetic


class Chromosome(var dZygote: Zygote, var rZygote: Zygote, var chrPos: Int, var allPools: AllPools) {

  def decodeGenotype: Int = {
    var p = 0
    val dZygotePos = allPools.numbers.dominantZygotePos(chrPos)
    val dDominancePos = allPools.numbers.dominancePos(dZygotePos)
    val dValuesPos = allPools.numbers.valuesPos(dZygotePos)
    val rZygotePos = allPools.numbers.recessiveZygotePos(chrPos)
    val rDominancePos = allPools.numbers.dominancePos(rZygotePos)
    val rValuesPos = allPools.numbers.valuesPos(rZygotePos)
    val decodedPos = allPools.numbers.tmpNumbrsPos(chrPos)
    val amount = allPools.numbers.numberLinesAmount
    while (p < amount) {
      val dd = allPools.numbers(dDominancePos + p)
      val dv = allPools.numbers(dValuesPos + p)
      val rd = allPools.numbers(rDominancePos + p)
      val rv = allPools.numbers(rValuesPos + p)
      allPools.numbers(decodedPos + p) = dv & ~rd | rd & rv & ~dd | dd & dv
      p += 1
    }
    decodedPos
  }

  def crossZygotes(begin: Int, amount: Int): Chromosome = {
    val normalizedAmount = if (begin + amount < allPools.numbers.lineBitsAmount) amount else allPools.numbers.lineBitsAmount - begin
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
    val chrNewPos = allPools.numbers.obtainPos()
    val newDZygote = Chromosome.newDZ(chrNewPos, allPools)
    val newRZygote = Chromosome.newRZ(chrNewPos, allPools)
    newDZygote.copyBits(dZygote)
    newRZygote.copyBits(rZygote)
    val chr = allPools.chromosomesArray.next()
    chr.dZygote = newDZygote
    chr.rZygote = newRZygote
    chr.chrPos = chrNewPos
    chr.allPools = allPools
    chr
  }

  def mutate(pos: Int, newGen: Gen): Chromosome = {
    dZygote.mutate(pos, newGen)
    this
  }

  override def toString: String = dZygote.toString + "\n" + rZygote.toString
}

object Chromosome {

  //being used in tests only
  def apply(z1: String, z2: String, allPools: AllPools): Chromosome = {
    val numbersArray = allPools.numbers
    val pos = numbersArray.obtainPos()
    val dZgt = Zygote(z1, numbersArray.dominantZygotePos(pos), allPools)
    val rZgt = Zygote(z2, numbersArray.recessiveZygotePos(pos), allPools)
    new Chromosome(dZgt, rZgt, pos, allPools)
  }

  def newDZ(pos: Int, allPools: AllPools): Zygote = {
    val zgt = allPools.zygotesArray.next()
    zgt.zygotePos = allPools.numbers.dominantZygotePos(pos)
    zgt.allPools = allPools
    zgt.dominance.posLine = allPools.numbers.dominancePos(zgt.zygotePos)
    zgt.values.posLine = allPools.numbers.valuesPos(zgt.zygotePos)
    zgt
  }

  def newRZ(pos: Int, allPools: AllPools): Zygote = {
    val zgt = allPools.zygotesArray.next()
    zgt.zygotePos = allPools.numbers.recessiveZygotePos(pos)
    zgt.allPools = allPools
    zgt.dominance.posLine = allPools.numbers.dominancePos(zgt.zygotePos)
    zgt.values.posLine = allPools.numbers.valuesPos(zgt.zygotePos)
    zgt
  }
}

