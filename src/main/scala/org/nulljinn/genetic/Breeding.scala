package org.nulljinn.genetic

class Breeding(rand: RandomUtils) {
  def generateChromosome(allPools: AllPools): Chromosome = {
    val numbers = allPools.numbers
    val chrPos = numbers.obtainPos()
    val dZygote = rand.generateZygote(numbers.dominantZygotePos(chrPos))
    val rZygote = rand.generateZygote(numbers.recessiveZygotePos(chrPos))
    val chr = allPools.chromosomesArray.next()
    chr.dZygote = dZygote
    chr.rZygote = rZygote
    chr.allPools = allPools
    chr.chrPos = chrPos
    chr
  }

  def canBreed(fitness: Double, minFitness: Double, maxFitness: Double): Boolean =
    rand.selectIndividualProbability(normalizeFitness(fitness, minFitness, maxFitness))

  def conception(firstParent: Chromosome, secondParent: Chromosome): Chromosome =
    attemptCrossZygotes(
      attemptMutate(
        firstParent.crossChromosomes(secondParent, rand.crossingChromosomePos(), rand.crossingChromosomePos())
      )
    )

  private def attemptCrossZygotes(chr: Chromosome): Chromosome =
    if (rand.shouldCrossZygotes())
      chr.crossZygotes(rand.crossingZygotePos(), rand.crossingZygotePos() + 1)
    else chr

  private def attemptMutate(chr: Chromosome): Chromosome =
    if (rand.shouldMutate())
      chr.mutate(rand.mutationPos(), rand.randGen())
    else chr
}
