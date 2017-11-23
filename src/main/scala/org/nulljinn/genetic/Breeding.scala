package org.nulljinn.genetic

class Breeding(rand: RandomUtils) {
  def generateChromosome(): Chromosome = Chromosome(rand.generateZygote(), rand.generateZygote())

  def canBreed(fitness: Double, minFitness: Double, maxFitness: Double): Boolean =
    rand.selectIndividualProbability(normalizeFitness(fitness, minFitness, maxFitness))

  def conception(firstParent: Chromosome, secondParent: Chromosome): Chromosome =
    attemptCrossZygotes(
      attemptMutate(
        firstParent.crossChromosomes(secondParent, rand.crossingChromosomePos(), rand.crossingChromosomePos())
      )
    )

  private def attemptCrossZygotes(chr: Chromosome) =
    if (rand.shouldCrossZygotes())
      chr.crossZygotes(rand.crossingZygotePos(), rand.crossingZygotePos() + 1)
    else chr

  private def attemptMutate(chr: Chromosome) =
    if (rand.shouldMutate())
      chr.mutate(rand.mutationPos(), rand.randGen())
    else chr
}
