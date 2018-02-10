package org.nulljinn.genetic


class IndividualsIncubator(allPools: AllPools, breeding: Breeding, fitnessCalculator: FitnessCalculator) {

  private val numbers = allPools.numbers

  private val parIds = 0 until allPools.numbers.chromosomesAmount

  private val individuals = new Individuals(numbers.chromosomesAmount, allPools)

  private var newGen =
    Generation(
      Array.fill[Individual](numbers.chromosomesAmount)(generateIndividual),
      breeding.canBreed
    )
  numbers.swapGenerations()
  private var oldGen =
    Generation(
      Array.fill[Individual](numbers.chromosomesAmount)(generateIndividual),
      breeding.canBreed
    )

  def getIndividuals: Array[Individual] = oldGen.individuals

  def getBestIndividual: Individual = oldGen.findBestIndividual()

  def getWorstIndividual: Individual = oldGen.findWorstIndividual()

  def createIndividuals(): Unit = {
    val parents = oldGen.selectParentPairs()
    numbers.swapGenerations()
    parIds.foreach { id =>
      val newChromosome = breeding.conception(parents(id).parentA.chromosome, parents(id).parentB.chromosome)
      val ind = individuals.next()
      ind.fitness = fitnessCalculator.calcFitness(newChromosome.decodeGenotype)
      ind.chromosome = newChromosome
      newGen.individuals(id) = ind
    }
  }

  def makeNextGeneration(): Unit = {
    createIndividuals()
    val tmpGen = oldGen
    oldGen = newGen
    newGen = tmpGen
    oldGen.recalculate()
  }

  def generateIndividual: Individual = {
    val chromosome = breeding.generateChromosome(allPools)
    val ind = individuals.next()
    ind.fitness = fitnessCalculator.calcFitness(chromosome.decodeGenotype)
    ind.chromosome = chromosome
    ind
  }

  override def toString: String = oldGen.toString

  def getChromosomes: Array[Chromosome] = oldGen.individuals.map(_.chromosome)
}
