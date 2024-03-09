package org.nulljinn.genetic

class IndividualsIncubator(chromosomesAmount: Int, breeding: Breeding, fitnessCalculator: FitnessCalculator):
  private var generation =
    Generation(
      Array.fill[Individual](chromosomesAmount)(generateIndividual),
      breeding.canBreed
    )

  def getIndividuals: Array[Individual] = generation.individuals

  def getBestIndividual: Individual = generation.findBestIndividual()

  def getWorstIndividual: Individual = generation.findWorstIndividual()

  def createIndividuals(): Array[Individual] =
    generation
      .selectParentPairs()
      .map { (firstParent, secondParent) =>
        val newChromosome = breeding.conception(firstParent.chromosome, secondParent.chromosome)
        Individual(fitnessCalculator.calcFitness(newChromosome.decodeGenotype), newChromosome)
      }

  def makeNextGeneration(): Unit =
    generation = Generation(
      createIndividuals(),
      breeding.canBreed
    )

  private def generateIndividual: Individual =
    val chromosome = breeding.generateChromosome()
    Individual(
      fitnessCalculator.calcFitness(chromosome.decodeGenotype), chromosome
    )

  def getChromosomes: Array[Chromosome] = generation.individuals.map(_.chromosome)
