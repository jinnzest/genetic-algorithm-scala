package org.nulljinn.genetic

import org.scalatest.wordspec.AnyWordSpec

class GenerationTest extends AnyWordSpec:

  val defaultCanBreedMock: (Double, Double, Double) => Boolean = (_, _, _) => true

  "Generation" when :
    "selecting new parent pairs" should :
      val chr = Chromosome("", "")
      "find exactly the same amount of pairs as initial generation size" in :
        val size = 5
        val individuals = Array.fill(size)(Individual(0.5, chr))
        val result = Generation(individuals, defaultCanBreedMock).selectParentPairs()
        assert(result.length == size)

      "find only parents which are selected by canBreedMock function" in :
        val size = 6
        val canBreedMock: (Double, Double, Double) => Boolean = (f, _, _) => if f > 0.5 then true else false
        val bestIndividuals = Array.fill(size / 2)(Individual(1, chr))
        val worstIndividuals = Array.fill(size / 2)(Individual(0, chr))
        val mergedIndividuals = bestIndividuals ++ worstIndividuals
        val result = Generation(mergedIndividuals, canBreedMock).selectParentPairs()
        val bestParents = result.filter: (firstParent, secondParent) =>
          firstParent.fitness > 0.5 && secondParent.fitness > 0.5

        val worstParents = result.filter: (firstParent, secondParent) =>
          firstParent.fitness < 0.5 || secondParent.fitness < 0.5

        assert(bestParents.length == size)
        assert(worstParents.isEmpty)

      "make worstIndividual to be equal to worst individual from generation" in :
        val size = 5
        val individuals = Array.fill(size)(Individual(0.5, chr))
        val worstIndividual = Individual(0.0, chr)
        val foundWorstIndividual = Generation(individuals :+ worstIndividual, defaultCanBreedMock).findWorstIndividual()
        assert(foundWorstIndividual == worstIndividual)