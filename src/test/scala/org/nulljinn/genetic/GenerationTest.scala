package org.nulljinn.genetic

import org.scalacheck.Arbitrary.arbitrary

class GenerationTest extends TestsBase {

  val defaultCanBreedMock: (Double, Double, Double) => Boolean = (_, _, _) => true

  "Generation" when {
    "selecting new parent pairs" should {
      val allPools = AllPools(10, 1, 2)
      val chr = Chromosome.apply("", "", allPools)
      "find exactly the same amount of pairs as initial generation size" in {
        val size = 5
        val individuals = Array.fill(size)(new Individual(0.5, chr, allPools))
        val result = Generation(individuals, defaultCanBreedMock).selectParentPairs()
        result.length mustBe size
      }
      "find only parents which are selected by canBreedMock function" in {
        val size = 6
        val canBreedMock: (Double, Double, Double) => Boolean = (f, min, max) => if (f > 0.5) true else false
        val bestIndividuals = Array.fill(size / 2)(new Individual(1, chr, allPools))
        val worstIndividuals = Array.fill(size / 2)(new Individual(0, chr, allPools))
        val mergedIndividuals = bestIndividuals ++ worstIndividuals
        val result = Generation(mergedIndividuals, canBreedMock).selectParentPairs()
        val bestParents = result.filter { parents =>
          parents.parentA.fitness > 0.5 && parents.parentB.fitness > 0.5
        }
        val worstParents = result.filter { parents =>
          parents.parentA.fitness < 0.5 || parents.parentB.fitness < 0.5
        }
        bestParents.length mustBe size
        worstParents mustBe empty
      }
      "make worstIndividual to be equal to worst individual from generation" in {
        val size = 5
        val individuals = Array.fill(size)(new Individual(0.5, chr, allPools))
        val worstIndividual = new Individual(0.0, chr, allPools)
        val foundWorstIndividual = Generation(individuals :+ worstIndividual, defaultCanBreedMock).findWorstIndividual()
        foundWorstIndividual mustBe worstIndividual
      }
      "make worstIndividual to be bigger than worst individual if fitness of the last one is worst 20 times then the overage by generation" in {
        val worseButBetterThatWorst = new Individual(1001, chr, allPools)
        val individuals = Array(worseButBetterThatWorst, new Individual(1002, chr, allPools), new Individual(1003, chr, allPools), new Individual(1004, chr, allPools),
          new Individual(1, chr, allPools))
        val foundWorstIndividual = Generation(individuals, defaultCanBreedMock).findWorstIndividual()
        foundWorstIndividual mustBe worseButBetterThatWorst
      }
      "get overage fitness of generation" in forAll(arbitrary[Array[Double]].filter(_.length > 0)) { fitnesses =>
        val individuals = fitnesses.map(f => new Individual(f, chr, allPools))
        val foundOverageFitness = Generation(individuals, defaultCanBreedMock).overageFitness
        foundOverageFitness mustBe (fitnesses.sum / fitnesses.length)
      }
    }
  }
}
