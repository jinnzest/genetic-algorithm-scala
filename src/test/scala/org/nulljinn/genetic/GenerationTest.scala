package org.nulljinn.genetic

import org.scalacheck.Gen.nonEmptyListOf
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.{Gen => SCGen}

class GenerationTest extends AnyWordSpec {

  val defaultCanBreedMock: (Double, Double, Double) => Boolean = (_, _, _) => true

  "Generation" when {
    "selecting new parent pairs" should {
      val chr = Chromosome("", "")
      "find exactly the same amount of pairs as initial generation size" in {
        val size = 5
        val individuals = Array.fill(size)(Individual(0.5, chr))
        val result = Generation(individuals, defaultCanBreedMock).selectParentPairs()
        assert(result.length == size)
      }
      "find only parents which are selected by canBreedMock function" in {
        val size = 6
        val canBreedMock: (Double, Double, Double) => Boolean = (f, _, _) => if (f > 0.5) true else false
        val bestIndividuals = Array.fill(size / 2)(Individual(1, chr))
        val worstIndividuals = Array.fill(size / 2)(Individual(0, chr))
        val mergedIndividuals = bestIndividuals ++ worstIndividuals
        val result = Generation(mergedIndividuals, canBreedMock).selectParentPairs()
        val bestParents = result.filter { parents =>
          val (fstParent, scndParent) = parents
          fstParent.fitness > 0.5 && scndParent.fitness > 0.5
        }
        val worstParents = result.filter { parents =>
          val (fstParent, scndParent) = parents
          fstParent.fitness < 0.5 || scndParent.fitness < 0.5
        }
        assert(bestParents.length == size)
        assert(worstParents.isEmpty)
      }
      "make worstIndividual to be equal to worst individual from generation" in {
        val size = 5
        val individuals = Array.fill(size)(Individual(0.5, chr))
        val worstIndividual = Individual(0.0, chr)
        val foundWorstIndividual = Generation(individuals :+ worstIndividual, defaultCanBreedMock).findWorstIndividual()
        assert(foundWorstIndividual == worstIndividual)
      }
      "make worstIndividual to be bigger than worst individual if fitness of the last one is worst 20 times then the overage by generation" in {
        val worseButBetterThatWorst = Individual(1001, chr)
        val individuals = Array(worseButBetterThatWorst, Individual(1002, chr), Individual(1003, chr), Individual(1004, chr),
          Individual(1, chr))
        val foundWorstIndividual = Generation(individuals, defaultCanBreedMock).findWorstIndividual()
        assert(foundWorstIndividual == worseButBetterThatWorst)
      }
      "get overage fitness of generation" in forAll(nonEmptyListOf(SCGen.double)) { individualsFitness: List[Double] =>
        val individuals = individualsFitness.map(f => Individual(f, chr))
        val foundOverageFitness = Generation(individuals.toArray, defaultCanBreedMock).overageFitness
        assert(foundOverageFitness == (individualsFitness.sum / individualsFitness.length))
      }
    }
  }
}
