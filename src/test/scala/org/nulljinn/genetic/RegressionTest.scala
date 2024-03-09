package org.nulljinn.genetic

import org.scalatest.wordspec.AnyWordSpec

class RegressionTest extends AnyWordSpec {
  private val varsAmount = 3
  private val genesAmount = longBitsAmount * varsAmount
  private val chromosomesAmount = 5

  class RandomUtilsMock(val chromosomeGenesAmount: Int, genFrom: Gen, genTo: Gen) extends RandomUtils {
    private var pos = 0
    private var mutatedChromosomes = 0

    override def mutationPos(): Int = synchronized {
      if mutatedChromosomes == chromosomesAmount then
        mutatedChromosomes = 0
        if pos == chromosomeGenesAmount - 1 then pos = 0
        else pos += 1
      else mutatedChromosomes += 1
      pos
    }

    override def crossingChromosomePos(): Int = chromosomeGenesAmount / 2

    override def crossingZygotePos(): Int = ???

    override def shouldCrossZygotes(): Boolean = false

    override def shouldMutate(): Boolean = true

    override def randGen(): Gen = genTo

    override def selectIndividualProbability(fitness: Double): Boolean = if fitness >= 0.5 then true else false

    override def generateZygote(): Zygote = Zygote((0 until chromosomeGenesAmount).map(_ => genFrom.toChar).mkString)
  }

  private def runGenerations(genFrom: Gen, genTo: Gen, sign: Int) = {
    def allChromosomesAreDegenerated(chromosomes: Array[Chromosome]) = {
      chromosomes.forall(_.toString.filter(v => v != ' ').split('\n')(0).forall(_ == genTo.toChar))
    }

    def checkNoUnexpectedGenesAppeared(chromosomes: Array[Chromosome]) = {
      chromosomes.map {
        _.toString.filter(_ != ' ').split('\n')(0).map { g =>
          assert(g == genTo.toChar || g == genFrom.toChar)
        }
      }
    }

    val fitnessCalculator = new FitnessCalculator {
      override def calcFitness(bits: Array[Boolean]): Double = sign * bits.foldLeft(0.0) { (acc, v) =>
        if v then acc + 1 else acc
      }
    }
    val rand = new RandomUtilsMock(genesAmount, genFrom, genTo)

    val incubator = new IndividualsIncubator(
      chromosomesAmount, new Breeding(rand), fitnessCalculator
    )
    var genCnt = 0
    while (!allChromosomesAreDegenerated(incubator.getChromosomes)) {
      checkNoUnexpectedGenesAppeared(incubator.getChromosomes)
      incubator.makeNextGeneration()
      genCnt += 1
    }
    genCnt
  }

  "Breeding new generations should replace all genes by defined ones during a few generations" in {
    assert(runGenerations(Gen('d'), Gen('D'), 1) == 230)
    assert(runGenerations(Gen('r'), Gen('R'), 1) == 230)
    assert(runGenerations(Gen('R'), Gen('d'), -1) == 230)
    assert(runGenerations(Gen('r'), Gen('D'), 1) == 230)
  }
}
