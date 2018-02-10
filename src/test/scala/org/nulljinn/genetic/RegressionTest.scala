package org.nulljinn.genetic

import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec

class RegressionTest extends AnyWordSpec {
  private val varsAmount = 3
  private val genesAmount = longBitsAmount * varsAmount
  private val chromosomesAmount = 5

  class RandomUtilsMock(val chromosomeGenesAmount: Int, genFrom: Gen, genTo: Gen, allPools: AllPools) extends RandomUtils {
    private var pos = 0
    private var mutatedChromosomes = 0

    override def mutationPos(): Int = synchronized {
      if (mutatedChromosomes == chromosomesAmount) {
        mutatedChromosomes = 0
        if (pos == chromosomeGenesAmount - 1) pos = 0
        else pos += 1
      }
      else mutatedChromosomes += 1
      pos
    }

    override def crossingChromosomePos(): Int = chromosomeGenesAmount / 2

    override def crossingZygotePos(): Int = ???

    override def shouldCrossZygotes(): Boolean = false

    override def shouldMutate(): Boolean = true

    override def randGen(): Gen = genTo

    override def selectIndividualProbability(fitness: Double): Boolean = if (fitness >= 0.5) true else false

    override def generateZygote(pos: Int): Zygote = Zygote((0 until chromosomeGenesAmount).map(_ => genFrom.toChar).mkString, pos, allPools)
  }

  private def runGenerations(genFrom: Gen, genTo: Gen, sign: Int): Int = {
    val pools = AllPools(chromosomesAmount, varsAmount, longBitsAmount)

    def allChromosomesAreDegenerated(chromosomes: Array[Chromosome]): Boolean = {
      chromosomes.forall(_.toString.filter(v => v != ' ').split('\n')(0).forall(_ == genTo.toChar))
    }

    def checkNoUnexpectedGenesAppeared(chromosomes: Array[Chromosome]): Array[IndexedSeq[Assertion]] = {
      chromosomes.map {
        _.toString.filter(v => v != ' ').split('\n')(0).map { g =>
          assert(g == genTo.toChar || g == genFrom.toChar)
        }
      }
    }

    val fitnessCalculator = new FitnessCalculator {
      override def calcFitness(pos: Int): Double = {
        var p = 0
        var acc = 0.0
        while (p < pools.numbers.numberLinesAmount) {
          var bitMask = 1L
          val v = pools.numbers(pos + p)
          acc += (0 until longBitsAmount).foldLeft(0.0) { (acc2, _) =>
            val res = acc2 + (if ((v & bitMask) != 0) 1 else 0)
            bitMask <<= 1L
            res
          }
          p += 1
        }
        sign * acc
      }
    }
    val rand = new RandomUtilsMock(genesAmount, genFrom, genTo, pools)
    pools.numbers.swapGenerations()
    val incubator = new IndividualsIncubator(
      pools, new Breeding(rand), fitnessCalculator
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
    runGenerations(Gen('d'), Gen('D'), 1) == 230
    runGenerations(Gen('r'), Gen('R'), 1) == 230
    runGenerations(Gen('R'), Gen('d'), -1) == 230
    runGenerations(Gen('r'), Gen('D'), 1) == 230
  }

}
