package org.nulljinn.genetic

import java.util.concurrent.ThreadLocalRandom

import org.nulljinn.genetic.Gen._

class RandomUtilsImpl(val chromosomeGenesAmount: Int, val allPools: AllPools) extends RandomUtils {

  private val numbers = allPools.numbers
  private val zygoteBitsAmount = numbers.lineBitsAmount
  private val mutationProbability = chromosomeGenesAmount / 10000.0
  private val crossProbability = 2 * chromosomeGenesAmount / 1000000.0

  //  private val rand = new SecureRandom() //TODO find a way to make it faster
  private def rand = ThreadLocalRandom.current()

  override def selectIndividualProbability(fitness: Double): Boolean = fitness > rand.nextDouble()

  def generateZygote(pos: Int): Zygote = {
    val dLine = Zygote.newDL(pos, allPools)
    val vLine = Zygote.newVL(pos, allPools)
    0 until zygoteBitsAmount foreach { p =>
      dLine(p) = rand.nextBoolean()
      vLine(p) = rand.nextBoolean()
    }
    val zgt = allPools.zygotesArray.next()
    zgt.dominance = dLine
    zgt.values = vLine
    zgt.allPools = allPools
    zgt
  }

  override def randGen(): Gen = {
    rand.nextInt(4) match {
      case 0 => D1
      case 1 => D0
      case 2 => R1
      case 3 => R0
    }
  }

  private def randPos(): Int = rand.nextInt(chromosomeGenesAmount)

  override def mutationPos(): Int = randPos()

  override def crossingChromosomePos(): Int = randPos()

  override def crossingZygotePos(): Int = randPos()

  override def shouldCrossZygotes(): Boolean = rand.nextDouble() < crossProbability

  override def shouldMutate(): Boolean = rand.nextDouble() < mutationProbability
}
