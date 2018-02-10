package org.nulljinn.genetic

import java.util.Random

import org.nulljinn.genetic.Gen._

class RandomUtilsImpl(val chromosomeGenesAmount: Int) extends RandomUtils {

  private val mutationProbability =  chromosomeGenesAmount / 10000.0
  private val crossProbability = 2 * chromosomeGenesAmount / 1000000.0

  //  private val rand = new SecureRandom() //TODO find a way to make it faster
  private val rand = new Random()

  override def selectIndividualProbability(fitness: Double): Boolean = fitness > rand.nextDouble()

  def generateZygote(): Zygote = {
    val len = chromosomeGenesAmount / longBitsAmount
    val nLen = len + (if (chromosomeGenesAmount % longBitsAmount > 0) 1 else 0)
    val dLine = NumbersLine(Array.fill(nLen)(len), chromosomeGenesAmount)
    val vLine = NumbersLine(Array.fill(nLen)(len), chromosomeGenesAmount)
    0 to chromosomeGenesAmount-longBitsAmount foreach { p =>
      dLine(p) = rand.nextBoolean()
      vLine(p) = rand.nextBoolean()
    }
    new Zygote(dLine, vLine)
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
