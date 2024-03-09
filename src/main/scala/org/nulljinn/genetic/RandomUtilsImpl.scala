package org.nulljinn.genetic

import org.nulljinn.genetic.Gen.*

import java.util.Random

class RandomUtilsImpl(val chromosomeGenesAmount: Int) extends RandomUtils:

  private val mutationProbability = chromosomeGenesAmount / 10000.0
  private val crossProbability = 2 * chromosomeGenesAmount / 1000000.0

  //  private val rand = new SecureRandom() //TODO find a way to make it faster
  private val rand = new Random()

  override def selectIndividualProbability(fitness: Double): Boolean = fitness > rand.nextDouble()

  def generateZygote() = new Zygote(Array.fill(chromosomeGenesAmount)(randGen()))

  override def randGen(): Gen =
    rand.nextInt(4) match
      case 0 => D1
      case 1 => D0
      case 2 => R1
      case 3 => R0

  private def randPos(): Int = rand.nextInt(chromosomeGenesAmount)

  override def mutationPos(): Int = randPos()

  override def crossingChromosomePos(): Int = randPos()

  override def crossingZygotePos(): Int = randPos()

  override def shouldCrossZygotes(): Boolean = rand.nextDouble() < crossProbability

  override def shouldMutate(): Boolean = rand.nextDouble() < mutationProbability
