package org.nulljinn.genetic

trait RandomUtils:

  def chromosomeGenesAmount: Int

  def mutationPos(): Int

  def crossingChromosomePos(): Int

  def crossingZygotePos(): Int

  def shouldCrossZygotes(): Boolean

  def shouldMutate(): Boolean

  def randGen(): Gen

  def generateZygote(): Zygote

  def selectIndividualProbability(fitness: Double): Boolean
