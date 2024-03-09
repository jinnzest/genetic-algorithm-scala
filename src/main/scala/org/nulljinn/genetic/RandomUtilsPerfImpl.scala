package org.nulljinn.genetic

class RandomUtilsPerfImpl(override val chromosomeGenesAmount: Int) extends RandomUtilsImpl(chromosomeGenesAmount):
  override def selectIndividualProbability(fitness: Double): Boolean = true
