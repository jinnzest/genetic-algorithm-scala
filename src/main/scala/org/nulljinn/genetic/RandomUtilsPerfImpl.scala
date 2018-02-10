package org.nulljinn.genetic

class RandomUtilsPerfImpl(override val chromosomeGenesAmount: Int, override val allPools: AllPools) extends RandomUtilsImpl(chromosomeGenesAmount, allPools) {
  override def selectIndividualProbability(fitness: Double): Boolean = true
}
