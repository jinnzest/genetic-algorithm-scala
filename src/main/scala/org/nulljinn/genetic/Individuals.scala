package org.nulljinn.genetic

class Individuals(val length: Int, val allPools: AllPools) extends Objects[Individual] {
  override def newArray(): Array[Individual] = Array.fill(tripleLength)(newObj())

  override def newObj() = new Individual(0.0, allPools.chromosomesArray.newObj(), allPools)

  override def setAllPools(allPools: AllPools): Unit = array.foreach(_.allPools = allPools)
}