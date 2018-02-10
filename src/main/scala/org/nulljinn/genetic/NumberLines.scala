package org.nulljinn.genetic

class NumberLines(val length: Int, var allPools: AllPools) extends Objects[NumbersLine] {
  override def newArray(): Array[NumbersLine] = Array.fill(tripleLength)(newObj())

  override def newObj() = new NumbersLine(0, allPools)

  override def setAllPools(pools: AllPools): Unit = {
    allPools = pools
    array.foreach(_.allPools = allPools)
  }
}