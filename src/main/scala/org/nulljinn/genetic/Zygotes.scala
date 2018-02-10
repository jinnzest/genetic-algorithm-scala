package org.nulljinn.genetic

class Zygotes(val length: Int, var allPools: AllPools) extends Objects[Zygote] {
  override def newArray(): Array[Zygote] = Array.fill(tripleLength)(newObj())

  override def newObj() = new Zygote(allPools.numberLines.newObj(), allPools.numberLines.newObj(), 0, allPools)

  override def setAllPools(pools: AllPools): Unit = {
    allPools = pools
    array.foreach(_.allPools = allPools)
  }
}

