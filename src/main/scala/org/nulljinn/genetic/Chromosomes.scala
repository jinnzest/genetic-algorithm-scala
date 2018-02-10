package org.nulljinn.genetic

class Chromosomes(val length: Int, val allPools: AllPools) extends Objects[Chromosome] {
  override def newArray(): Array[Chromosome] = Array.fill(tripleLength)(newObj())

  override def newObj() = new Chromosome(allPools.zygotesArray.newObj(), allPools.zygotesArray.newObj(), 0, allPools)

  override def setAllPools(allPools: AllPools): Unit = array.foreach(_.allPools = allPools)

  override def toString: String = {
    val stringBuilder = new StringBuilder()
    array.foreach(chr =>
      stringBuilder.append(chr.toString+"\n\n")
    )
    stringBuilder.toString
  }
}