package org.nulljinn.genetic

case class AllPools(var numbers: Numbers, var chromosomesArray: Chromosomes, var zygotesArray: Zygotes, var numberLines: NumberLines)

object AllPools {
  def apply(chromosomesAmount: Int, varsAmount: Int, bitsPerItem: Int): AllPools = {
    val numbers = Numbers(chromosomesAmount, varsAmount * bitsPerItem)
    val pools = AllPools(numbers, null, null, new NumberLines(chromosomesAmount * 4, null))
    pools.numberLines.setAllPools(pools)
    pools.zygotesArray = new Zygotes(chromosomesAmount * 2, pools)
    pools.chromosomesArray = new Chromosomes(chromosomesAmount, pools)
    pools
  }
}