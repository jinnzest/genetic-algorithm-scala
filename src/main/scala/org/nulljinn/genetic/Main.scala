package org.nulljinn.genetic

object Main {
  private val amountOfVariables = 20
  private val chromosomesAmount = 1000
  private val chromosomeGenesAmount = amountOfVariables * longBitsAmount

  private def createIncubator() = {
    val pools = AllPools(chromosomesAmount, amountOfVariables, longBitsAmount)
    val rand = new RandomUtilsImpl(chromosomeGenesAmount, pools)
    val fitnessCalculator = new FitnessCalculator {
      override def calcFitness(pos: Int): Double = {
        decodeBitsToNumbers(pos, pools.numbers)
        -funcToFindGlobalExtremum(pos)
      }

      private def funcToFindGlobalExtremum(pos: Int): Double = {
        var p = 0
        var prod = 1.0
        while (p < pools.numbers.numberLinesAmount) {
          val long = pools.numbers(pos + p)
          prod += long
          p += 1
        }
        prod
      }
    }

    new IndividualsIncubator(
      pools, new Breeding(rand), fitnessCalculator
    )
  }

  private val generationsAmount = 100000

  def main(args: Array[String]): Unit = {
    //    printIntermediateResults(runGenerationsForFitness(-1))
    printIntermediateResults(runGenerationsForCount(generationsAmount))
  }

  private def runGenerationsForCount(genesNum: Int): (IndividualsIncubator, Int) = {
    warmingUp()
    val incubator = createIncubator()
    var mark = System.currentTimeMillis()
    1 to genesNum foreach { _ =>
      incubator.makeNextGeneration()
    }
    val execTime = System.currentTimeMillis() - mark
    println(s"exec time = $execTime ms")
    (incubator, genesNum)
  }

  private def warmingUp(): Unit = {
    println("starting warming up ...")
    val incubator = createIncubator()
    1 to 10000 foreach { _ =>
      incubator.makeNextGeneration()
    }
    println("warming up is done")
    println("sleeping for 5 sec to allow GC to clean up memory")
    Thread.sleep(5000)
    println("running 100.000 generations...")
  }

  private def runGenerationsForFitness(finalFitness: Int): Int = {
    var genCount = 0
    val incubator = createIncubator()
    printIntermediateResults(incubator, genCount)
    while (incubator.getBestIndividual.fitness != finalFitness) {
      incubator.makeNextGeneration()
      if (genCount % 1000 == 0)
        printIntermediateResults(incubator, genCount)
      genCount += 1
    }
    genCount
  }

  private def printIntermediateResults(data: (IndividualsIncubator, Int)): Unit = {
    val (incubator, cnt) = data
    println("===================================================")
    println(s"genNum=$cnt")
    println(s"min=\n${incubator.getWorstIndividual}")
    println(s"max=\n${incubator.getBestIndividual}\n")
    val decodedArrayPos = incubator.getWorstIndividual.chromosome.decodeGenotype
    decodeBitsToNumbers(decodedArrayPos, incubator.getBestIndividual.chromosome.allPools.numbers)
    val worst = toStr(decodedArrayPos, incubator.getBestIndividual.chromosome.allPools.numbers)
    println(s"$worst")
    decodeBitsToNumbers(incubator.getBestIndividual.chromosome.decodeGenotype, incubator.getBestIndividual.chromosome.allPools.numbers)
    val best = toStr(decodedArrayPos, incubator.getBestIndividual.chromosome.allPools.numbers)
    println(s"$best\n\n\n")
    println(s"genNum=$cnt")
  }
}
