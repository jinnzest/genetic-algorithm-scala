package org.nulljinn.genetic

object Main {
  private val amountOfVariables = 20
  private val chromosomesAmount = 1000
  private val chromosomeGenesAmount = amountOfVariables * longBitsAmount

  private def createIncubator() = {
    val rand = new RandomUtilsPerfImpl(chromosomeGenesAmount)
    val fitnessCalculator = new FitnessCalculator {
      override def calcFitness(bits: Array[Boolean]): Double =
        funcToFindGlobalExtremum(decodeBitsToNumbers(bits))

      private def funcToFindGlobalExtremum(numbersList: Array[Long]): Double = numbersList.map(_.toDouble).sum
    }
    new IndividualsIncubator(
      chromosomesAmount, new Breeding(rand), fitnessCalculator
    )
  }

  private val generationsAmount = 100000

  def main(args: Array[String]): Unit = {
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
    while (incubator.getBestIndividual.fitness != finalFitness) {
      incubator.makeNextGeneration()
      //      if (genCount % 100 == 0) printIntermediateResults(genCount)
      genCount += 1
    }
    genCount
  }

  private def printIntermediateResults(data: (IndividualsIncubator, Int)): Unit = {
    val (incubator, cnt) = data
    println("===================================================")
    println(s"min=\n${incubator.getWorstIndividual}")
    println(s"max=\n${incubator.getBestIndividual}\n")
    val worst = decodeBitsToNumbers(incubator.getWorstIndividual.chromosome.decodeGenotype).mkString(", ")
    println(s"$worst")
    val best = decodeBitsToNumbers(incubator.getBestIndividual.chromosome.decodeGenotype).mkString(", ")
    println(s"$best\n\n\n")
    println(s"genNum=$cnt")
  }
}
