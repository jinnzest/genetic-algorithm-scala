package org.nulljinn.genetic

case class Generation(individuals: Array[Individual], canBreed: (Double, Double, Double) => Boolean) {

  private var pos = 0

  val overageFitness: Double = calcOverageFitness()

  private val minFitness: Double = findWorstIndividual().fitness

  private val maxFitness: Double = findBestIndividual().fitness

  def selectParentPairs(): Array[(Individual, Individual)] = {
    val parents: Array[Option[(Individual, Individual)]] = Array.fill(individuals.length)(None)
    var pairPos = 0
    while (pairPos < individuals.length) {
      val firstParentPos = findParentPos()
      var secondParentPos = findParentPos()
      if (firstParentPos == secondParentPos) {
        secondParentPos = if (pos > 0) pos - 1 else individuals.length - 1
      }
      parents(pairPos) = Some((individuals(firstParentPos), individuals(secondParentPos)))
      pairPos += 1
    }
    parents.map(_.get)
  }

  private def findParentPos() = {
    var foundParentPos = -1
    while (foundParentPos == -1) {
      val candidate = individuals(pos)
      if (canBreed(candidate.fitness, minFitness, maxFitness)) {
        foundParentPos = pos
      }
      if (pos < individuals.length - 1) pos += 1 else pos = 0
    }
    foundParentPos
  }

  def findWorstIndividual(): Individual = {
    def filterOutUnviableFetus(i: Individual): Boolean = (if (i.fitness == 0) 0 else overageFitness / i.fitness) < 20.0

    individuals.foldLeft(individuals.head) { (acc, i) =>
      if (acc.fitness > i.fitness && filterOutUnviableFetus(i)) i
      else acc
    }
  }

  def findBestIndividual(): Individual = individuals.foldLeft(individuals.head) { (acc, i) =>
    if (acc.fitness < i.fitness) i
    else acc
  }

  private def calcOverageFitness(): Double = {
    var p = 0
    var fitnessSum = 0.0
    while (p < individuals.length) {
      fitnessSum += individuals(p).fitness
      p += 1
    }
    fitnessSum / individuals.length
  }

  override def toString = {
    individuals.foldLeft("") { (acc, i) =>
      acc + i + "\n"
    }
  }
}
