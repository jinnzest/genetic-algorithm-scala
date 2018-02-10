package org.nulljinn.genetic

class Parents(var parentA: Individual, var parentB: Individual) {
  override def toString = s"$parentA\n$parentB"
}

case class Generation(individuals: Array[Individual], canBreed: (Double, Double, Double) => Boolean) {
  def recalculate(): Unit = {
    overageFitness = calcOverageFitness()
    minFitness = findWorstIndividual().fitness
    maxFitness = findBestIndividual().fitness
  }

  private val parents: Array[Parents] = Array.fill(individuals.length)(new Parents(null, null))

  private var pos = 0

  var overageFitness: Double = calcOverageFitness()

  private var minFitness: Double = findWorstIndividual().fitness

  private var maxFitness: Double = findBestIndividual().fitness

  def selectParentPairs(): Array[Parents] = {
    var pairPos = 0
    while (pairPos < individuals.length) {
      val firstParent = findParent(individuals.length)
      var secondParent = findParent(individuals.length)
      if (firstParent.eq(secondParent)) {
        secondParent = if (pos > 0) individuals(pos - 1) else individuals.last
      }
      parents(pairPos).parentA = firstParent
      parents(pairPos).parentB = secondParent
      pairPos += 1
    }
    parents
  }

  private def findParent(indLength: Int) = {
    var foundParent: Individual = null
    while (foundParent == null) {
      val candidate = individuals(pos)
      if (canBreed(candidate.fitness, minFitness, maxFitness)) {
        foundParent = candidate
      }
      if (pos < indLength - 1) pos += 1 else pos = 0
    }
    foundParent
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
