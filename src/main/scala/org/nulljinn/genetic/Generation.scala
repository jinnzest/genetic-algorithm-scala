package org.nulljinn.genetic

case class Generation(individuals: Array[Individual], canBreed: (Double, Double, Double) => Boolean):

  private var pos = 0

  val overageFitness: Double = calcOverageFitness()

  private val minFitness: Double = findWorstIndividual().fitness

  private val maxFitness: Double = findBestIndividual().fitness

  def selectParentPairs(): Array[(Individual, Individual)] =
    val parents: Array[Option[(Individual, Individual)]] = Array.fill(individuals.length)(None)
    var pairPos = 0
    while pairPos < individuals.length do {
      val firstParentPos = findParentPos()
      var secondParentPos = findParentPos()
      if firstParentPos == secondParentPos then
        secondParentPos = if pos > 0 then pos - 1 else individuals.length - 1
      parents(pairPos) = Some((individuals(firstParentPos), individuals(secondParentPos)))
      pairPos += 1
    }
    parents.map(_.get)

  private def findParentPos(): Int =
    var foundParentPos = -1
    while foundParentPos == -1 do
      val candidate = individuals(pos)
      if canBreed(candidate.fitness, minFitness, maxFitness) then
        foundParentPos = pos
      if pos < individuals.length - 1 then pos += 1 else pos = 0
    foundParentPos

  def findWorstIndividual(): Individual =
    def filterOutUnviableFetus(i: Individual): Boolean = (if i.fitness == 0 then 0 else overageFitness / i.fitness) < 20.0

    individuals.foldLeft(individuals.head) { (acc, i) =>
      if acc.fitness > i.fitness && filterOutUnviableFetus(i) then i
      else acc
    }

  def findBestIndividual(): Individual = individuals.foldLeft(individuals.head) { (acc, i) =>
    if acc.fitness < i.fitness then i
    else acc
  }

  private def calcOverageFitness(): Double =
    val fitnessSum = individuals.map(_.fitness).sum
    fitnessSum / individuals.length
