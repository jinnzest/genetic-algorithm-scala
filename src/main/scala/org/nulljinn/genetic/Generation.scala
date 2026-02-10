package org.nulljinn.genetic

case class Generation(individuals: Array[Individual], canBreed: (Double, Double, Double) => Boolean):

  private var pos = 0

  private val minFitness: Double = findWorstIndividual().fitness

  private val maxFitness: Double = findBestIndividual().fitness

  def selectParentPairs(): Array[(Individual, Individual)] =
    val parents: Array[Option[(Individual, Individual)]] = Array.fill(individuals.length)(None)
    var pairPos = 0
    while pairPos < individuals.length do
      val firstParentPos = findParentPos(None)
      val secondParentPos = findParentPos(Some(firstParentPos))
      parents(pairPos) = Some((individuals(firstParentPos), individuals(secondParentPos)))
      pairPos += 1
    parents.map(_.get)

  private def findParentPos(skipPos: Option[Int]): Int = {
    while true do
      val candidate = individuals(pos)
      if canBreed(candidate.fitness, minFitness, maxFitness) then
        return pos
      if skipPos.contains(pos) then pos += 1
      if pos < individuals.length - 1 then pos += 1 else pos = 0
    pos
  }

  def findWorstIndividual(): Individual =
    individuals.foldLeft(individuals.head): (acc, i) =>
      if acc.fitness > i.fitness then i
      else acc
  
  def findBestIndividual(): Individual = individuals.foldLeft(individuals.head): (acc, i) =>
    if acc.fitness < i.fitness then i
    else acc