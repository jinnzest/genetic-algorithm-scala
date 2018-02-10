package org.nulljinn.genetic

class Parents(var firstParent: Individual, var secondParent: Individual):
  override def toString = s"$firstParent\n$secondParent"

case class Generation(individuals: Array[Individual], canBreed: (Double, Double, Double) => Boolean):

  private var pos = 0

  def recalculate(): Unit =
    minFitness = findWorstIndividual().fitness
    maxFitness = findBestIndividual().fitness

  private val parents: Array[Parents] = Array.fill(individuals.length)(new Parents(null, null))

  private var minFitness: Double = findWorstIndividual().fitness

  private var maxFitness: Double = findBestIndividual().fitness

  def selectParentPairs(): Array[Parents] =
    var pairPos = 0
    while pairPos < individuals.length do
      val firstParentPos = findParentPos(None)
      val secondParentPos = findParentPos(Some(firstParentPos))
      parents(pairPos).firstParent = individuals(firstParentPos)
      parents(pairPos).secondParent = individuals(secondParentPos)
      pairPos += 1
    parents

  private def findParentPos(skipPos: Option[Int]): Int =
    while true do
      val candidate = individuals(pos)
      if canBreed(candidate.fitness, minFitness, maxFitness) then
        return pos
      if skipPos.contains(pos) then pos += 1
      if pos < parents.length - 1 then pos += 1 else pos = 0
    pos

  def findWorstIndividual(): Individual =
    individuals.foldLeft(individuals.head): (acc, i) =>
      if acc.fitness > i.fitness then i
      else acc

  def findBestIndividual(): Individual = individuals.foldLeft(individuals.head): (acc, i) =>
    if acc.fitness < i.fitness then i
    else acc