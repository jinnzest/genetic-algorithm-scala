package org.nulljinn.genetic

trait FitnessCalculator {
  def calcFitness(decodedGenotype: Array[Long]): Double
}
