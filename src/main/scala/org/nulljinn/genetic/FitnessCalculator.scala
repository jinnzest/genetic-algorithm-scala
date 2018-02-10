package org.nulljinn.genetic

trait FitnessCalculator {
  def calcFitness(pos: Int): Double
}
