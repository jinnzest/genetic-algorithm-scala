package org.nulljinn.genetic

case class Individual(fitness: Double, chromosome: Chromosome):
  override def toString: String = s"$chromosome\n$fitness\n"
