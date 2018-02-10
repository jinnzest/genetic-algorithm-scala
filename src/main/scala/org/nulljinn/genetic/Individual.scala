package org.nulljinn.genetic

class Individual(var fitness: Double, var chromosome: Chromosome, var allPools: AllPools) {
  override def toString: String = s"$chromosome\n$fitness\n"
}
