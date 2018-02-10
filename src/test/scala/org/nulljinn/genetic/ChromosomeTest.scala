package org.nulljinn.genetic

import org.scalacheck.{Gen => SCGen}

class ChromosomeTest extends TestsBase {
  "Chromosome" when {
    "decode first zygote with dominant genes" should {
      "always override recessive genes of second  zygote" in {
        val pools = AllPools(1, 1, 4)
        val genotype = Chromosome("DDdd", "RrRr", pools).decodeGenotype
        pools.numbers(genotype) mustBe 0xC
      }
      "always override dominant genes of second zygote" in {
        val pools = AllPools(1, 1, 4)
        pools.numbers(Chromosome("DDdd", "DdDd", pools).decodeGenotype) mustBe 0xC
      }
    }
    "decode first zygote with recessive genes" should {
      "always override recessive genes of second zygote" in {
        val pools = AllPools(1, 1, 4)
        pools.numbers(Chromosome("RRrr", "RrRr", pools).decodeGenotype) mustBe 0xC
      }
      "always be overridden by dominant genes of second zygote" in {
        val pools = AllPools(1, 1, 4)
        pools.numbers(Chromosome("RRrr", "DdDd", pools).decodeGenotype) mustBe 0xA
      }
    }
    "cross zygotes" should {
      "swap 3 genes starting from pos 2" in {
        val pools = AllPools(2, 1, 8)
        val crossed = Chromosome("dddddddd", "rrrrrrrr", pools)
        crossed.crossZygotes(2, 3).toString mustBe Chromosome("dddrrrdd", "rrrdddrr", pools).toString
      }
      "swap whole right part of chromosome if num of genes to move bigger than size of chromosome" in forAll(SCGen.posNum[Int]) { (pos: Int) =>
        (pos: Int) =>
          val pools = AllPools(2, 1, 4)
          val chromosome = Chromosome("ddddd", "rrrrr", pools)
          chromosome.crossZygotes(3, 5 + pos).toString mustBe Chromosome("rrddd", "ddrrr", pools).toString
      }
    }
    "cross chromosomes" should {
      "cross dominant zygote for first parent with dominant zygote of second one and recessive zygote of first one with recessive zygote of second one" in {
        val pools = AllPools(4, 1, 5)
        val firstParent = Chromosome("dddd dddd", "rrrr rrrr", pools)
        val secondParent = Chromosome("DDDD DDDD", "RRRR RRRR", pools)
        firstParent.crossChromosomes(secondParent, 1, 2).toString mustBe Chromosome("dddd dDDd", "rrrr rRRr", pools).toString
      }
      "mutate" should {
        "change gen in defined position of dominant zygote" in {
          val pools = AllPools(2, 1, 5)
          Chromosome("dddd dddd", "rrrr rrrr", pools).mutate(2, Gen.R1).toString mustBe Chromosome("dddd dRdd", "rrrr rrrr", pools).toString
        }
      }
    }
    "toString" should {
      "return strings concatenation of apply method params" in {
        val pools = AllPools(1, 1, 8)
        Chromosome("dDrR rrrr", "RrDd dddd", pools).toString mustBe "dDrR rrrr\nRrDd dddd"
      }
    }
  }
}
