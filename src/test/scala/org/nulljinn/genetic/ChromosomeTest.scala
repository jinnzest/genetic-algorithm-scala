package org.nulljinn.genetic

import org.scalacheck.{Gen => SCGen}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class ChromosomeTest extends AnyWordSpec {

  def toBoolArray(str: String): Array[Boolean] = str.map {
    case '0' => false
    case '1' => true
  }.toArray.reverse

  "Chromosome" when {
    "decode first zygote with dominant genes" should {
      "always override recessive genes of second  zygote" in {
        assert(Chromosome("DDdd", "RrRr").decodeGenotype(0) == 0xC)
      }
      "always override dominant genes of second  zygote" in {
        assert(Chromosome("DDdd", "DdDd").decodeGenotype(0) == 0xC)
      }
    }
    "decode first zygote with recessive genes" should {
      "always override recessive genes of second zygote" in {
        assert(Chromosome("RRrr", "RrRr").decodeGenotype(0) == 0xC)
      }
      "always be overridden by dominant genes of second zygote" in {
        assert(Chromosome("RRrr", "DdDd").decodeGenotype(0) == 0xA)
      }
    }
    "cross zygotes" should {
      "swap 3 genes starting from pos 2" in {
        assert(Chromosome("dddd dddd", "rrrr rrrr").crossZygotes(2, 3).toString == Chromosome("dddr rrdd", "rrrd ddrr").toString)
      }
      "swap whole right part of chromosome if num of genes to move bigger than size of chromosome" in forAll(SCGen.posNum[Int]) {
        (pos: Int) =>
          assert(Chromosome("dddd dddd", "rrrr rrrr").crossZygotes(3, 5 + pos).toString == Chromosome("rrrr rddd", "dddd drrr").toString)
      }
    }
    "cross chromosomes" should {
      "cross dominant zygote for first parent with dominant zygote of second one and recessive zygote of first one with recessive zygote of second one" in {
        val firstParent = Chromosome("dddd dddd", "rrrr rrrr")
        val secondParent = Chromosome("DDDD DDDD", "RRRR RRRR")
        assert(firstParent.crossChromosomes(secondParent, 1, 2).toString == Chromosome("dddd dDDd", "rrrr rRRr").toString)
      }
    }
    "mutate" should {
      "change gen in defined position of dominant zygote" in {
        assert(Chromosome("dddd dddd", "rrrr rrrr").mutate(2, Gen.R1).toString == Chromosome("dddd dRdd", "rrrr rrrr").toString)
      }
    }
    "toString" should {
      "return strings concatenation of apply method params" in {
        assert(Chromosome("dDrR rrrr", "RrDd dddd").toString == "dDrR rrrr\nRrDd dddd")
      }
    }
  }
}
