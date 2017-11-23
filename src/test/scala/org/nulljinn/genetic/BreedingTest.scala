package org.nulljinn.genetic

import org.scalamock.scalatest.MockFactory

class BreedingTest extends TestsBase with MockFactory {
  "Breeding" when {
    "canBreed" should {
      "forward calling canBreed to rand.selectIndividualProbability" in forAll { (breed: Boolean, f: Double, min: Double, max: Double) =>
        val randMock = stub[RandomUtils]
        (randMock.selectIndividualProbability _).when(*).returns(breed)
        val result = new Breeding(randMock).canBreed(f, min, max)
        result mustBe breed
      }
    }
    "conception" should {
      "mutate if doMutation returns true" in {
        val randMock = stub[RandomUtils]
        (randMock.shouldMutate _).when.returns(true).once
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        (chr1.crossChromosomes _).when(*, *, *).returns(chr1)
        (chr1.mutate _).when(*, *).noMoreThanOnce
        new Breeding(randMock).conception(chr1, chr2)
      }
      "not mutate if doMutation returns false" in {
        val randMock = stub[RandomUtils]
        (randMock.shouldMutate _).when.returns(false).once
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        (chr1.crossChromosomes _).when(*, *, *).returns(chr1)
        (chr1.mutate _).when(*, *).never
        new Breeding(randMock).conception(chr1, chr2)
      }
      "cross zygotes if doCrossZygotes returns true" in {
        val randMock = stub[RandomUtils]
        (randMock.shouldCrossZygotes _).when.returns(true).once
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        (chr1.crossChromosomes _).when(*, *, *).returns(chr1)
        (chr1.crossZygotes _).when(*, *).noMoreThanOnce
        new Breeding(randMock).conception(chr1, chr2)
      }
      "not cross zygotes if doMutation returns false" in {
        val randMock = stub[RandomUtils]
        (randMock.shouldCrossZygotes _).when.returns(false).once
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        (chr1.crossChromosomes _).when(*, *, *).returns(chr1)
        (chr1.crossZygotes _).when(*, *).never
        new Breeding(randMock).conception(chr1, chr2)
      }
      "cross chromosome in positions defined by rand" in forAll { (begin: Int, end: Int) =>
        val randMock = stub[RandomUtils]
        inSequence {
          (randMock.crossingChromosomePos _).when.returns(begin).once
          (randMock.crossingChromosomePos _).when.returns(end).once
        }
        val chr1 = mock[Chromosome]
        val chr2 = mock[Chromosome]
        (chr1.crossChromosomes _).expects(*, begin, end)
        new Breeding(randMock).conception(chr1, chr2)
      }
      "mutate chromosome in position defined by rand" in forAll { (pos: Int) =>
        val randMock = stub[RandomUtils]
        (randMock.mutationPos _).when.returns(pos).once
        (randMock.shouldMutate _).when.returns(true).once
        val chr1 = stub[Chromosome]
        val chr2 = mock[Chromosome]
        (chr1.crossChromosomes _).when(*, *, *).returns(chr2)
        (chr2.mutate _).expects(pos, *).once
        new Breeding(randMock).conception(chr1, chr2)
      }
      "cross zygotes of chromosome in positions defined by rand" in forAll { (begin: Int, amount: Int) =>
        val randMock = stub[RandomUtils]
        (randMock.shouldMutate _).when.returns(false).once
        inSequence {
          (randMock.crossingZygotePos _).when.returns(begin).once
          (randMock.crossingZygotePos _).when.returns(amount).once
        }
        (randMock.shouldCrossZygotes _).when.returns(true).once
        val chr1 = stub[Chromosome]
        val chr2 = mock[Chromosome]
        (chr1.crossChromosomes _).when(*, *, *).returns(chr2)
        (chr2.crossZygotes _).expects(begin, amount + 1).once
        new Breeding(randMock).conception(chr1, chr2)
      }
    }
  }
}
