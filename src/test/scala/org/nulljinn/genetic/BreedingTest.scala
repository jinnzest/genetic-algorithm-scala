package org.nulljinn.genetic

import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class BreedingTest extends AnyWordSpec with MockFactory:
  "Breeding" when :
    "canBreed" should :
      "forward calling canBreed to rand.selectIndividualProbability" in forAll: (breed: Boolean, f: Double, min: Double, max: Double) =>
        val randMock = stub[RandomUtils]
        (x => randMock.selectIndividualProbability(x)).when(*).returns(breed)
        val result = new Breeding(randMock).canBreed(f, min, max)
        assert(result == breed)

    "conception" should :
      "mutate if doMutation returns true" in :
        val randMock = stub[RandomUtils]
        (() => randMock.shouldMutate()).when().returns(true).once()
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        ((x, y, z) => chr1.crossChromosomes(x, y, z)).when(*, *, *).returns(chr1)
        ((x, y) => chr1.mutate(x, y)).when(*, *).atLeastOnce()
        ((x, y) => chr1.mutate(x, y)).when(*, *).noMoreThanOnce()
        new Breeding(randMock).conception(chr1, chr2)
        succeed

      "not mutate if doMutation returns false" in :
        val randMock = stub[RandomUtils]
        (() => randMock.shouldMutate()).when().returns(false).once()
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        ((x, y, z) => chr1.crossChromosomes(x, y, z)).when(*, *, *).returns(chr1)
        ((x, y) => chr1.mutate(x, y)).when(*, *).never()
        new Breeding(randMock).conception(chr1, chr2)
        succeed

      "cross zygotes if doCrossZygotes returns true" in :
        val randMock = stub[RandomUtils]
        (() => randMock.shouldCrossZygotes()).when().returns(true).once()
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        ((x, y, z) => chr1.crossChromosomes(x, y, z)).when(*, *, *).returns(chr1)
        ((x, y) => chr1.crossZygotes(x, y)).when(*, *).noMoreThanOnce()
        new Breeding(randMock).conception(chr1, chr2)
        succeed

      "not cross zygotes if doMutation returns false" in :
        val randMock = stub[RandomUtils]
        (() => randMock.shouldCrossZygotes()).when().returns(false).once()
        val chr1 = stub[Chromosome]
        val chr2 = stub[Chromosome]
        ((x, y, z) => chr1.crossChromosomes(x, y, z)).when(*, *, *).returns(chr1)
        ((x, y) => chr1.crossZygotes(x, y)).when(*, *).never()
        new Breeding(randMock).conception(chr1, chr2)
        succeed

      "cross chromosome in positions defined by rand" in forAll: (begin: Int, end: Int) =>
        val randMock = stub[RandomUtils]
        inSequence:
          (() => randMock.crossingChromosomePos()).when().returns(begin).once()
          (() => randMock.crossingChromosomePos()).when().returns(end).once()

        val chr1 = mock[Chromosome]
        val chr2 = mock[Chromosome]
        ((x, y, z) => chr1.crossChromosomes(x, y, z)).expects(*, begin, end)
        new Breeding(randMock).conception(chr1, chr2)
        succeed

      "mutate chromosome in position defined by rand" in forAll: (pos: Int) =>
        val randMock = stub[RandomUtils]
        (() => randMock.mutationPos()).when().returns(pos).once()
        (() => randMock.shouldMutate()).when().returns(true).once()
        val chr1 = stub[Chromosome]
        val chr2 = mock[Chromosome]
        ((x, y, z) => chr1.crossChromosomes(x, y, z)).when(*, *, *).returns(chr2)
        ((x, y) => chr2.mutate(x, y)).expects(pos, *).once()
        new Breeding(randMock).conception(chr1, chr2)
        succeed

      "cross zygotes of chromosome in positions defined by rand" in forAll: (begin: Int, amount: Int) =>
        val randMock = stub[RandomUtils]
        (() => randMock.shouldMutate()).when().returns(false).once()
        inSequence:
          (() => randMock.crossingZygotePos()).when().returns(begin).once()
          (() => randMock.crossingZygotePos()).when().returns(amount).once()

        (() => randMock.shouldCrossZygotes()).when().returns(true).once()
        val chr1 = stub[Chromosome]
        val chr2 = mock[Chromosome]
        chr1.crossChromosomes.when(*, *, *).returns(chr2)
        chr2.crossZygotes.expects(begin, amount + 1).once()
        new Breeding(randMock).conception(chr1, chr2)
        succeed