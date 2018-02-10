package org.nulljinn.genetic

//array segments per chromosome:
//1. dominance of dominant zygote
//2. values of dominant zygote
//3. dominance of recessive zygote
//4. values of recessive zygote
//5. decoded bits

case class Numbers(chromosomesAmount: Int, lineBitsAmount: Int) {
  val numberLinesAmount = (lineBitsAmount >> longBitsShift) + (if ((lineBitsAmount & longBitsMask) > 0) 1 else 0)
  val chromosomeNumbersAmount = numberLinesAmount * 5
  val generationNumbersAmount = chromosomesAmount * chromosomeNumbersAmount
  val genChromosomesAmount = chromosomesAmount * chromosomeNumbersAmount
  val genZygotesAmount = genChromosomesAmount * 2
  val genLinesAmount = genZygotesAmount * 2
  private val recessivePosShift = numberLinesAmount * 2
  private val tmpIntsPosShift = numberLinesAmount * 4

  def dominantZygotePos(pos: Int) = pos

  def dominancePos(pos: Int) = pos

  def valuesPos(pos: Int) = pos + numberLinesAmount

  def tmpNumbrsPos(pos: Int) = pos + tmpIntsPosShift

  def recessiveZygotePos(pos: Int) = pos + recessivePosShift

  val numbers: Array[Long] = Array.fill(generationNumbersAmount * 2)(0)
  var nextFreePos = 0
  var currentGenPos = 0

  def length: Int = numbers.length

  def copy(posLine: Int, posTo: Int): Unit = copy(posLine, posTo, numberLinesAmount)

  def copy(from: Int, to: Int, size: Int): Unit = java.lang.System.arraycopy(numbers, from, numbers, to, size)

  def obtainPos() = synchronized {
    val pos = nextFreePos
    nextFreePos += chromosomeNumbersAmount
    currentGenPos + pos
  }

  def swapGenerations() = {
    if (currentGenPos == 0) currentGenPos = generationNumbersAmount
    else currentGenPos = 0
    nextFreePos = 0
  }

  def apply(p: Int): Long = numbers(p)

  def update(p: Int, i: Long): Unit = numbers(p) = i

  override def toString: String = {
    var p = numbers.length - 1
    var str = ""
    var splitByChromosomePos = 0
    var splitByLinePos = 0
    var divideBy2Pos = 0

    def insertSplitLine =
      0 to numberLinesAmount * longBitsAmount foreach (_ => str += "=")

    while (p >= 0) {
      str += toBinary(numbers(p)) + " "
      p -= 1
      if (splitByChromosomePos == chromosomeNumbersAmount - 1) {
        splitByChromosomePos = 0
        str += "\n\n"
      } else splitByChromosomePos += 1
      if (splitByLinePos == numberLinesAmount - 1) {
        splitByLinePos = 0
        str += "\n"
      } else splitByLinePos += 1
      if (divideBy2Pos == numbers.length / 2 - 1) {
        divideBy2Pos = 0
        str += "\n"
        insertSplitLine
        str += "\n\n\n"
      } else divideBy2Pos += 1
    }
    str
  }
}
