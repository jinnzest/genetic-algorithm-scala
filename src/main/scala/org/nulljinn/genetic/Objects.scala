package org.nulljinn.genetic

trait Objects[T] {
  private var pos = -1
  protected val tripleLength: Int = length * 3
  private val runUntilLength = tripleLength - 2
  protected val array: Array[T] = newArray()

  def allPools: AllPools

  def length: Int

  def newArray(): Array[T]

  def newObj(): T

  def setAllPools(allPools: AllPools): Unit

  def next(): T = synchronized {
    if (pos > runUntilLength) pos = 0
    else pos += 1
    array(pos)
  }
}
