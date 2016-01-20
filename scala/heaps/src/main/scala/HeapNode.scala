package jngmisc.heaps


import scala.collection.mutable
import scala.math
import scala.util.control.Breaks._


class BinaryHeapNode[T]
  (var data: T = null) {

}


class BinaryMinHeap[T]
  () {

  var nodes: Vector[BinaryHeapNode[Int]] = Vector()

  def log2(x: Int) = math.log(x) / math.log(2)
  def nodeLevel(pos: Int) = log2(pos).floor.toInt

  def parentPos(pos: Int): Int = {
    val currLevel = nodeLevel(pos)
    val countWithCurrLevel = math.pow(2, currLevel).toInt
    val currRowPos = pos - countWithCurrLevel
    val parentRowPos = log2(currRowPos).toInt
    val parentLevelStartInd = math.pow(2, currLevel - 1).toInt
    val parentPos = parentLevelStartInd + parentRowPos
    if (parentPos < 0) 1 else parentPos
  }

  def firstChildPos(pos: Int): Int = {
    val currLevel = nodeLevel(pos)
    val countWithoutCurrLevel = math.pow(2, currLevel - 1).toInt
    val currRowPos = pos - countWithoutCurrLevel
    var childLevelStartPos = 2 * (currRowPos - 1)
    childLevelStartPos = if (currLevel == 0) childLevelStartPos else childLevelStartPos + 1
    val firstChildPos = childLevelStartPos + math.pow(2, currLevel).toInt
    //println(s"\npos $pos currLevel ${currLevel} countWithoutCurrLevel ${countWithoutCurrLevel} currRowPos ${currRowPos} childLevelStartPos ${childLevelStartPos} firstChildPos ${firstChildPos}")
    firstChildPos
  }

  def extractMin(): BinaryHeapNode[Int] = {

    if (nodes.size < 1) return null

    val min = nodes.head
    nodes = nodes.updated(0, nodes(nodes.size-1)).dropRight(1)

    if (nodes.size == 1) return min

    var currInd = 0
    var firstChildInd = firstChildPos(currInd+1) - 1
    //println(s"(${pos-1}) ${curr.data} children are ($firstChildPos) ${leftChild.data} (${firstChildPos+1}) ${rightChild.data}")

    while(true) {
      if (!nodes.isDefinedAt(currInd)) return min
      if (!nodes.isDefinedAt(firstChildInd)) return min

      val curr: BinaryHeapNode[Int] = nodes(currInd)
      val first: Int = nodes(firstChildInd).data
      val second: Int = if (nodes.isDefinedAt(firstChildInd+1)) nodes(firstChildInd+1).data else first
      val swapInd = if (first <= second) firstChildInd else firstChildInd + 1

      if (curr.data <= nodes(swapInd).data) return min

      //println(s"swapping nodes ${curr.data} <=> ${nodes(swapInd).data}")

      nodes = nodes.updated(currInd, nodes(swapInd))
      nodes = nodes.updated(swapInd, curr)
      currInd = swapInd
      firstChildInd = firstChildPos(currInd+1) - 1
    }

    min
  }

  def insert(data: Int): Unit = {
    nodes = nodes :+ new BinaryHeapNode[Int](data)

    if (nodes.size <= 1) return

    var parentInd = parentPos(nodes.size) - 1
    var currInd = nodes.size - 1
    var parent = nodes(parentInd)
    var curr = nodes(currInd)

    while (parent.data > curr.data) {
      // swap
      //println(s"Swapping parent ${parent.data} ($parentInd) with current ${curr.data} ($currInd)")
      nodes = nodes.updated(parentInd, curr)
      nodes = nodes.updated(currInd, parent)

      // update
      val prevCurrInd = currInd
      currInd = parentInd
      parentInd = parentPos(currInd + 1) - 1

      if (parentInd < 0) return
      parent = nodes(parentInd)
      curr = nodes(currInd)
      //println(s"New parent ${parent.data} ($parentInd) and current ${curr.data} ($currInd)")
    }
  }

}


object BinaryMinHeap {


}
