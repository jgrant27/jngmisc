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

  def parentPos(pos: Int): Int = (pos / 2).floor.toInt

  def firstChildPos(pos: Int): Int = (2 * pos).toInt

  def extractMin(): BinaryHeapNode[Int] = {

    if (nodes.size < 1) return null

    val min = nodes.head
    nodes = nodes.updated(0, nodes(nodes.size-1)).dropRight(1)

    if (nodes.size == 1) return min

    var currInd = 0
    var firstChildInd = firstChildPos(currInd+1) - 1

    while(true) {
      if (!nodes.isDefinedAt(currInd)) return min
      if (!nodes.isDefinedAt(firstChildInd)) return min

      val curr: BinaryHeapNode[Int] = nodes(currInd)
      val first: Int = nodes(firstChildInd).data
      val second: Int = if (nodes.isDefinedAt(firstChildInd+1)) nodes(firstChildInd+1).data else first
      val swapInd = if (first <= second) firstChildInd else firstChildInd + 1

      if (curr.data <= nodes(swapInd).data) return min

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
      nodes = nodes.updated(parentInd, curr)
      nodes = nodes.updated(currInd, parent)

      // update
      val prevCurrInd = currInd
      currInd = parentInd
      parentInd = parentPos(currInd + 1) - 1

      if (parentInd < 0) return
      parent = nodes(parentInd)
      curr = nodes(currInd)
    }
  }

}


object BinaryMinHeap {


}
