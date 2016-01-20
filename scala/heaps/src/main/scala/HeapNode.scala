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
  def nodeLevel(pos: Int) = log2(pos).floor.toInt + 1
  def parentPos(pos: Int): Int = {
    val lastLevel = nodeLevel(pos)
    val countWithoutCurrLevel =
      math.pow(2, lastLevel - 1).toInt
    val lastRowPos = pos - countWithoutCurrLevel
    var parentPos = log2(lastRowPos).floor.toInt
    parentPos = if (parentPos < 1) 0 else parentPos
    parentPos = math.pow(2, lastLevel - 2).toInt + parentPos
    parentPos
  }

  def insert(data: Int) = {
    nodes = nodes :+ new BinaryHeapNode[Int](data)

    if (nodes.size > 1) {
      var parentInd = parentPos(nodes.size) - 1
      var currInd = nodes.size - 1
      var parent = nodes(parentInd)
      var node = nodes(currInd)

      while (nodes.size > 1 && currInd > 0 && parent.data > node.data) {
        parent = nodes(parentInd)
        node = nodes(currInd)

        println(currInd + " => " + parentInd)
        //println("replacing parent node " + parent.data + " with curr node " + node.data)
        //println(parentInd + " " + currInd)
        nodes = nodes.updated(parentInd, node)
        nodes = nodes.updated(currInd, parent)
        val prevParentInd = parentInd
        parentInd = currInd
        currInd = prevParentInd
      }
    }

  }

  def remove[T](data: T) = {

  }

}


object BinaryMinHeap {


}
