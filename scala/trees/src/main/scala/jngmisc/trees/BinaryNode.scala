package jngmisc.trees


import scala.collection.mutable


class BinaryNode[T]
  (var data: T = null,
    var left: BinaryNode[T] = null,
    var right: BinaryNode[T] = null,
    var parent: BinaryNode[T] = null) {


}

object BinaryNode {

  def createBalancedBinaryTree(items: Vector[Int]): BinaryNode[Int] = {

    if (items != null && items.size < 1) return null

    val mid = (items.size / 2).floor.toInt
    val left = createBalancedBinaryTree(items.slice(0, mid))
    val right = createBalancedBinaryTree(items.slice(mid+1, items.size))
    new BinaryNode(items(mid), left, right)

  }

  def traverseBinaryTreeInOrder
    (node: BinaryNode[Int],
      fun: BinaryNode[Int] => Unit): Unit = {

    if (null == node) return

    traverseBinaryTreeInOrder(node.left, fun)
    fun(node)
    traverseBinaryTreeInOrder(node.right, fun)

  }

  def traverseBinaryTreeBreadthFirst
    (node: BinaryNode[Int],
      fun: BinaryNode[Int] => Unit): Unit = {

    val queue: mutable.Queue[BinaryNode[Int]] = mutable.Queue()
    queue += node

    while (queue.nonEmpty) {
      val node = queue.dequeue()
      fun(node)
      if (null != node.left) queue += node.left
      if (null != node.right) queue += node.right
    }

  }

  def binaryTreeDepth(node: BinaryNode[Int]): Int = {

    if (null == node) return 0

    val leftDepth = binaryTreeDepth(node.left)
    val rightDepth = binaryTreeDepth(node.right)
    if (leftDepth > rightDepth) leftDepth + 1 else rightDepth + 1

  }

  def binaryTreeIsBalanced(node: BinaryNode[Int]): Boolean = {

    def binaryTreeDepth2(node: BinaryNode[Int]): Int = {

      if (null == node) return 0

      val leftDepth = binaryTreeDepth2(node.left)
      if (leftDepth == -1) return -1
      val rightDepth = binaryTreeDepth2(node.right)
      if (rightDepth == -1) return -1
      val diff = (leftDepth - rightDepth).abs
      if (diff > 1) return -1
      if (leftDepth > rightDepth) leftDepth + 1 else rightDepth + 1

    }

    (binaryTreeDepth2(node) != -1)

  }

}
