package jngmisc.trees

class BinaryNode[T]
  (var data: T = null,
    var left: BinaryNode[T] = null,
    var right: BinaryNode[T] = null,
    var parent: BinaryNode[T] = null) {


}

object BinaryNode {

  def createBalancedBinaryTree(items: Vector[Int]): BinaryNode[Int] = {

    if (items != null && items.size < 1) {
      null
    } else {
      val mid = items.size / 2
      val left = createBalancedBinaryTree(items.slice(0, mid))
      val right = createBalancedBinaryTree(items.slice(mid+1, items.size))
      new BinaryNode(items(mid), left, right)
    }

  }

  def traverseBinaryTreeInOrder(
    node: BinaryNode[Int],
    fun: BinaryNode[Int] => Unit): Unit = {

    if (null != node) {
      traverseBinaryTreeInOrder(node.left, fun)
      fun(node)
      traverseBinaryTreeInOrder(node.right, fun)
    }

  }

  def binaryTreeDepth(node: BinaryNode[Int]): Int = {

    if (null != node) {
      val leftDepth = binaryTreeDepth(node.left)
      val rightDepth = binaryTreeDepth(node.right)
      if (leftDepth > rightDepth) {
        leftDepth + 1
      } else {
        rightDepth + 1
      }
    } else {
      0
    }

  }

}