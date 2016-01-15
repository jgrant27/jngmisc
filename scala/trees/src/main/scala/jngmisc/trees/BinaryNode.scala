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

  def traverseBinaryTreeInOrder(node: BinaryNode[Int]): Unit = {

    if (null != node) {
      traverseBinaryTreeInOrder(node.left)
      println(node.data)
      traverseBinaryTreeInOrder(node.right)
    }

  }

}
