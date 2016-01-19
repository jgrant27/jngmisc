import org.scalatest._

import jngmisc.trees._


class TreeSpec extends FlatSpec with Matchers {

  "A Binary Tree" should "be correctly structured" in {
    val left = new BinaryNode(666)
    val right = new BinaryNode(777)
    val root = new BinaryNode(555, left, right)

    root.parent should === (null)
    root.data should === (555)
    root.left should === (left)
    root.left.data should === (666)
    root.right should === (right)
    root.right.data should === (777)
  }

  val vals = Vector.range(0,10)
  val tree = BinaryNode.createBalancedBinaryTree(vals)

  "A balanced Binary Tree" should "be balanced" in {
    val isBalanced = BinaryNode.binaryTreeIsBalanced(tree)

    isBalanced should === (true)
  }

  "A Binary Tree" should "be travesable in order" in {
    var res:Vector[Int] = Vector()
    val fun =

    BinaryNode.traverseBinaryTreeInOrder(tree,
      node => res = res :+ node.data)

    res should === (vals)
  }

  "A Binary Tree" should "be bread-first traversable" in {
    var res:Vector[Int] = Vector()

    BinaryNode.traverseBinaryTreeBreadthFirst(tree,
      node => res = res :+ node.data)

    res should === (Vector(5, 2, 8, 1, 4, 7, 9, 0, 3, 6))
  }

  "A Binary Tree's Depth" should "be correct" in {
    val depth = BinaryNode.binaryTreeDepth(tree)

    depth should === (4)
  }

  "An unbalanced Binary Tree" should "NOT be balanced" in {
    val left2 = new BinaryNode(777)
    val left = new BinaryNode(666, left2)
    val root = new BinaryNode(555, left, null)

    val isNotBalanced = !BinaryNode.binaryTreeIsBalanced(root)

    isNotBalanced should === (true)
  }

}
