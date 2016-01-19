import org.scalatest._

import jngmisc.trees._


class TreeSpec extends FlatSpec with Matchers {

  "Root BinaryNode" should "be valid" in {
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

  "Balanced Binary Tree" should "be valid" in {
    tree.data should === (5)
    tree.left.data should === (2)
    tree.left.left.data should === (1)
    tree.left.left.left.data should === (0)
    tree.left.right.data should === (4)
    tree.left.right.left.data should === (3)

    tree.right.data should === (8)
    tree.right.left.data should === (7)
    tree.right.right.data should === (9)
  }

  "Traverse Binary Tree" should "be valid" in {
    var res:Vector[Int] = Vector()
    val fun =

    BinaryNode.traverseBinaryTreeInOrder(tree,
      node => res = res :+ node.data)

    res should === (vals)
  }

  "Traverse Binary Tree Breadth First" should "be valid" in {
    var res:Vector[Int] = Vector()

    BinaryNode.traverseBinaryTreeBreadthFirst(tree,
      node => res = res :+ node.data)

    res should === (Vector(5, 2, 8, 1, 4, 7, 9, 0, 3, 6))
  }

  "Binary Tree Depth" should "be valid" in {
    val depth = BinaryNode.binaryTreeDepth(tree)

    depth should === (4)
  }

  "Binary Tree is Balanced" should "be valid" in {
    val isBalanced = BinaryNode.binaryTreeIsBalanced(tree)

    isBalanced should === (true)
  }

  "Binary Tree is NOT Balanced" should "be valid" in {
    val left2 = new BinaryNode(777)
    val left = new BinaryNode(666, left2)
    val root = new BinaryNode(555, left, null)

    val isBalanced = BinaryNode.binaryTreeIsBalanced(root)

    isBalanced should === (false)
  }

}
