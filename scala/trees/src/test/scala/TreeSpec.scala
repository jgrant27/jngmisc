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

  "Balanced Binary Tree" should "be valid" in {
    val vals = Vector.range(0,10)
    val tree = BinaryNode.createBalancedBinaryTree(vals)

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
    val vals = Vector.range(0,100)
    val tree = BinaryNode.createBalancedBinaryTree(vals)

    var res:Vector[Int] = Vector()
    val fun = (node: BinaryNode[Int]) => {
      res = res :+ node.data
    }
    BinaryNode.traverseBinaryTreeInOrder(tree, fun)

    res should === (vals)
  }

  "Binary Tree Depth" should "be valid" in {
    val vals = Vector.range(0,1024)
    val tree = BinaryNode.createBalancedBinaryTree(vals)

    val depth = BinaryNode.binaryTreeDepth(tree)

    depth should === (11)
  }

}