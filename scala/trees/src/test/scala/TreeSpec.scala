import org.scalatest._

import jngmisc.trees._


class RootBinaryNodeSpec extends FlatSpec with Matchers {
  "BinaryNode" should "be valid" in {
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
}
