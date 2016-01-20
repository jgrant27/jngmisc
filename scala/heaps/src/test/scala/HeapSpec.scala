import org.scalatest._

import jngmisc.heaps._


class HeapSpec extends FlatSpec with Matchers {

  "A Heap" should "be correctly structured" in {
    val heap = new BinaryMinHeap[Int]()
    //Vector(3,7,2,9,1,4,10,8,6,5).foreach( num => heap.insert(num) )
    (1 to 10).reverse.foreach( num => heap.insert(num) )
      //(1 to 8).foreach( num => println(heap.nodeLevel(num)) )
      //(1 to 8).foreach( num => println(num + " -> " + heap.parentPos(num)) )

    heap.nodes.foreach(node => println(node.data))

  }

}
