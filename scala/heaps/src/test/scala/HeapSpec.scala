import jngmisc.heaps._

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class HeapSpec extends AnyFlatSpec with should.Matchers {

  "A Heap" should "be correctly structured" in {
    // Insert
    val heap = new BinaryMinHeap[Int]()
    Vector(3,7,2,9,1,4,10,8,6,5).foreach( num => heap.insert(num) )
    //(1 to 5).reverse.foreach( num => heap.insert(num) )
    heap.nodes.foreach(node => print(node.data + " "))
    println("\n")

    // Remove
    heap.nodes.foreach( _ => {
      var min = heap.extractMin()
      val mindata = if (null == min) null else min.data
      println("min: " + mindata)
      heap.nodes.foreach(node => print(node.data + " "))
      println()
    })

  }

}
