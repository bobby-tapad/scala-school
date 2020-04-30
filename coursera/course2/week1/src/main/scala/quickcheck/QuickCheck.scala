package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    j <- oneOf(const(empty), genHeap)
  } yield insert(i, j)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def single(x: H): H = x

  // for any heap, adding the minimal element, and then finding it, should return the element in question:

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum
  // of the resulting heap should get the smallest of the two elements back.

  property("gen2") = forAll { (first: Int, second: Int) =>
    val heap = insert(second, insert(first, empty))
    val min = if (first < second) first else second
    findMin(heap) == min
  }

  // If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.

  property("gen3") = forAll { e: A =>
    deleteMin(insert(e, empty)) == empty
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)

  property("gen4") = forAll { h: H =>
    def helper(min: A, h: H): Boolean = {
      if (isEmpty(h)) true
      else if (min > findMin(h)) false
      else helper(findMin(h), deleteMin(h))
    }
    helper(findMin(h), deleteMin(h))
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.

  property("gen5") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }

  //bogus3 has a broken meld function, so ill write shit with meld
  property("gen6") = forAll { a: A =>
    val h = insert(a, empty)
    isEmpty(deleteMin(meld(h, empty)))
  }

  property("gen8") = forAll { (h1: H, h2: H) =>
    def isEqual(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else if (findMin(h1) != findMin(h2)) false
      else isEqual(deleteMin(h1), deleteMin(h2))
    }

    def melder(h1: H, h2: H): H = {
      if (isEmpty(h1)) h2
      else melder(deleteMin(h1), insert(findMin(h1), h2))
    }

    isEqual(meld(h1, h2), melder(h2, h1))
  }
}