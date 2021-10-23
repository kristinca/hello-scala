package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*

import scala.collection.View.Empty

trait HeapProperties(val heapInterface: HeapInterface):

  // Import all the operations of the `HeapInterface` (e.g., `empty`
  // `insert`, etc.)
  import heapInterface.*

  val minOfTwo: (String, Prop) =
    "the minimum of a heap of two elements should be the smallest of the two elements" ->
    forAll { (x1: Int, x2: Int) =>
      val heap = insert(x2, insert(x1, empty))
      val min = if x1 <= x2 then x1 else x2
      findMin(heap) == min
    }

  val deleteMinOfOne: (String, Prop) =
    "delete minumum of heap of one element should return an empty heap" ->
    forAll { (x: Int) =>
      // create a heap with exactly one element, `x`
      val heap1: List[Node] = insert(x, empty)
      // delete the minimal element from it
      val heap0: List[Node] = deleteMin(heap1)
      // check that heap0 is empty
      isEmpty(heap0)
    }

  val insertMinAndGetMin: (String, Prop) =
    "inserting the minimal element and then finding it should return the same minimal element" ->
    forAll(generatedHeap.suchThat(heap => !isEmpty(heap))) { (heap: List[Node]) =>
      // find the miniminal element of the heap
      // (you don’t need to handle the case of empty heaps because it has been excluded from the heap generator)
      val min: Int = findMin(heap)
      // insert the minimal element to the heap
      val updatedHeap: List[Node] = insert(min, heap)
      // find the minimal element of the updated heap should return the same minimal element
      findMin(updatedHeap) == min
    }

  val deleteAllProducesSortedList: (String, Prop) =
    // recursively traverse the heap
    def check(heap: List[Node]): Boolean =
      val h1: List[Node] = heap
      // if the heap is empty, or if it has just one element, we have
      // successfully finished our checks
      if isEmpty(h1) || isEmpty(deleteMin(h1)) then
        true
      else
        // find the minimal element
        val x1: Int = findMin(h1)
        // delete the minimal element of `heap`
        val heap2: List[Node] = deleteMin(h1)
        // find the minimal element in `heap2`
        val x2: Int = findMin(heap2)
        // check that the deleted element is smaller than the minimal element
        // of the remaining heap, and that the remaining heap verifies the
        // same property (by recursively calling `check`
        val checked: Boolean = (x1 <= x2) && check(heap2)
        checked
    // check arbitrary heaps
    "continually finding and deleting the minimal element of a heap should return a sorted sequence" ->
    forAll { (heap: List[Node]) =>
      check(heap)
    }

  val meldingSmallHeaps: (String, Prop) =
    "melding a heap containing two low values with a heap containing two high values" ->
    forAll { (x: Int, y: Int) =>
      val x1 : Int = x
      val y1: Int = y
      // create two heaps:
      // - the first has two duplicate elements inserted, where both are equal to the
      //   highest value among `x` and `y`
      // - the second also has two duplicate elements insterted, where both are equal
      //   to the lowest value among `x` and `y`
      // finally, meld both heaps.
      val meldedHeap : List[Node] =
        if x1 >= y1 then
          val heap1 : List[Node] = insert(x1, insert(x1, empty))
          val heap2 : List[Node] = insert(y1, insert(y1, empty))
          meld(heap1, heap2)
        else
          val heap1 : List[Node] = insert(y1, insert(y1, empty))
          val heap2 : List[Node] = insert(x1, insert(x1, empty))
          meld(heap1, heap2)
      // check that deleting the minimal element twice in a row from the melded heap,
      // and then finding the minimal element in the resulting heap returns the
      // highest value
      val deleteTwoMinAndFindMin: Boolean =
        if isEmpty(meldedHeap) then
          true
        else if x1 >= y1 then
          val delm: List[Node] = deleteMin(meldedHeap)
          val delm1: List[Node] = deleteMin(delm)
          val dm: Boolean = x1 == findMin(delm1)
          dm
        else
          val delm: List[Node] = deleteMin(meldedHeap)
          val delm1: List[Node] = deleteMin(delm)
          val dm: Boolean = y1 == findMin(delm1)
          dm
      // check that inserting the lowest value to the melded heap, and then
      // finding the minimal element returns the lowest value
      val insertMinAndFindMin: Boolean =
        if x1 <= y1 then
          val inm: List[Node] = insert(x1, meldedHeap)
          val dmm: Boolean = x1 == findMin(inm)
          dmm
        else
          val inm: List[Node] = insert(y1, meldedHeap)
          val dmm: Boolean = y1 == findMin(inm)
          dmm
      // check that both conditions are fulfilled
      deleteTwoMinAndFindMin && insertMinAndFindMin
    }

  // Given two arbitrary heaps, and the heap that results from melding
  // them together, finding the minimum of the melded heap should return
  // the minim  um of the two source heaps. Then, continuously deleting
  // that minimum element (from both the melded heap and the source heap
  // that contained it) should always give back a melded heap whose
  // minimum element is the minimum element of one of the two source
  // heaps, until the two source heaps are empty.
  //
  // Hint 1: write an auxiliary (recursive) method checking that the melded
  // heap is valid with respect to its two source heaps.
  //
  // Hint 2: that auxiliary method should handle four cases:
  //  1. the melded heap is empty (which should happen only if the two source
  //     heaps were empty),
  //  2. the minimum of the melded heap is the minimum of the first source
  //     heap (then, check that after removing the minimum from the melded
  //     heap and from the first source heap, the resulting heaps are still
  //     valid),
  //  3. the minimum of the melded heap is the minimum of the second source
  //     heap (then, check that after removing the minimum from the melded
  //     heap and from the second source heap, the resulting heaps are still
  //     valid),
  //  4. all the other cases (which should not happen in correct heap
  //     implementations).
  val meldingHeaps: (String, Prop) =
    def verifier(heap1:List[Node], heap2:List[Node], m: List[Node]): Boolean = {
      val x1 = heap1
      val x2 = heap2
      val x3 = m
      if isEmpty(x1) && isEmpty(x2) then
        true
      else if isEmpty(x1) && !isEmpty(x2) then
        if findMin(x2) == findMin(x3) then
          val x22: List[Node] = deleteMin(x2)
          val x33: List[Node] = deleteMin(x3)
          verifier(x1, x22, x33)
        else
          false
      else if !isEmpty(x1) && isEmpty(x2) then
        if findMin(x1) == findMin(x3) then
          val x11: List[Node] = deleteMin(x1)
          val x33: List[Node] = deleteMin(x3)
          verifier(x11, x2, x33)
        else
          false
      else
        val min1: Int = findMin(x1)
        val min2: Int = findMin(x2)
        val min3: Int = findMin(x3)
        if min3 != min1 && min3 != min2 then
          false
        else if min1 == min3 then
          val x11: List[Node] = deleteMin(x1)
          val x33: List[Node] = deleteMin(x3)
          verifier(x11, x2, x33)
        else if min2 == min3 then
          val x22: List[Node] = deleteMin(x2)
          val x33: List[Node] = deleteMin(x3)
          verifier(x1, x22, x33)
        else
          false
    }




//
//      val x11 = heap1
//      val x22 = heap2
//      if (x11 == Nil && x22 == Nil) then
//        false
//      else
//        val x1: List[Node] = x11
//        val x2: List[Node] = x22
//        val x3: List[Node] = meld(x1, x2)
//        val isE1: Boolean = isEmpty(x1)
//        val isE2: Boolean = isEmpty(x2)
//        val isE3: Boolean = isEmpty(x3)
//        if isE2 && isE1
//          true
//        else if (isE1 && !isE2 && findMin(x2) == findMin(x3)) then
//          deleteMin(x3)
//          deleteMin(x2)
//          verifier(x1, x2)
//        else if (isE2 && !isE1 && findMin(x1) == findMin(x3)) then
//          deleteMin(x1)
//          deleteMin(x3)
//          verifier(x1, x2)
//        else
////        else if (!isE1 && !isE2 && (findMin(x3) == findMin(x1) || findMin(x3) == findMin(x2))) then
//          deleteMin(x1)
//          deleteMin(x2)
//          verifier(x1, x2)
//            NoSuchElementException("min of empty heap")

//      if isEmpty(x3) then
//        val r: Boolean = isEmpty(x1) && isEmpty(x2)
//        r
//      else if (!isEmpty(x1) && (findMin(x1) == findMin(x3))) then
//        deleteMin(x1)
//        deleteMin(x3)
//        verifier(x1, x3)
//      else if (!isEmpty(x2) && (findMin(x2) == findMin(x3))) then
//        deleteMin(x2)
//        deleteMin(x3)
//        verifier(x2, x3)
//      else
//        false
    "finding the minimum of melding any two heaps should return the minimum of one or the other of the source heaps" ->
      forAll { (heap1: List[Node], heap2: List[Node]) =>
        val h1: List[Node] = heap1
        val h2: List[Node] = heap2
        val mheaps: List[Node] = meld(h1, h2)
        verifier(h1, h2, mheaps)
      }

  // Random heap generator (used by Scalacheck)
  given generatedHeap: Gen[List[Node]]
  given Arbitrary[List[Node]] = Arbitrary(generatedHeap)

end HeapProperties
