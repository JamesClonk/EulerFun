import scala.annotation.tailrec

object Problem014 {
  /*
Project Euler - Problem 14

The following iterative sequence is defined for the set of positive Longegers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*/

  def stepping(n: Long): Long =
    if (n % 2 == 0) n / 2
    else n * 3 + 1                                //> stepping: (n: Long)Long

  def chain(n: Long): List[Long] = {
    @tailrec
    def compute(x: Long, xs: List[Long]): List[Long] = {
      if (x == 1) xs
      else {
        val y = stepping(x)
        compute(y, y :: xs)
      }
    }
    compute(n, List(n)).reverse
  }                                               //> chain: (n: Long)List[Long]

  (1 to 10).map(n => chain(n.toLong))             //> res0: scala.collection.immutable.IndexedSeq[List[Long]] = Vector(List(1), L
                                                  //| ist(2, 1), List(3, 10, 5, 16, 8, 4, 2, 1), List(4, 2, 1), List(5, 16, 8, 4,
                                                  //|  2, 1), List(6, 3, 10, 5, 16, 8, 4, 2, 1), List(7, 22, 11, 34, 17, 52, 26, 
                                                  //| 13, 40, 20, 10, 5, 16, 8, 4, 2, 1), List(8, 4, 2, 1), List(9, 28, 14, 7, 22
                                                  //| , 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1), List(10, 5, 16, 8
                                                  //| , 4, 2, 1))
  // oops, went out of memory
  //(1 to 1000000).map(n => chain(n.toLong))

  // performance improvement: lets not care about the chain itself and just care about its size..
  def chainSize(n: Long): Long = {
    @tailrec
    def compute(x: Long, y: Long): Long = {
      if (x == 1) y
      else compute(stepping(x), y + 1)
    }
    compute(n, 1)
  }                                               //> chainSize: (n: Long)Long

  // whats the max?
  (1L to 1000000L).map(chainSize).max             //> res1: Long = 525

  // memory :-(
  //(500000 to 1000000).map(n => (n, chainSize(n)))

  // I need a way to eliminate calculating any numbers that were already "visited" or I have the rest of the chain already calculated once.
  // non-functional way, just to prove my idea.
  var chainstore = Map[Long, List[Long]]()        //> chainstore  : scala.collection.immutable.Map[Long,List[Long]] = Map()

  def chainWithMemoizationNonFunctional(n: Long): List[Long] = {
    @tailrec
    def computeChain(x: Long, xs: List[Long]): List[Long] = {
      if (x == 1) xs.reverse
      else if (chainstore.contains(x)) xs.reverse ::: chainstore(x)
      else {
        val y = stepping(x)
        computeChain(y, y :: xs)
      }
    }
    val result = n :: computeChain(n, Nil)

    def memoize(xs: List[Long], chains: Map[Long, List[Long]]): Map[Long, List[Long]] = {
      if (!xs.isEmpty) {
        if (!chainstore.contains(xs.head)) {
          memoize(xs.tail, chains + (xs.head -> xs.tail))
        }
      }
      chains
    }
    chainstore = memoize(result, chainstore.toMap)

    result
  }                                               //> chainWithMemoizationNonFunctional: (n: Long)List[Long]

  // 'view' is awesome! lazyness for teh win.. :-P
  //(1L to 1000000L).view.map(n => (n, chainWithMemoizationNonFunctional(n).size)).sortWith(_._2 > _._2).head

  // and now for the purely functional approach..
  def chainWithMemoization(n: Long, mem: Map[Long, List[Long]]): Map[Long, List[Long]] = {
    @tailrec
    def computeChain(x: Long, xs: List[Long]): List[Long] = {
      if (x == 1) xs.reverse
      else if (mem.contains(x)) xs.reverse ::: mem(x).tail
      else {
        val y = stepping(x)
        computeChain(y, y :: xs)
      }
    }

    @tailrec
    def memoize(xs: List[Long], mem: Map[Long, List[Long]]): Map[Long, List[Long]] = {
      if (!xs.isEmpty && !mem.contains(xs.head)) memoize(xs.tail, mem + (xs.head -> xs))
      else mem
    }

    memoize(n :: computeChain(n, Nil), mem)
  }                                               //> chainWithMemoization: (n: Long, mem: Map[Long,List[Long]])Map[Long,List[Lon
                                                  //| g]]

  // good so far, but toList is expensive..
  (1L to 1000L).view.foldLeft[Map[Long, List[Long]]](
    Map(0L -> List(0L)))(
      (mem, n) => chainWithMemoization(n, mem)).map(m => (m._1 -> m._2.size)).toList.sortWith(_._2 > _._2).head
                                                  //> res2: (Long, Int) = (871,179)

  // FINAL SOLUTION
  (1L to 1000000L).view.foldLeft[Map[Long, List[Long]]](
    Map(0L -> List(0L)))((mem, n) => chainWithMemoization(n, mem)).view.map(m => (m._1 -> m._2.size)).reduceLeft((l, r) => if (l._2 > r._2) l else r)
                                                  //> res3: (Long, Int) = (837799,525)

}