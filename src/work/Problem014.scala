package work

import scala.annotation.tailrec

object Problem014 {

  def main(args: Array[String]): Unit = {
    (1L to 1000000L).view.foldLeft[Map[Long, List[Long]]](
      Map(1L -> List(1L)))(
        (mem, n) => chainWithMemoization(n, mem)).map(m => (m._1, m._2.size)).toList.sortWith(_._2 > _._2).take(10).foreach(println)
  }

  def stepping(n: Long): Long =
    if (n % 2 == 0) n / 2
    else n * 3 + 1

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
  }
}