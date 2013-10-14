import scala.annotation.tailrec

object Problem012 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(837); 
  /*
Project Euler - Problem 12

The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

     1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?
   */

  def triangleNumber(n: Int): Int = {
    @tailrec
    def triangulate(n: Int, sum: Int): Int = {
      if (n <= 0) sum
      else triangulate(n - 1, sum + n)
    }
    triangulate(n, 0)
  };System.out.println("""triangleNumber: (n: Int)Int""");$skip(22); val res$0 = 

  triangleNumber(7);System.out.println("""res0: Int = """ + $show(res$0));$skip(220); 

  def listOfDivisors(n: Int): List[Int] = {
    @tailrec
    def divide(d: Int, xs: List[Int]): List[Int] = {
      if (d <= 0) xs
      else divide(d - 1, if (n % d == 0) d :: xs else xs)
    }
    divide(n, Nil)
  };System.out.println("""listOfDivisors: (n: Int)List[Int]""");$skip(23); val res$1 = 

  listOfDivisors(10);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(21); val res$2 = 
  listOfDivisors(15);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(21); val res$3 = 
  listOfDivisors(21);System.out.println("""res3: List[Int] = """ + $show(res$3));$skip(21); val res$4 = 
  listOfDivisors(28);System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(603); 

  // this is bad! I need to change this to a stream somehow.
  // also, the triangleNumber generation is very bad. It starts from scratch for every new number instead of building on from the last one calculated.
  //(1 to 1000).toList.filter(n => listOfDivisors(triangleNumber(n)).size > 50).head
  
  // better, but not a stream..
  def triangles(n: Int): List[Int] = {
    if (n < 2) List(1)
    else {
      @tailrec
      def triangulate(x: Int, xs: List[Int]): List[Int] = {
        if (x > n) xs
        else triangulate(x + 1, x + xs.head :: xs)
      }
      triangulate(2, List(1))
    }
  };System.out.println("""triangles: (n: Int)List[Int]""");$skip(19); val res$5 = 

  triangles(100);System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(309); 
  // let's try a triangle number stream
  def triangleStream: Stream[(Int, Int)] = {
    def triangle(is: Stream[(Int, Int)]): Stream[(Int, Int)] = {
      Stream.cons(is.head, triangle(Stream.cons((is.head._1 + 1, is.head._2 + is.head._1 + 1), is)))
    }
    triangle(Stream.cons((1, 1), Stream.empty))
  };System.out.println("""triangleStream: => Stream[(Int, Int)]""");$skip(28); val res$6 = 

  triangleStream.take(10);System.out.println("""res6: scala.collection.immutable.Stream[(Int, Int)] = """ + $show(res$6));$skip(33); val res$7 = 
  triangleStream.take(10).toList;System.out.println("""res7: List[(Int, Int)] = """ + $show(res$7));$skip(85); val res$8 = 

  triangleStream.take(10).map(t => (t._1, t._2, listOfDivisors(t._2).size)).toList;System.out.println("""res8: List[(Int, Int, Int)] = """ + $show(res$8));$skip(90); val res$9 = 
  triangleStream.map(t => (t._1, t._2, listOfDivisors(t._2).size)).filter(d => d._3 > 50);System.out.println("""res9: scala.collection.immutable.Stream[(Int, Int, Int)] = """ + $show(res$9));$skip(415); 
  
  //triangleStream.map(t => (t._1, t._2, listOfDivisors(t._2).size)).filter(d => d._3 > 500)
  
  // listOfDivisor is too slow. new approach with some math. trickery..
  def divisorCount(n: Int): Int = {
  	@tailrec
  	def divide(d: Int, x: Int): Int = {
  		if (d <= 1) x
  		else divide(d - 1, if (n % d == 0) x + 2 else x)
  	}
  	val sqrt = Math.sqrt(n)
  	divide(sqrt.toInt, if(n % sqrt == 0) 1 else 2)
  };System.out.println("""divisorCount: (n: Int)Int""");$skip(21); val res$10 = 
  
  divisorCount(5);System.out.println("""res10: Int = """ + $show(res$10));$skip(18); val res$11 = 
  divisorCount(8);System.out.println("""res11: Int = """ + $show(res$11));$skip(18); val res$12 = 
  divisorCount(9);System.out.println("""res12: Int = """ + $show(res$12));$skip(19); val res$13 = 
  divisorCount(10);System.out.println("""res13: Int = """ + $show(res$13));$skip(98); val res$14 = 
  
  // works
  triangleStream.map(t => (t._1, t._2, divisorCount(t._2))).filter(d => d._3 > 500);System.out.println("""res14: scala.collection.immutable.Stream[(Int, Int, Int)] = """ + $show(res$14));$skip(91); val res$15 = 
  // simplified..
  // FINAL SOLUTION
  triangleStream.find(t => divisorCount(t._2) > 500);System.out.println("""res15: Option[(Int, Int)] = """ + $show(res$15))}
}
