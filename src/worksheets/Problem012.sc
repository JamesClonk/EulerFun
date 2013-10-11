import scala.annotation.tailrec

object Problem012 {
  /*
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
  }                                               //> triangleNumber: (n: Int)Int

  triangleNumber(7)                               //> res0: Int = 28

  def listOfDivisors(n: Int): List[Int] = {
    @tailrec
    def divide(d: Int, xs: List[Int]): List[Int] = {
      if (d <= 0) xs
      else divide(d - 1, if (n % d == 0) d :: xs else xs)
    }
    divide(n, Nil)
  }                                               //> listOfDivisors: (n: Int)List[Int]

  listOfDivisors(10)                              //> res1: List[Int] = List(1, 2, 5, 10)
  listOfDivisors(15)                              //> res2: List[Int] = List(1, 3, 5, 15)
  listOfDivisors(21)                              //> res3: List[Int] = List(1, 3, 7, 21)
  listOfDivisors(28)                              //> res4: List[Int] = List(1, 2, 4, 7, 14, 28)

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
  }                                               //> triangles: (n: Int)List[Int]

  triangles(100)                                  //> res5: List[Int] = List(5050, 4950, 4851, 4753, 4656, 4560, 4465, 4371, 4278
                                                  //| , 4186, 4095, 4005, 3916, 3828, 3741, 3655, 3570, 3486, 3403, 3321, 3240, 3
                                                  //| 160, 3081, 3003, 2926, 2850, 2775, 2701, 2628, 2556, 2485, 2415, 2346, 2278
                                                  //| , 2211, 2145, 2080, 2016, 1953, 1891, 1830, 1770, 1711, 1653, 1596, 1540, 1
                                                  //| 485, 1431, 1378, 1326, 1275, 1225, 1176, 1128, 1081, 1035, 990, 946, 903, 8
                                                  //| 61, 820, 780, 741, 703, 666, 630, 595, 561, 528, 496, 465, 435, 406, 378, 3
                                                  //| 51, 325, 300, 276, 253, 231, 210, 190, 171, 153, 136, 120, 105, 91, 78, 66,
                                                  //|  55, 45, 36, 28, 21, 15, 10, 6, 3, 1)
  // let's try a triangle number stream
  def triangleStream: Stream[(Int, Int)] = {
    def triangle(is: Stream[(Int, Int)]): Stream[(Int, Int)] = {
      Stream.cons(is.head, triangle(Stream.cons((is.head._1 + 1, is.head._2 + is.head._1 + 1), is)))
    }
    triangle(Stream.cons((1, 1), Stream.empty))
  }                                               //> triangleStream: => Stream[(Int, Int)]

  triangleStream.take(10)                         //> res6: scala.collection.immutable.Stream[(Int, Int)] = Stream((1,1), ?)
  triangleStream.take(10).toList                  //> res7: List[(Int, Int)] = List((1,1), (2,3), (3,6), (4,10), (5,15), (6,21), 
                                                  //| (7,28), (8,36), (9,45), (10,55))

  triangleStream.take(10).map(t => (t._1, t._2, listOfDivisors(t._2).size)).toList
                                                  //> res8: List[(Int, Int, Int)] = List((1,1,1), (2,3,2), (3,6,4), (4,10,4), (5,
                                                  //| 15,4), (6,21,4), (7,28,6), (8,36,9), (9,45,6), (10,55,4))
  triangleStream.map(t => (t._1, t._2, listOfDivisors(t._2).size)).filter(d => d._3 > 50)
                                                  //> res9: scala.collection.immutable.Stream[(Int, Int, Int)] = Stream((224,2520
                                                  //| 0,90), ?)
  
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
  }                                               //> divisorCount: (n: Int)Int
  
  divisorCount(5)                                 //> res10: Int = 2
  divisorCount(8)                                 //> res11: Int = 4
  divisorCount(9)                                 //> res12: Int = 3
  divisorCount(10)                                //> res13: Int = 4
  
  // works
  triangleStream.map(t => (t._1, t._2, divisorCount(t._2))).filter(d => d._3 > 500)
                                                  //> res14: scala.collection.immutable.Stream[(Int, Int, Int)] = Stream((12375,7
                                                  //| 6576500,576), ?)
  // simplified..
  // FINAL SOLUTION
  triangleStream.find(t => divisorCount(t._2) > 500)
                                                  //> res15: Option[(Int, Int)] = Some((12375,76576500))
}