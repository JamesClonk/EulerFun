object Problem006 {
    /*
Project Euler - Problem 6

The sum of the squares of the first ten natural numbers is,
1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
    */

    def square(x: Int): Int = x * x               //> square: (x: Int)Int

    def sum(a: Int, b: Int): Int = a + b          //> sum: (a: Int, b: Int)Int

    def sumOfSquares(a: Int, b: Int): Int = {
        if (a > b) 0
        //else square(a) + sumOfSquares(a + 1, b)
        else sum(square(a), sumOfSquares(a + 1, b))
    }                                             //> sumOfSquares: (a: Int, b: Int)Int

    sumOfSquares(1, 10)                           //> res0: Int = 385
    sumOfSquares(1, 100)                          //> res1: Int = 338350

    def sumRange(a: Int, b: Int): Int = {
        if (a > b) 0
        //else a + sumRange(a + 1, b)
        else sum(a, sumRange(a + 1, b))
    }                                             //> sumRange: (a: Int, b: Int)Int

    def squareOfSum(a: Int, b: Int): Int = {
        square(sumRange(a, b))
    }                                             //> squareOfSum: (a: Int, b: Int)Int

    squareOfSum(1, 10)                            //> res2: Int = 3025
    squareOfSum(1, 100)                           //> res3: Int = 25502500

    def solution(a: Int, b: Int): Int = {
        squareOfSum(a, b) - sumOfSquares(a, b)
    }                                             //> solution: (a: Int, b: Int)Int

    solution(1, 10)                               //> res4: Int = 2640
    solution(1, 100)                              //> res5: Int = 25164150

    // ====================================================================================================
    def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
        if (a > b) zero
        else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
    }                                             //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b
                                                  //| : Int)Int
    mapReduce(square, sum, 0)(1, 10)              //> res6: Int = 385
    square(mapReduce(x => x, sum, 0)(1, 10))      //> res7: Int = 3025

    def solution2(a: Int, b: Int) =
        square(mapReduce(x => x, sum, 0)(a, b)) -
            mapReduce(square, sum, 0)(a, b)       //> solution2: (a: Int, b: Int)Int

    solution2(1, 10)                              //> res8: Int = 2640
    solution2(1, 100)                             //> res9: Int = 25164150

    // ====================================================================================================
    (1 to 10).toList.reduceLeft((c, e) => c + e * e)
                                                  //> res10: Int = 385
    square((1 to 10).toList.reduceLeft(_ + _))    //> res11: Int = 3025
    (1 to 100).toList.reduceLeft((c, e) => c + e * e)
                                                  //> res12: Int = 338350
    square((1 to 100).toList.reduceLeft(_ + _))   //> res13: Int = 25502500

    square((1 to 100).toList.reduceLeft(_ + _)) -
        (1 to 100).toList.reduceLeft((c, e) => c + e * e)
                                                  //> res14: Int = 25164150
 
    // ====================================================================================================
    def solution3(xs: List[Int]): Int =
        square(xs.reduceLeft(_ + _)) -
            xs.reduceLeft((c, e) => c + e * e)    //> solution3: (xs: List[Int])Int

    solution3((1 to 10).toList)                   //> res15: Int = 2640
    
    // FINAL SOLUTION
    solution3((1 to 100).toList)                  //> res16: Int = 25164150


}