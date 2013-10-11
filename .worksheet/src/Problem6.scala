object Problem6 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(538); 
    /*
Project Euler - Problem 6

The sum of the squares of the first ten natural numbers is,
1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
    */

    def square(x: Int): Int = x * x;System.out.println("""square: (x: Int)Int""");$skip(43); 

    def sum(a: Int, b: Int): Int = a + b;System.out.println("""sum: (a: Int, b: Int)Int""");$skip(177); 

    def sumOfSquares(a: Int, b: Int): Int = {
        if (a > b) 0
        //else square(a) + sumOfSquares(a + 1, b)
        else sum(square(a), sumOfSquares(a + 1, b))
    };System.out.println("""sumOfSquares: (a: Int, b: Int)Int""");$skip(26); val res$0 = 

    sumOfSquares(1, 10);System.out.println("""res0: Int = """ + $show(res$0));$skip(25); val res$1 = 
    sumOfSquares(1, 100);System.out.println("""res1: Int = """ + $show(res$1));$skip(149); 

    def sumRange(a: Int, b: Int): Int = {
        if (a > b) 0
        //else a + sumRange(a + 1, b)
        else sum(a, sumRange(a + 1, b))
    };System.out.println("""sumRange: (a: Int, b: Int)Int""");$skip(84); 

    def squareOfSum(a: Int, b: Int): Int = {
        square(sumRange(a, b))
    };System.out.println("""squareOfSum: (a: Int, b: Int)Int""");$skip(25); val res$2 = 

    squareOfSum(1, 10);System.out.println("""res2: Int = """ + $show(res$2));$skip(24); val res$3 = 
    squareOfSum(1, 100);System.out.println("""res3: Int = """ + $show(res$3));$skip(97); 

    def solution(a: Int, b: Int): Int = {
        squareOfSum(a, b) - sumOfSquares(a, b)
    };System.out.println("""solution: (a: Int, b: Int)Int""");$skip(22); val res$4 = 

    solution(1, 10);System.out.println("""res4: Int = """ + $show(res$4));$skip(21); val res$5 = 
    solution(1, 100);System.out.println("""res5: Int = """ + $show(res$5));$skip(303); 

    // ====================================================================================================
    def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
        if (a > b) zero
        else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
    };System.out.println("""mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int)Int""");$skip(37); val res$6 = 
    mapReduce(square, sum, 0)(1, 10);System.out.println("""res6: Int = """ + $show(res$6));$skip(45); val res$7 = 
    square(mapReduce(x => x, sum, 0)(1, 10));System.out.println("""res7: Int = """ + $show(res$7));$skip(132); 

    def solution2(a: Int, b: Int) =
        square(mapReduce(x => x, sum, 0)(a, b)) -
            mapReduce(square, sum, 0)(a, b);System.out.println("""solution2: (a: Int, b: Int)Int""");$skip(23); val res$8 = 

    solution2(1, 10);System.out.println("""res8: Int = """ + $show(res$8));$skip(22); val res$9 = 
    solution2(1, 100);System.out.println("""res9: Int = """ + $show(res$9));$skip(163); val res$10 = 

    // ====================================================================================================
    (1 to 10).toList.reduceLeft((c, e) => c + e * e);System.out.println("""res10: Int = """ + $show(res$10));$skip(47); val res$11 = 
    square((1 to 10).toList.reduceLeft(_ + _));System.out.println("""res11: Int = """ + $show(res$11));$skip(54); val res$12 = 
    (1 to 100).toList.reduceLeft((c, e) => c + e * e);System.out.println("""res12: Int = """ + $show(res$12));$skip(48); val res$13 = 
    square((1 to 100).toList.reduceLeft(_ + _));System.out.println("""res13: Int = """ + $show(res$13));$skip(110); val res$14 = 

    square((1 to 100).toList.reduceLeft(_ + _)) -
        (1 to 100).toList.reduceLeft((c, e) => c + e * e);System.out.println("""res14: Int = """ + $show(res$14));$skip(237); 
 
    // ====================================================================================================
    def solution3(xs: List[Int]): Int =
        square(xs.reduceLeft(_ + _)) -
            xs.reduceLeft((c, e) => c + e * e);System.out.println("""solution3: (xs: List[Int])Int""");$skip(34); val res$15 = 

    solution3((1 to 10).toList);System.out.println("""res15: Int = """ + $show(res$15));$skip(33); val res$16 = 
    solution3((1 to 100).toList);System.out.println("""res16: Int = """ + $show(res$16))}


}