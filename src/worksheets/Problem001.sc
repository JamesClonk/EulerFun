object Problem001 {
    /*
Project Euler - Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
    */

    def solve(n: Int): Int = {
        if (n % 3 == 0 || n % 5 == 0) n + solve(n - 1)
        else if (n > 3) solve(n - 1)
        else 0
    }                                             //> solve: (n: Int)Int

    solve(9)                                      //> res0: Int = 23
    solve(10)                                     //> res1: Int = 33
    solve(999)                                    //> res2: Int = 233168
    solve(1000)                                   //> res3: Int = 234168

    def solveList(xs: List[Int]): Int = {
        if (xs.isEmpty) 0
        else if (xs.head % 3 == 0 || xs.head % 5 == 0) xs.head + solveList(xs.tail)
        else solveList(xs.tail)
    }                                             //> solveList: (xs: List[Int])Int

    solveList((1 to 9).toList)                    //> res4: Int = 23
    solveList((1 to 10).toList)                   //> res5: Int = 33
    solveList((1 to 999).toList)                  //> res6: Int = 233168
    solveList((1 to 1000).toList)                 //> res7: Int = 234168

    def solveFunc(n: Int): Int =
        (1 until n).filter(v => (v % 3 == 0 || v % 5 == 0)).reduceLeft(_ + _)
                                                  //> solveFunc: (n: Int)Int
    solveFunc(10)                                 //> res8: Int = 23
    solveFunc(1000)                               //> res9: Int = 233168
    
    
    // one-liner
    (1 until 1000).filter(n => (n % 3 == 0 || n % 5 == 0)).reduceLeft(_ + _)
                                                  //> res10: Int = 233168
    
    // FINAL SOLUTION
    (1 until 1000).view.filter(n => (n % 3 == 0 || n % 5 == 0)).sum
                                                  //> res11: Int = 233168
}