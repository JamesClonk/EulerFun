object Problem009 {
    /*
Project Euler - Problem 9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
   */

    def isPythagoreanTriplet(a: Int, b: Int, c: Int): Boolean =
        (a < b && b < c && a * a + b * b == c * c)//> isPythagoreanTriplet: (a: Int, b: Int, c: Int)Boolean

    def findSolution(x: Int): Int = {
        (1 until x / 2).toList.foreach { a =>
            (a until x / 2).toList.foreach { b =>
                (b until x / 2).toList.foreach { c =>
                	if (isPythagoreanTriplet(a,b,c) && a+b+c == x) return a*b*c
                }
            }
        }
        
        -1
    }                                             //> findSolution: (x: Int)Int
    
    findSolution(1+4+9)                           //> res0: Int = -1
    findSolution(9+16+25)                         //> res1: Int = -1
    // FINAL SOLUTION
    findSolution(1000)                            //> res2: Int = 31875000
    
}