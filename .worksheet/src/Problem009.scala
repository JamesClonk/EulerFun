object Problem009 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(415); 
    /*
Project Euler - Problem 9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
   */

    def isPythagoreanTriplet(a: Int, b: Int, c: Int): Boolean =
        (a < b && b < c && a * a + b * b == c * c);System.out.println("""isPythagoreanTriplet: (a: Int, b: Int, c: Int)Boolean""");$skip(336); 

    def findSolution(x: Int): Int = {
        (1 until x / 2).toList.foreach { a =>
            (a until x / 2).toList.foreach { b =>
                (b until x / 2).toList.foreach { c =>
                	if (isPythagoreanTriplet(a,b,c) && a+b+c == x) return a*b*c
                }
            }
        }
        
        -1
    };System.out.println("""findSolution: (x: Int)Int""");$skip(30); val res$0 = 
    
    findSolution(1+4+9);System.out.println("""res0: Int = """ + $show(res$0));$skip(26); val res$1 = 
    findSolution(9+16+25);System.out.println("""res1: Int = """ + $show(res$1));$skip(45); val res$2 = 
    // FINAL SOLUTION
    findSolution(1000);System.out.println("""res2: Int = """ + $show(res$2))}
    
}
