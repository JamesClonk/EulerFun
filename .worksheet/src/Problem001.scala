object Problem001 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(395); 
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
    };System.out.println("""solve: (n: Int)Int""");$skip(15); val res$0 = 

    solve(9);System.out.println("""res0: Int = """ + $show(res$0));$skip(14); val res$1 = 
    solve(10);System.out.println("""res1: Int = """ + $show(res$1));$skip(15); val res$2 = 
    solve(999);System.out.println("""res2: Int = """ + $show(res$2));$skip(16); val res$3 = 
    solve(1000);System.out.println("""res3: Int = """ + $show(res$3));$skip(192); 

    def solveList(xs: List[Int]): Int = {
        if (xs.isEmpty) 0
        else if (xs.head % 3 == 0 || xs.head % 5 == 0) xs.head + solveList(xs.tail)
        else solveList(xs.tail)
    };System.out.println("""solveList: (xs: List[Int])Int""");$skip(33); val res$4 = 

    solveList((1 to 9).toList);System.out.println("""res4: Int = """ + $show(res$4));$skip(32); val res$5 = 
    solveList((1 to 10).toList);System.out.println("""res5: Int = """ + $show(res$5));$skip(33); val res$6 = 
    solveList((1 to 999).toList);System.out.println("""res6: Int = """ + $show(res$6));$skip(34); val res$7 = 
    solveList((1 to 1000).toList);System.out.println("""res7: Int = """ + $show(res$7));$skip(113); 

    def solveFunc(n: Int): Int =
        (1 until n).filter(v => (v % 3 == 0 || v % 5 == 0)).reduceLeft(_ + _);System.out.println("""solveFunc: (n: Int)Int""");$skip(18); val res$8 = 
    solveFunc(10);System.out.println("""res8: Int = """ + $show(res$8));$skip(20); val res$9 = 
    solveFunc(1000);System.out.println("""res9: Int = """ + $show(res$9));$skip(128); val res$10 = 
    
    
    // one-liner
    // FINAL SOLUTION
    (1 until 1000).filter(n => (n % 3 == 0 || n % 5 == 0)).reduceLeft(_ + _);System.out.println("""res10: Int = """ + $show(res$10))}
}
