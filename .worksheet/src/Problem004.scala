object Problem004 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(356); 
    /*
Project Euler - Problem 4

A palindromic number reads the same both ways.
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.

Find the largest palindrome made from the product of two 3-digit numbers.
    */

    def isPalindromic(n: Long): Boolean =
        n.toString == n.toString.reverse;System.out.println("""isPalindromic: (n: Long)Boolean""");$skip(33); val res$0 = 
        
    isPalindromic(717);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(22); val res$1 = 
    isPalindromic(99);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(24); val res$2 = 
    isPalindromic(9009);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(24); val res$3 = 
    isPalindromic(9019);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(172); val res$4 = 
    
    
    // FINAL SOLUTION
    (100L to 999L) map { x =>
    	(x to 999L) map(x * _) filter(isPalindromic)
    } filter(!_.isEmpty) map(_.last) sortWith(_ < _) last;System.out.println("""res4: Long = """ + $show(res$4))}
}
