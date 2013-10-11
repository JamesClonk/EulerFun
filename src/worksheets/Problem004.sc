object Problem004 {
    /*
Project Euler - Problem 4

A palindromic number reads the same both ways.
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.

Find the largest palindrome made from the product of two 3-digit numbers.
    */

    def isPalindromic(n: Long): Boolean =
        n.toString == n.toString.reverse          //> isPalindromic: (n: Long)Boolean
        
    isPalindromic(717)                            //> res0: Boolean = true
    isPalindromic(99)                             //> res1: Boolean = true
    isPalindromic(9009)                           //> res2: Boolean = true
    isPalindromic(9019)                           //> res3: Boolean = false
    
    
    // FINAL SOLUTION
    (100L to 999L) map { x =>
    	(x to 999L) map(x * _) filter(isPalindromic)
    } filter(!_.isEmpty) map(_.last) sortWith(_ < _) last
                                                  //> res4: Long = 906609
}