object Problem005 {
    /*
Project Euler - Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
    */

	def divisibleBy(n: Int, xs: List[Int]): Boolean =
		xs.forall(n % _ == 0)             //> divisibleBy: (n: Int, xs: List[Int])Boolean
	
	divisibleBy(100, (1 to 10).toList)        //> res0: Boolean = false
	divisibleBy(2520, (1 to 10).toList)       //> res1: Boolean = true
	
	def findDivisibleBy(xs: List[Int]): Int = {
		def findDivisible(x: Int, step: Int): Int = {
			if (divisibleBy(x,xs)) x
			else findDivisible(x+step, step)
		}
		findDivisible(xs.sortWith(_ < _).last, xs.sortWith(_ < _).last)
	}                                         //> findDivisibleBy: (xs: List[Int])Int
	
	findDivisibleBy((1 to 5).toList)          //> res2: Int = 60
	findDivisibleBy((1 to 10).toList)         //> res3: Int = 2520
	findDivisibleBy((1 to 15).toList)         //> res4: Int = 360360
	
	// FINAL SOLUTION
	findDivisibleBy((1 to 20).toList)         //> res5: Int = 232792560
}