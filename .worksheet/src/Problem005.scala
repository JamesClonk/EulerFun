object Problem005 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(345); 
    /*
Project Euler - Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
    */

	def divisibleBy(n: Int, xs: List[Int]): Boolean =
		xs.forall(n % _ == 0);System.out.println("""divisibleBy: (n: Int, xs: List[Int])Boolean""");$skip(39); val res$0 = 
	
	divisibleBy(100, (1 to 10).toList);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(37); val res$1 = 
	divisibleBy(2520, (1 to 10).toList);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(233); 
	
	def findDivisibleBy(xs: List[Int]): Int = {
		def findDivisible(x: Int, step: Int): Int = {
			if (divisibleBy(x,xs)) x
			else findDivisible(x+step, step)
		}
		findDivisible(xs.sortWith(_ < _).last, xs.sortWith(_ < _).last)
	};System.out.println("""findDivisibleBy: (xs: List[Int])Int""");$skip(37); val res$2 = 
	
	findDivisibleBy((1 to 5).toList);System.out.println("""res2: Int = """ + $show(res$2));$skip(35); val res$3 = 
	findDivisibleBy((1 to 10).toList);System.out.println("""res3: Int = """ + $show(res$3));$skip(35); val res$4 = 
	findDivisibleBy((1 to 15).toList);System.out.println("""res4: Int = """ + $show(res$4));$skip(57); val res$5 = 
	
	// FINAL SOLUTION
	findDivisibleBy((1 to 20).toList);System.out.println("""res5: Int = """ + $show(res$5))}
}
