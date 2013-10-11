object Problem010 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(495); 
    /*
Project Euler - Problem 10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
   */

    def isPrime(n: Long): Boolean = {
        def searchPrime(x: Long, max: Long): Boolean = {
            if (x > max) true
            else if (n % x == 0) false
            else searchPrime(x + 2, max)
        }

        if ((n % 2 == 0 && n != 2) || (n % 5 == 0 && n != 5)) false
        else searchPrime(3, n / 2)
    };System.out.println("""isPrime: (n: Long)Boolean""");$skip(315); 

    def listOfPrimesUntil(n: Long): List[Long] = {
        def findPrime(x: Long, xs: List[Long]): List[Long] = {
            if (x < 2) xs
            else if (isPrime(x)) findPrime(x - 1, x :: xs)
            else findPrime(x - 1, xs)
        }

        findPrime((if (n % 2 == 0) n - 1 else n), List())
    };System.out.println("""listOfPrimesUntil: (n: Long)List[Long]""");$skip(29); val res$0 = 

    listOfPrimesUntil(100);System.out.println("""res0: List[Long] = """ + $show(res$0));$skip(45); val res$1 = 
    listOfPrimesUntil(100).reduceLeft(_ + _);System.out.println("""res1: Long = """ + $show(res$1));$skip(46); val res$2 = 
    listOfPrimesUntil(1000).reduceLeft(_ + _);System.out.println("""res2: Long = """ + $show(res$2));$skip(69); val res$3 = 
		// FINAL SOLUTION A
    listOfPrimesUntil(2000000).reduceLeft(_+_);System.out.println("""res3: Long = """ + $show(res$3));$skip(190); val res$4 = 
    // works this way too..
    //(1 to 2000000).toList.filter(i => isPrime(i))

    (2 to 100).toList
        .filter(x => x == 2 || x % 2 != 0)
        .filter(x => x == 3 || x % 3 != 0);System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(187); 
    val primes: Stream[Int] = {
        def sieve(is: Stream[Int]): Stream[Int] =
            Stream.cons(is.head, sieve(is.filter(_ % is.head != 0)))
        sieve(Stream.from(2))
    };System.out.println("""primes  : Stream[Int] = """ + $show(primes ));$skip(29); val res$5 = 

    primes.take(10).toList;System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(81); val res$6 = 
    
    // FINAL SOLUTION B
    primes.takeWhile(_ < 2000000).reduceLeft(_ + _);System.out.println("""res6: Int = """ + $show(res$6));$skip(143); 

    def primeSieve(n: Int) = (2 to n).foldLeft((2 to n).toSet) { (ps, x) =>
        if (ps(x)) ps -- (x * x to n by x)
        else ps
    };System.out.println("""primeSieve: (n: Int)scala.collection.immutable.Set[Int]""");$skip(49); val res$7 = 
    
    primeSieve(100).toList.sortWith(_ < _);System.out.println("""res7: List[Int] = """ + $show(res$7))}
}
