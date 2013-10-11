object Problem003 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(501); 
    /*
Project Euler - Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
    */

    def isPrime(n: Long): Boolean = {
        def searchPrime(x: Long, max: Long): Boolean = {
            if (x > max) true
            else if (n % x == 0) false
            else searchPrime(x + 2, max)
        }

        if ((n % 2 == 0 && n != 2) || (n % 5 == 0 && n != 5)) false
        else searchPrime(3, n / 2)
    };System.out.println("""isPrime: (n: Long)Boolean""");$skip(189); 

    def primes: Stream[Int] = {
        def sieve(is: Stream[Int]): Stream[Int] =
            Stream.cons(is.head, sieve(is.filter(_ % is.head != 0)))
        sieve(Stream.from(2))
    };System.out.println("""primes: => Stream[Int]""");$skip(29); val res$0 = 

    primes.take(10).toList;System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(37); val res$1 = 
    primes.takeWhile(_ < 100).toList;System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(143); 

    def primeSieve(n: Int) = (2 to n).foldLeft((2 to n).toSet) { (ps, x) =>
        if (ps(x)) ps -- (x * x to n by x)
        else ps
    };System.out.println("""primeSieve: (n: Int)scala.collection.immutable.Set[Int]""");$skip(45); val res$2 = 

    primeSieve(100).toList.sortWith(_ < _);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(55); val res$3 = 
    //primeSieve(2000000).toList.last

    isPrime(9);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(16); val res$4 = 
    isPrime(11);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(16); val res$5 = 
    isPrime(13);System.out.println("""res5: Boolean = """ + $show(res$5));$skip(16); val res$6 = 
    isPrime(71);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(16); val res$7 = 
    isPrime(81);System.out.println("""res7: Boolean = """ + $show(res$7));$skip(17); val res$8 = 
    isPrime(997);System.out.println("""res8: Boolean = """ + $show(res$8));$skip(19); val res$9 = 
    isPrime(13195);System.out.println("""res9: Boolean = """ + $show(res$9));$skip(27); val res$10 = 
    isPrime(600851475143L);System.out.println("""res10: Boolean = """ + $show(res$10));$skip(329); 

    def listOfPrimesUntil(n: Long): List[Long] = {
        def findPrime(x: Long, xs: List[Long]): List[Long] = {
            if (x < 1) xs
            else if (isPrime(x)) findPrime(x - 1, xs ++ List(x))
            else findPrime(x - 1, xs)
        }

        findPrime((if (n % 2 == 0) n - 1 else n), List()).reverse
    };System.out.println("""listOfPrimesUntil: (n: Long)List[Long]""");$skip(29); val res$11 = 

    listOfPrimesUntil(100);System.out.println("""res11: List[Long] = """ + $show(res$11));$skip(74); val res$12 = 
    // works this way too..
    (1 to 100).toList.filter(i => isPrime(i));System.out.println("""res12: List[Int] = """ + $show(res$12));$skip(324); 

    def primeFactors(n: Long): List[Long] = {

        def factor(x: Long, xs: List[Long]): List[Long] = {
            if (x == 1) xs.init
            else if (x % xs.last == 0) factor(x / xs.last, xs ++ List(xs.last))
            else factor(x, xs.init ++ List(xs.last + 1))
        }

        factor(n, List(2))
    };System.out.println("""primeFactors: (n: Long)List[Long]""");$skip(24); val res$13 = 

    primeFactors(20L);System.out.println("""res13: List[Long] = """ + $show(res$13));$skip(23); val res$14 = 
    primeFactors(101L);System.out.println("""res14: List[Long] = """ + $show(res$14));$skip(23); val res$15 = 
    primeFactors(200L);System.out.println("""res15: List[Long] = """ + $show(res$15));$skip(23); val res$16 = 
    primeFactors(201L);System.out.println("""res16: List[Long] = """ + $show(res$16));$skip(25); val res$17 = 
    primeFactors(13195L);System.out.println("""res17: List[Long] = """ + $show(res$17));$skip(48); val res$18 = 
    primeFactors(600851475143L).sortWith(_ < _);System.out.println("""res18: List[Long] = """ + $show(res$18));$skip(77); val res$19 = 

    // FINAL SOLUTION
    primeFactors(600851475143L).sortWith(_ < _).last;System.out.println("""res19: Long = """ + $show(res$19))}
}
