object Problem010 {
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
    }                                             //> isPrime: (n: Long)Boolean

    def listOfPrimesUntil(n: Long): List[Long] = {
        def findPrime(x: Long, xs: List[Long]): List[Long] = {
            if (x < 2) xs
            else if (isPrime(x)) findPrime(x - 1, x :: xs)
            else findPrime(x - 1, xs)
        }

        findPrime((if (n % 2 == 0) n - 1 else n), List())
    }                                             //> listOfPrimesUntil: (n: Long)List[Long]

    listOfPrimesUntil(100)                        //> res0: List[Long] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 
                                                  //| 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
    listOfPrimesUntil(100).reduceLeft(_ + _)      //> res1: Long = 1060
    listOfPrimesUntil(1000).reduceLeft(_ + _)     //> res2: Long = 76127|
		// FINAL SOLUTION A
    listOfPrimesUntil(2000000).reduceLeft(_+_)
    // works this way too..
    //(1 to 2000000).toList.filter(i => isPrime(i))

    (2 to 100).toList
        .filter(x => x == 2 || x % 2 != 0)
        .filter(x => x == 3 || x % 3 != 0)
    val primes: Stream[Int] = {
        def sieve(is: Stream[Int]): Stream[Int] =
            Stream.cons(is.head, sieve(is.filter(_ % is.head != 0)))
        sieve(Stream.from(2))
    }

    primes.take(10).toList
    
    // FINAL SOLUTION B
    primes.takeWhile(_ < 2000000).reduceLeft(_ + _)

    def primeSieve(n: Int) = (2 to n).foldLeft((2 to n).toSet) { (ps, x) =>
        if (ps(x)) ps -- (x * x to n by x)
        else ps
    }
    
    primeSieve(100).toList.sortWith(_ < _)
}