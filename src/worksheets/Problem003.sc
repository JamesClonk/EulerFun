object Problem003 {
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
    }                                             //> isPrime: (n: Long)Boolean

    def primes: Stream[Int] = {
        def sieve(is: Stream[Int]): Stream[Int] =
            Stream.cons(is.head, sieve(is.filter(_ % is.head != 0)))
        sieve(Stream.from(2))
    }                                             //> primes: => Stream[Int]

    primes.take(10).toList                        //> res0: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
    primes.takeWhile(_ < 100).toList              //> res1: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 4
                                                  //| 7, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

    def primeSieve(n: Int) = (2 to n).foldLeft((2 to n).toSet) { (ps, x) =>
        if (ps(x)) ps -- (x * x to n by x)
        else ps
    }                                             //> primeSieve: (n: Int)scala.collection.immutable.Set[Int]

    primeSieve(100).toList.sortWith(_ < _)        //> res2: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 4
                                                  //| 7, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
    //primeSieve(2000000).toList.last

    isPrime(9)                                    //> res3: Boolean = false
    isPrime(11)                                   //> res4: Boolean = true
    isPrime(13)                                   //> res5: Boolean = true
    isPrime(71)                                   //> res6: Boolean = true
    isPrime(81)                                   //> res7: Boolean = false
    isPrime(997)                                  //> res8: Boolean = true
    isPrime(13195)                                //> res9: Boolean = false
    isPrime(600851475143L)                        //> res10: Boolean = false

    def listOfPrimesUntil(n: Long): List[Long] = {
        def findPrime(x: Long, xs: List[Long]): List[Long] = {
            if (x < 1) xs
            else if (isPrime(x)) findPrime(x - 1, xs ++ List(x))
            else findPrime(x - 1, xs)
        }

        findPrime((if (n % 2 == 0) n - 1 else n), List()).reverse
    }                                             //> listOfPrimesUntil: (n: Long)List[Long]

    listOfPrimesUntil(100)                        //> res11: List[Long] = List(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
                                                  //|  43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
    // works this way too..
    (1 to 100).toList.filter(i => isPrime(i))     //> res12: List[Int] = List(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 
                                                  //| 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

    def primeFactors(n: Long): List[Long] = {

        def factor(x: Long, xs: List[Long]): List[Long] = {
            if (x == 1) xs.init
            else if (x % xs.last == 0) factor(x / xs.last, xs ++ List(xs.last))
            else factor(x, xs.init ++ List(xs.last + 1))
        }

        factor(n, List(2))
    }                                             //> primeFactors: (n: Long)List[Long]

    primeFactors(20L)                             //> res13: List[Long] = List(2, 2, 5)
    primeFactors(101L)                            //> res14: List[Long] = List(101)
    primeFactors(200L)                            //> res15: List[Long] = List(2, 2, 2, 5, 5)
    primeFactors(201L)                            //> res16: List[Long] = List(3, 67)
    primeFactors(13195L)                          //> res17: List[Long] = List(5, 7, 13, 29)
    primeFactors(600851475143L).sortWith(_ < _)   //> res18: List[Long] = List(71, 839, 1471, 6857)

    // FINAL SOLUTION
    primeFactors(600851475143L).sortWith(_ < _).last
                                                  //> res19: Long = 6857
}