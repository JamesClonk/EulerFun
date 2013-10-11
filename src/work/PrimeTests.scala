package work

import scala.collection.mutable.ListBuffer

object PrimeTests {

    def main(args: Array[String]): Unit = {

        test("Method 1", new Method1)
        //test("Method 2", new Method2)
    }

    def test(name: String, clazz: PrimeSearch) = {
        val start = System.currentTimeMillis
        
        println(clazz.listOfPrimes(10001))
        println(clazz.listOfPrimes(1000))
        println(clazz.listOfPrimes(100))
        println(clazz.listOfPrimes(10))
//        println(clazz.listOfPrimes(10000).size)
//        println(clazz.listOfPrimes(1000).size)
//        println(clazz.listOfPrimes(100).size)
//        println(clazz.listOfPrimes(10).size)

        val end = System.currentTimeMillis
        println("Duration for " + name + ": " + (end - start) + "ms")
        println("-----------------------------------------------------")
    }
}

trait PrimeSearch {
    //def listOfPrimesUntil(n: Int): List[Int]
    def listOfPrimes(n: Int): List[Int]
}

class Method1 extends PrimeSearch {
    private def isPrime(n: Int): Boolean = {
        def searchPrime(x: Int, max: Int): Boolean = {
            if (x > max) true
            else if (n % x == 0) false
            else searchPrime(x + 2, max)
        }

        if ((n % 2 == 0 && n != 2) || (n % 5 == 0 && n != 5)) false
        else searchPrime(3, n / 2)
    }

    def listOfPrimes(n: Int): List[Int] = {
        def findPrime(x: Int, xs: List[Int]): List[Int] = {
            if (xs.size >= n) xs
            else if (isPrime(x)) findPrime(x + 2, x :: xs)
            else findPrime(x + 2, xs)
        }

        if (n == 1) List(2)
        else findPrime(3, List(2)).reverse
    }
}

class Method2 extends PrimeSearch {
    private val _primes = new ListBuffer[Int]
    _primes += 2
    _primes += 3
    
    def primes(): List[Int] = _primes.toList

    private def isPrime(n: Int): Boolean = {
        if(_primes.exists(n % _ == 0)) false
        //if(!_primes.forall(n % _ != 0)) false
        else { _primes += n; true }
    }

    def listOfPrimes(n: Int): List[Int] = {
        def findPrime(x: Int): List[Int] = {
            if (_primes.size == n) _primes.toList
            else if (isPrime(x)) findPrime(x + 2)
            else findPrime(x + 2)
        }

        if (_primes.size >= n) _primes.take(n).toList
        else findPrime(_primes.last + 2)
    }
}
//
//    def method4 = {
//        def primeSieve(n: Int) = (2 to n).foldLeft((2 to n).toSet) { (ps, x) =>
//            if (ps(x)) ps -- (x * x to n by x)
//            else ps
//        }
//
//        primeSieve(100).toList.sortWith(_ < _)
//    }
//}