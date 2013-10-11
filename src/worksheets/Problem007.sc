object Problem007 {
  /*
Project Euler - Problem 7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10001st prime number?
    */

  def isPrime(n: Long): Boolean = {
    def searchPrime(x: Long, max: Long): Boolean = {
      if (x > max) true
      else if (n % x == 0) false
      else searchPrime(x + 2, max)
    }

    if ((n % 2 == 0 && n != 2) || (n % 5 == 0 && n != 5)) false
    else searchPrime(3, n / 2)
  }                                               //> isPrime: (n: Long)Boolean

  isPrime(9)                                      //> res0: Boolean = false
  isPrime(11)                                     //> res1: Boolean = true
  isPrime(13)                                     //> res2: Boolean = true
  isPrime(71)                                     //> res3: Boolean = true
  isPrime(81)                                     //> res4: Boolean = false
  isPrime(997)                                    //> res5: Boolean = true
  isPrime(13195)                                  //> res6: Boolean = false

  def getPrimeNr(e: Long): Long = {
    def loop(n: Long, x: Long): Long = {
      if (isPrime(x)) {
        if (n == e) x
        else loop(n + 1, x + 2)
      } else loop(n, x + 2)
    }
    if (e == 1) 2
    else loop(2, 3)
  }                                               //> getPrimeNr: (e: Long)Long

  getPrimeNr(1)                                   //> res7: Long = 2
  getPrimeNr(2)                                   //> res8: Long = 3
  getPrimeNr(3)                                   //> res9: Long = 5
  getPrimeNr(4)                                   //> res10: Long = 7
  getPrimeNr(5)                                   //> res11: Long = 11
  getPrimeNr(6)                                   //> res12: Long = 13
  getPrimeNr(7)                                   //> res13: Long = 17
  getPrimeNr(8)                                   //> res14: Long = 19
  getPrimeNr(9)                                   //> res15: Long = 23
  getPrimeNr(10)                                  //> res16: Long = 29
  getPrimeNr(100)                                 //> res17: Long = 541
  getPrimeNr(1000)                                //> res18: Long = 7919
  // FINAL SOLUTION
  getPrimeNr(10001)                               //> res19: Long = 104743

  def primes: Stream[Int] = {
    def sieve(is: Stream[Int]): Stream[Int] =
      Stream.cons(is.head, sieve(is.filter(_ % is.head != 0)))
    sieve(Stream.from(2))
  }                                               //> primes: => Stream[Int]
  //primes.take(10001).last
}