object Problem007 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(486); 
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
  };System.out.println("""isPrime: (n: Long)Boolean""");$skip(15); val res$0 = 

  isPrime(9);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(14); val res$1 = 
  isPrime(11);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(14); val res$2 = 
  isPrime(13);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(14); val res$3 = 
  isPrime(71);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(14); val res$4 = 
  isPrime(81);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(15); val res$5 = 
  isPrime(997);System.out.println("""res5: Boolean = """ + $show(res$5));$skip(17); val res$6 = 
  isPrime(13195);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(233); 

  def getPrimeNr(e: Long): Long = {
    def loop(n: Long, x: Long): Long = {
      if (isPrime(x)) {
        if (n == e) x
        else loop(n + 1, x + 2)
      } else loop(n, x + 2)
    }
    if (e == 1) 2
    else loop(2, 3)
  };System.out.println("""getPrimeNr: (e: Long)Long""");$skip(18); val res$7 = 

  getPrimeNr(1);System.out.println("""res7: Long = """ + $show(res$7));$skip(16); val res$8 = 
  getPrimeNr(2);System.out.println("""res8: Long = """ + $show(res$8));$skip(16); val res$9 = 
  getPrimeNr(3);System.out.println("""res9: Long = """ + $show(res$9));$skip(16); val res$10 = 
  getPrimeNr(4);System.out.println("""res10: Long = """ + $show(res$10));$skip(16); val res$11 = 
  getPrimeNr(5);System.out.println("""res11: Long = """ + $show(res$11));$skip(16); val res$12 = 
  getPrimeNr(6);System.out.println("""res12: Long = """ + $show(res$12));$skip(16); val res$13 = 
  getPrimeNr(7);System.out.println("""res13: Long = """ + $show(res$13));$skip(16); val res$14 = 
  getPrimeNr(8);System.out.println("""res14: Long = """ + $show(res$14));$skip(16); val res$15 = 
  getPrimeNr(9);System.out.println("""res15: Long = """ + $show(res$15));$skip(17); val res$16 = 
  getPrimeNr(10);System.out.println("""res16: Long = """ + $show(res$16));$skip(18); val res$17 = 
  getPrimeNr(100);System.out.println("""res17: Long = """ + $show(res$17));$skip(19); val res$18 = 
  getPrimeNr(1000);System.out.println("""res18: Long = """ + $show(res$18));$skip(40); val res$19 = 
  // FINAL SOLUTION
  getPrimeNr(10001);System.out.println("""res19: Long = """ + $show(res$19));$skip(171); 

  def primes: Stream[Int] = {
    def sieve(is: Stream[Int]): Stream[Int] =
      Stream.cons(is.head, sieve(is.filter(_ % is.head != 0)))
    sieve(Stream.from(2))
  };System.out.println("""primes: => Stream[Int]""")}
  //primes.take(10001).last
}
