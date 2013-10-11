object Problem3 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(459); 
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
  };System.out.println("""isPrime: (n: Long)Boolean""");$skip(15); val res$0 = 

  isPrime(9);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(14); val res$1 = 
  isPrime(11);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(14); val res$2 = 
  isPrime(13);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(14); val res$3 = 
  isPrime(71);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(14); val res$4 = 
  isPrime(81);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(15); val res$5 = 
  isPrime(997);System.out.println("""res5: Boolean = """ + $show(res$5));$skip(17); val res$6 = 
  isPrime(13195);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(25); val res$7 = 
  isPrime(600851475143L);System.out.println("""res7: Boolean = """ + $show(res$7));$skip(295); 

  def listOfPrimesUntil(n: Long): List[Long] = {
    def findPrime(x: Long, xs: List[Long]): List[Long] = {
      if (x < 1) xs
      else if (isPrime(x)) findPrime(x - 1, xs ++ List(x))
      else findPrime(x - 1, xs)
    }

    findPrime((if (n % 2 == 0) n - 1 else n), List()).reverse
  };System.out.println("""listOfPrimesUntil: (n: Long)List[Long]""");$skip(27); val res$8 = 

  listOfPrimesUntil(100);System.out.println("""res8: List[Long] = """ + $show(res$8));$skip(70); val res$9 = 
  // works this way too..
  (1 to 100).toList.filter(i => isPrime(i));System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(290); 

  def primeFactors(n: Long): List[Long] = {

    def factor(x: Long, xs: List[Long]): List[Long] = {
      if (x == 1) xs.init
      else if (x % xs.last == 0) factor(x / xs.last, xs ++ List(xs.last))
      else factor(x, xs.init ++ List(xs.last + 1))
    }

    factor(n, List(2))
  };System.out.println("""primeFactors: (n: Long)List[Long]""");$skip(22); val res$10 = 

  primeFactors(20L);System.out.println("""res10: List[Long] = """ + $show(res$10));$skip(21); val res$11 = 
  primeFactors(101L);System.out.println("""res11: List[Long] = """ + $show(res$11));$skip(21); val res$12 = 
  primeFactors(200L);System.out.println("""res12: List[Long] = """ + $show(res$12));$skip(21); val res$13 = 
  primeFactors(201L);System.out.println("""res13: List[Long] = """ + $show(res$13));$skip(23); val res$14 = 
  primeFactors(13195L);System.out.println("""res14: List[Long] = """ + $show(res$14));$skip(46); val res$15 = 
  primeFactors(600851475143L).sortWith(_ < _);System.out.println("""res15: List[Long] = """ + $show(res$15));$skip(67); val res$16 = 

  // solution
  primeFactors(600851475143L).sortWith(_ < _).last;System.out.println("""res16: Long = """ + $show(res$16))}
}