object Problem002 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(493); 
  /*
Project Euler - Problem 2

Each new term in the Fibonacci sequence is generated by adding the previous two terms.
By starting with 1 and 2, the first 10 terms will be:
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million,
find the sum of the even-valued terms.
    */

  def fibonacci(n: Int): Int = n match {
    case 0 | 1 => 1
    case _     => fibonacci(n - 1) + fibonacci(n - 2)
  };System.out.println("""fibonacci: (n: Int)Int""");$skip(18); val res$0 = 

  fibonacci(10);System.out.println("""res0: Int = """ + $show(res$0));$skip(226); 

  def fibonacciListNotExceeding(n: Int): List[Int] = {

    def fibList(x: Int, xs: List[Int]): List[Int] = {
      if (xs.head > n) xs.tail
      else fibList(x + 1, fibonacci(x) :: xs)
    }

    fibList(1, List(0))
  };System.out.println("""fibonacciListNotExceeding: (n: Int)List[Int]""");$skip(39); val res$1 = 

  fibonacciListNotExceeding(4000000);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(56); val res$2 = 
  fibonacciListNotExceeding(4000000).filter(_ % 2 == 0);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(95); val res$3 = 

  // FINAL SOLUTION
  fibonacciListNotExceeding(4000000).filter(_ % 2 == 0).reduceLeft(_ + _);System.out.println("""res3: Int = """ + $show(res$3));$skip(115); 

	// fibonacci stream
  def fibStream(a: Int, b: Int): Stream[Int] = {
    Stream.cons(a, fibStream(b, a + b))
  };System.out.println("""fibStream: (a: Int, b: Int)Stream[Int]""");$skip(36); val res$4 = 
  
  fibStream(0,1).take(10).toList;System.out.println("""res4: List[Int] = """ + $show(res$4))}
}
