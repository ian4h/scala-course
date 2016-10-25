import scala.annotation.tailrec

object session {

  def abs(x: Double) = if(x<0) -x else x
  def sqrtIter(guess: Double, x: Double): Double = {
    println("testing >> " + guess)
    if(isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double) = {
    abs(guess * guess - x) < 0.00000000001
  }

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)
//  sqrt(1.0e50)

  def factorial(n: Int) : Int = {
    def loop(acc: Int, n: Int): Int =
      if(n==0) acc
      else loop(acc*n, n-1)

    loop(1,n)
  }
  factorial(4)
}