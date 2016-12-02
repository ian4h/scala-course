import com.sun.javafx.image.IntPixelSetter

import scala.annotation.tailrec
import scala.collection.immutable.RedBlack

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

  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 incl 4
  println(t2)

//  val a: Array[NonEmpty] = Array(new NonEmpty(1, new Empty, new Empty))
//  val b: Array[IntSet] = a
//  b(0) = new Empty
//  val s: NonEmpty = a(0)
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if(x< elem) left contains x
    else if(x < elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"
}
