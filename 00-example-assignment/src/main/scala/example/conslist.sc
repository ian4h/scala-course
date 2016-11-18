package example
import example._

object session {

  println("test")
  def nth[T](n: Int, xs: List[T]): T =
    if(n==0) xs.head
    else nth(n-1, xs.tail)


  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero = true
    def predecessor = throw new Error("zero.predecessor")
    def + (that: Nat) = that
    def - (that: Nat) = if(that.isZero) this else throw new Error("zero.negate")

  }

  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n
    def  + (that: Nat) = new Succ(n+that)
    def -(that: Nat) = n - that.predecessor
  }

}