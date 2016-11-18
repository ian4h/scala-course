package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    * Define a function singletonSet which creates a singleton set
    * from one integer value: the set represents the set of the one
    * given element. Now that we have a way to create singleton sets,
    * we want to define a function that allow us to build bigger
    * sets from smaller ones.
    */
  def singletonSet(elem: Int): Set = (x: Int) => elem == x


  /**
    * Define the functions union,intersect, and diff, which takes two
    * sets, and return, respectively, their union, intersection and
    * differences. diff(s, t) returns a set which contains all the
    * elements of the set s that are not in the set t.
    */

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
    * Returns the subset of `s` for which `p` holds.
    * Define the function filter which selects only the elements
    * of a set that are accepted by a given predicate p. The filtered
    * elements are returned as a new set.
    */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    *
    * Using forall, implement a function exists which tests whether a set
    * contains at least one element for which the given predicate is true.
    * Note that the functions forall and exists behave like the universal
    * and existential quantifiers of first-order logic.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = forall(s, p)

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = ???

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
