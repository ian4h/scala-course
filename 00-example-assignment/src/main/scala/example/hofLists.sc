

object hofLists {

//  def squareLists(xs: List[Int]) : List[Int] = xs match {
//    case Nil => xs
//    case xs::xs1 => xs*xs::squareLists(xs1)
//  }

  def squareLists(xs: List[Int]) : List[Int] = {
    xs map (x=>x*x)
  }

  val list = List(1,2,3,4)
  val test = squareLists(list)

  val nums = List(2,-4,5,7,1)

  nums filter (x=> x>0)
  nums filterNot (x=> x>0)
  nums partition (x => x>0)
  nums takeWhile (x => x>0)
  nums dropWhile (x => x>0)
  nums span (x => x>0)

  val listToPack = List("a","a","a","b","c","c","a")

  def pack[T](xs:List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x::xs1 =>
      val(first,rest) = xs span(y=> y==x)
      first::pack(rest)
  }

  def encode[T](xs: List[T]): List[(T,Int)] = {
    val packed = pack(xs)
    packed map(x=> (x.head, x.length))
  }

  pack(listToPack)
  encode(listToPack)
}