package chapter2

sealed trait List[+A]
case object Nil extends List[Nothing] {
  override def toString: String = "Nil"
}
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = {
    val start = "{ "
    val end = "}"
    start + head + " , " + tail.toString + end
  }
}

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1,2,3)
  val total = sum(example)

  /*
  * EXERCISE 2: Implement the function tail for "removing" the first element of a List. Notice the function takes constant time.
  * */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head,tail) => tail
  }

  /*
  * EXERCISE 3: Generalize tail to the function drop, which removes the first n elements from a list.
  * */
  def drop[A](l: List[A], numElementsToDrop: Int): List[A] = {
    require(numElementsToDrop > 0, "Must provide an input greater than zero !!!")
    def loop[A](l: List[A], acc: Int): List[A] = {
      if(acc == numElementsToDrop) l
      else loop(tail(l),acc + 1)
    }
    loop(l,0)
  }

  /*
  * Implement dropWhile,10 which removes elements from the List prefix as long as they match a predicate.
  * Again, notice these functions take time proportional only to the number of elements being dropped—
  * we do not need to make a copy of the entire List.
  * */

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head,tail) =>
        if(f(head)) dropWhile(tail)(f)
        else l
    }
  }

  /*
  * EXERCISE 5: Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
  * */
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(head,tail) => Cons(a,tail)
  }

  /*
  * EXERCISE 6: Not everything works out so nicely.
  * Implement a function, init, which returns a List consisting of all but the last element
  * of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can't this
  * function be implemented in constant time like tail ?
  * */
  def init[A](l: List[A]): List[A] = {
      require(l != Nil,"List Must not be empty !!!")
    l match {
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

}

object Main extends App {
  import List._

  val testList = List(1,2,3,4,5)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)
  println(tail(testList))
  println(drop(testList, 3))
  println(dropWhile[Int](testList)( _ < 5))
  println(setHead(testList, 6))
  println(init(testList))
}