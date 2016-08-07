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
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

  /**
  * EXERCISE 2: Implement the function tail for "removing" the first element of a List. Notice the function takes constant time.
  * */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  /**
  * EXERCISE 3: Generalize tail to the function drop, which removes the first n elements from a list.
  * */
  def drop[A](l: List[A], numElementsToDrop: Int): List[A] = {
    require(numElementsToDrop > 0, "Must provide an input greater than zero !!!")
    def loop[A](l: List[A], acc: Int): List[A] = {
      if (acc == numElementsToDrop) l
      else loop(tail(l), acc + 1)
    }
    loop(l, 0)
  }

  /*
  * Implement dropWhile,10 which removes elements from the List prefix as long as they match a predicate.
  * Again, notice these functions take time proportional only to the number of elements being dropped—
  * we do not need to make a copy of the entire List.
  * */

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) dropWhile(tail)(f)
        else l
    }
  }

  /*
  * EXERCISE 5: Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
  * */
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => Cons(a, tail)
  }

  /*
  * EXERCISE 6: Not everything works out so nicely.
  * Implement a function, init, which returns a List consisting of all but the last element
  * of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can't this
  * function be implemented in constant time like tail ?
  * */
  def init[A](l: List[A]): List[A] = {
    require(l != Nil, "List Must not be empty !!!")
    l match {
      case Nil => sys.error("List Must not be empty !!!")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  /*
  * EXERCISE 9: Compute the length of a list using foldRight.
  * */

  def length[A](l: List[A]): Int = {
      foldRight(l,0)((_,n) => n + 1)
  }

  /*
  * EXERCISE 10: write another general list-recursion function, foldLeft that is tail-recursive
  * */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
    }
  }

  /**
  * EXERCISE 11: Write sum, product, and a function to compute the length of a list using foldLeft.
  * */
  def sum3(l: List[Int]) = foldLeft(l,0)(_+_)

  def product3(l: List[Double]) = foldLeft(l,1.0)(_*_)

  def length2[A](l: List[A]): Int = foldLeft(l,0)((n,_) => n + 1)

  /**
  * EXERCISE 12: Write a function that returns the reverse of a list
   * so given List(1,2,3) it returns List(3,2,1). See if you can write it using a fold.
  * */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  /*
  * EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight? How about the other way around?
  * */
//  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l,Nil: List[A])((a,b) => Cons(a,b))

  /**
  * EXERCISE 14: Implement append in terms of either foldLeft or foldRight. (list append list)
  * */
  def append[A](listA: List[A], listB: List[A]): List[A] =
//  foldLeft(reverse(listA),listB)((acc,i) => Cons(i,acc))
  foldRight(listA,listB)((i,acc) => Cons(i,acc))


  /**
  * EXERCISE 15 (hard): Write a function that concatenates a list of lists into a single list.
  * Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
  * */
  def flatten[A](list: List[List[A]]): List[A] = foldLeft(list,Nil: List[A])((acc,i) => append(acc,i))

  /**
    * EXERCISE 16: Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    *
    * EXERCISE 17: Write a function that turns each value in a List[Double] into a String.
    *
    * EXERCISE 18: Write a function map, that generalizes modifying each element in a list while maintaining the structure of the list.
    * */
  def map[A,B](list: List[A])(f: A => B): List[B] = list match {
    case Nil => Nil
    case Cons(h,t) => Cons(f(h),map(t)(f))
  }

  /**
    * EXERCISE 19: Write a function filter that removes elements from a list
    * unless they satisfy a given predicate. Use it to remote all odd numbers from a List[Int].
    * */
  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    foldRight(list,Nil: List[A])((i,acc) => if(f(i)) {Cons(i,acc)} else acc)

  /**
    * EXERCISE 20: Write a function flatMap, that works like map except that
    * the function given will return a list instead of a single result, and
    * that list should be inserted into the final resulting list.
    * */
  def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] =
  flatten(map(list)(f))

  /**
    * EXERCISE 21: Can you use flatMap to implement filter?
    * */
  def filterUsingFlatMap[A](list: List[A])(f: A => Boolean): List[A] = ???

  /**
    * EXERCISE 22: Write a function that accepts two lists and
    * constructs a new list by adding corresponding elements.
    * For example, List(1,2,3) and List(4,5,6) becomes List(5,7,9).
    * */


  /**
    * EXERCISE 23: Generalize the function you just wrote so that it's not specific to integers or addition.
    * */

  /**
    * EXERCISE 24 (hard): As an example, implement hasSubsequence
    * for checking whether a List contains another List as a subsequence.
    * For instance, List(1,2,3,4) would have
    * List(1,2), List(2,3), and List(4) as subsequences, among others.
    * */

}

object Main extends App {

  import List._

  val testList = List(1, 2, 3, 4, 5)
  val testList2 = List(6,7,8,9,10)
  val testListD = List[Double](1, 2, 3, 4, 5)


  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)
  println(tail(testList))
  println(drop(testList, 3))
  println(dropWhile[Int](testList)(_ < 5))
  println(setHead(testList, 6))
  println(init(testList))
  println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  println(length(testList))
  println(foldLeft(testList, 0)(_ + _))


  println(sum3(testList))
  println(product3(testListD))
  println(length2(testList))
  println(reverse(testList))

  println(append(testList,Cons(6,Nil)))
  println(flatten(List(testList, testList2)))
  println(map(testList)((x: Int) => x + 1))
  println(map(testListD)((d: Double) => "string="+d.toString))
  println(filter(testList)((x: Int) => x % 2 == 0))

  println(flatMap(List(1,2,3))(i => List(i,i)))
}