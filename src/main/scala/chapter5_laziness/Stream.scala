package chapter5_laziness

/**
  * Created by Hierro on 8/9/16.
  */
sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
    * Write the function take(n) for returning the first n elements of a Stream,
    * and drop(n) for skipping the first n elements of a Stream.
    * */
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
    * Write the function takeWhile for returning all
    * starting elements of a Stream that match the given predicate.
    * */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Stream.empty
    case Cons(h,t) if(p(h())) => cons(h(),t().takeWhile(p))
  }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒B) ⇒ B): B = this match {
    case Cons(h,t) ⇒ f(h(), t().foldRight(z)(f))
    case _ ⇒ z
  }

  /**
    * Here `b` is the unevaluated recursive step that folds
    * the tail of the stream. If `p(a)` returns true,
    * `b` will never be evaluated and the computation terminates early.
    * */
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * Implement forAll, which checks that all elements in the
    * Stream match a given predi- cate. Your implementation
    * should terminate the traversal as soon as it encounters
    * a nonmatching value.
    * */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((i,acc) ⇒ acc && p(i) )

  /**
    * Use foldRight to implement takeWhile.
    * */
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a,acc) ⇒ if(p(a)) cons(a,acc) else acc)

  /**
    * Hard: Implement headOption using foldRight.
    *  */

  /**
    * Implement:
    * map, filter, append, and flatMap using foldRight.
    * The append method should be non-strict in its argument.
    * */

  /**
    * foldLeft in terms of foldRight
    * */
  def foldLeft[B](z: B)(f: (B, A) ⇒ B): B =
    ???


}

case object Empty extends Stream[Nothing]

//due to technical limitations, these must be `thunk`s and not by-name parameters
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd //these lazy vals make this a smart constructor
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  //smart constructor because the return type is Stream[A] instead of Empty.type
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*)) //these will not be evaluated until they are forced


}

object MainStream extends App {
  val testStream: Stream[Int] = Stream(2,4,6,3,4)
  val testStreamEven: Stream[Int] = Stream(2,4,6,4)


  println(Stream(1, 2, 3).take(2).toList)
  println(testStream.forAll(_ % 2 == 0))
  println(testStreamEven.forAll(_ % 2 == 0))
}