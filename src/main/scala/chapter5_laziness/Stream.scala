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
  println(Stream(1, 2, 3).take(2).toList)
}