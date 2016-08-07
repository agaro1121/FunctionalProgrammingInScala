package chapter4_errors

/**
  * Created by Hierro on 8/7/16.
  */

/**
  * EXERCISE 1: We'll explore when you'd use each of these next.
  * But first, as an exercise, implement all of the above functions on Option.
  * As you implement each function, try to think about what it means and in
  * what situations you'd use it. Here are a few hints:
  *
  * It is fine to use pattern matching, though you should be able to implement all the functions besides map and getOrElse without resorting to pattern matching.
  * For map and flatMap, the type signature should be sufficient to determine the implementation.
  * getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
  * orElse returns the first Option if it is defined, otherwise, returns the second Option.
  **/
trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = map { x => if (f(x)) x else Nothing }

  //flatMap((a: A) => if (f(a)) Some(a) else None)

}

object Option {
  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  /**
    * EXERCISE 3: Write a generic function map2, that combines two Option values
    * using a binary function. If either Option value is None,
    * then the return value is too.
    **/
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  for {o1 <- a; o2 <- b} yield f(o1, o2)

  /**
    * EXERCISE 4: Re-implement bothMatch above in terms of this new function, to the extent possible.
    **/
  //Not sure about this one //TODO
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
  map2(Some(pat1), Some(pat2))(doesMatch).getOrElse(None)

  /**
    * EXERCISE 5: Write a function sequence, that combines a list of Options
    * into one option containing a list of all the Some values in the original list.
    * If the original list contains None even once, the result of the function should be None,
    * otherwise the result should be Some with a list of all the values.
    **/
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight[Option[List[A]]](Some(Nil))((o, acc) => map2(o, acc)(_ :: _))


  /**
    * EXERCISE 6: Implement this function.
    * It is straightforward to do using map and sequence,
    * but try for a more efficient implementation that only looks at the list once.
    * In fact, implement sequence in terms of traverse.
    **/
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight[Option[List[B]]](Some(Nil))((o, acc) => map2(f(o), acc)(_ :: _))

  

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]