package chapter4_errors

/**
  * Created by Hierro on 8/7/16.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(t) => Left(t)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(t) => Left(t)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      e1 <- this
      e2 <- b
    } yield f(e1, e2)
}

object Either {
  /**
    * EXERCISE 8: Implement sequence and traverse for Either
    **/

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

case class Person(name: Name, age: Age) {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}

sealed class Name(val value: String)

sealed class Age(val value: Int)

case class employee(name: Name , age: Int, salary: Double)

object Boot extends App {
  val r = for {
    age <- Right(42)
    name <- Left("invalid name")
    salary <- Right(1000000.0)
  } yield employee(name, age, salary)

  println(r)
}