package chapter6_purefunctional

/**
  * The key to recovering referential transparency is to make the state updates explicit.
  * Don’t update the state as a side effect, but simply return the new state along with
  * the value that we’re generating. Here’s one possible interface to a random number
  * generator
  */
trait RNG {
  def nextInt: (Int, RNG)
  type Rand[+A] = RNG ⇒ (A, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    //linear congruential generator (http:// mng.bz/r046)
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }


  /*
  * Write a function that uses RNG.nextInt to generate a random integer
  * between 0 and Int.maxValue (inclusive). Make sure to handle the
  * corner case when nextInt returns Int.MinValue, which doesn’t have a
  * non-negative counterpart.
  * */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val result@(v1, r1) = rng.nextInt

    (if (v1 < 0) -(v1 + 1) else v1, r1)
  }

  /*
  * Write a function to generate a Double between 0 and 1, not including 1.
  * Note: You can use Int.MaxValue to obtain the maximum positive integer
  * value, and you can use x.toDouble to convert an x: Int to a Double.
  * */
  def double(rng: RNG): (Double, RNG) = {
    val (v,r) = nonNegativeInt(rng)
    (v.toDouble, r)
  }

  /*
  * Write functions to generate an (Int, Double) pair,
  * a (Double, Int) pair,
  * and a (Double, Double, Double) 3-tuple.
  * You should be able to reuse the functions you’ve already written.
  * */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (v,r) = rng.nextInt
    ((v, v.toDouble), r)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 ⇒ (List(),rng)
      case _ ⇒
        val (x,r1) = rng.nextInt
        val (xs, r2) = ints(count - 1)(r1)
        (x::xs,r2)
    }
  }

  /**************** A BETTER API ******************/
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  //Use map to reimplement double in a more elegant way.
  def doubleUsingMap: Rand[Double] = map(int)(_.toDouble)

  /*
  * Write the implementation of map2 based on the following signature.
  * This function takes two actions, ra and rb, and a function f for
  * combining their results, and returns a new action that combines them
  * */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng) ⇒ {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)


  //Implement sequence for combining a List of transitions into a single transition.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
     fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  //Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng ⇒ {
      val (v,r) = f(rng)
      ???
    }
  }

}

object SimpleRNGTester extends App {
  val rng = SimpleRNG(42)

  val e1@(n1, rng2) = rng.nextInt
  val e2@(n2, rng3) = rng2.nextInt

  println(e1)
  println(e2)

  val is = rng.ints(3)(rng)
  println(is)

  val d = rng.double(rng)
  val dWithMap = rng.doubleUsingMap(rng)
  println(d)
  println(dWithMap)

}