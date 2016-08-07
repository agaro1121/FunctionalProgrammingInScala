package chapter2

/**
  * Created by Hierro on 8/5/16.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
    * EXERCISE 25: Write a function size that counts the number of nodes in a tree.
    **/
  def size[A](tree: Tree[A]): Int = {
    def loop[A](t: Tree[A], acc: Int): Int = t match {
      case Leaf(v) => {
        acc + 1
      }
      case Branch(l, r) => size(l) + size(r)
    }
    loop(tree, 0)
  }

  /**
    * EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
    * (Note: in Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
    **/
  def max(tree: Tree[Int]): Int = {
    def loop(t: Tree[Int], m: Int): Int = t match {
      case Leaf(v) => if(m < v) v else m
      case Branch(l, r) =>
        val ml: Int = max(l)
        val mr: Int = max(r)
        if(ml < mr) mr else ml

    }
    loop(tree, Int.MinValue)
  }

  /**
    * EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    **/
  def depth[A](tree: Tree[A]): Int = {
    def loop[A](t: Tree[A], acc: Int): Int = t match {
      case Leaf(v) => acc
      case Branch(l,r) =>
        val dl = loop(l, acc + 1)
        val dr = loop(r, acc + 1)
        if(dl < dr) dr else dl
    }

    loop(tree,0)
  }

  /**
    * EXERCISE 28: Write a function map, analogous to the method of the same
    * name on List, that modifies each element in a tree with a given function.
    **/
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * EXERCISE 29: Generalize size, maximum, depth, and map, writing a new function fold that
    * abstracts over their similarities. Reimplement them in terms of this more general function.
    * Can you draw an analogy between this fold function and the left and right folds for List?
    **/
  def fold[A](tree: Tree[A], z: A)(f: (A,A) => A): A = ???
}

object TreeMain extends App {
  import Tree._

  val testTree = Branch(Leaf(1), Branch(Leaf(6), Leaf(3)))

  println(size(testTree))
  println(max(testTree))
  println(depth(testTree))
  println(map(testTree)(_ + 1))
  println(fold(testTree,0)(_+_))


}