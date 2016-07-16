def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean =
    as.sliding(2).forall(a => gt(a(0) , a(1)))

isSorted[Int](Array(1,2,4,5),(a,b) => a<b)
isSorted[Int](Array(1,2,6,4,5),(a,b) => a<b)
isSorted[Int](Array(1,2,6,4,5,0),(a,b) => a<b)

////////////////////////////////////////////////
def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    f(a, _:B)

////////////////////////////////////////////////
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
//f.curried
}

////////////////////////////////////////////////
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
//     Function.uncurried(f)
}

////////////////////////////////////////////////
def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a:A => f(g(a))
    // f andThen g
    // g compose f
}