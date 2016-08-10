def if2[A](cond: Boolean,
           onTrue: () => A,
           onFalse: () => A): A =
  if (cond) onTrue() else onFalse()

if2( 1 < 2,
  () => println("true"), //unevaluated form of an expression cuz of `() => ...`
  () => println("false") //this is called a `thunk`
)

//by-name params get evaluated in EVERY place the function uses it
def if22[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse
if22(false, sys.error("fail"), 3)

def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0
val x = maybeTwice(true, { println("hi"); 1+41 })

//`lazy` caches the value
def maybeTwice2(b: Boolean, i: => Int) = {
     lazy val j=i
     if (b) j+j else 0
  }
val x2 = maybeTwice2(true, { println("hi"); 1+41 })

