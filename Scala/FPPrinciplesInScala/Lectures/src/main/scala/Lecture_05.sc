
//------------------------------------------------------------------------------
// List function impelementations
//------------------------------------------------------------------------------

val l = 1 :: 2 :: 3 :: 4 :: Nil
val r = 5 :: 6 :: 7 :: 8 :: Nil

def last[T](xs: List[T]) : T = xs match {
  case Nil => throw new Error("last(Nil)")
  case List(y) => y
  case y :: ys => last(ys)
}

last(l)

def init[T](xs: List[T]) : List[T] = xs match {
  case Nil     => throw new Error("init(Nil)")
  case List(y) => Nil
  case y :: ys => y :: init(ys)
}

init(l)

def concat[T](xs: List[T], ys: List[T]) : List[T] = xs match {
  case Nil => ys
  case z :: zs => z :: concat(zs, ys)
}

concat(l, r)

def reverse[T](xs: List[T]) : List[T] = xs match {
  case Nil => Nil
  case y :: ys => reverse(ys) ::: List(y)
}

reverse(l)

def removeAt[T](n: Int, xs: List[T]) : List[T] = xs match {
  case Nil => Nil
  case y :: ys =>
    if (n == 0) {
      ys
    } else {
      y :: removeAt(n - 1, ys)
    }
}

removeAt(0, l)
removeAt(1, l)
removeAt(10, l)

//------------------------------------------------------------------------------
// List : built-in functions
//------------------------------------------------------------------------------

val xs = 1 :: 2 :: 3 :: 4 :: Nil

xs.isEmpty
xs.head
xs.tail
xs.length
xs.last
xs.init
xs take 2
xs drop 2
xs(2)
xs.reverse
xs indexOf 4
xs contains 4

val zs = 11 :: 22 :: 33 :: 44 :: Nil

xs ++ zs
xs ::: zs