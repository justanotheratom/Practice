
//------------------------------------------------------------------------------
// Lists
//------------------------------------------------------------------------------

trait List[T] {
  def isEmpty : Boolean
  def head : T
  def tail : List[T]
}

class Cons[T](val head : T, val tail : List[T]) extends List[T] {
  override def isEmpty = false
}

class Nil[T] extends List[T] {
  override def isEmpty = true
  override def head = throw new NoSuchElementException("Nil.head")
  override def tail = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](e: T) = new Cons[T](e, new Nil[T])

def nth[T](n: Int, xs: List[T]) : T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException()
  else if (n < 0) throw new IndexOutOfBoundsException()
  else if (n == 0) xs head
  else nth(n - 1, xs tail)

val l = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nth(2, l)
//nth(10, l)
//nth(-1, l)

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

abstract class IntSet {
  def incl(x: Int) : IntSet
  def contains(x: Int) : Boolean
  def union(other: IntSet) : IntSet
}

object Empty extends IntSet {
  def contains(x: Int) : Boolean = false
  def incl(x: Int) : IntSet = new NonEmpty(x, Empty, Empty)
  override def union(other: IntSet) = other
  override def toString: String = "."
}

class NonEmpty(e: Int, left: IntSet, right: IntSet) extends IntSet {

  override def toString: String = "{" + left + e + right + "}"

  def contains(x: Int) : Boolean =
    if (x < e) left contains x
    else if (x > e) right contains x
    else true

  def incl(x: Int) : IntSet =
    if (x < e) new NonEmpty(e, left incl x, right)
    else if (x > e) new NonEmpty(e, left, right incl x)
    else this

  override def union(other: IntSet) : IntSet =
    ((left union right) union other) incl e
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
