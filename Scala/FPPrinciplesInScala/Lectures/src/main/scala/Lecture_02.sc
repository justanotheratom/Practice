
//------------------------------------------------------------------------------
// Data Structures
//------------------------------------------------------------------------------

class Rational(x: Int, y: Int) {
  require(y > 0, "Denominator must be positive")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) Math.abs(a) else gcd(b, a % b)

  private val g = gcd(x, y)

  val numer: Int = x / g

  val denom: Int = y / g

  override def toString: String = numer + "/" + denom

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational): Rational = this + -that

  def < (that: Rational): Boolean =
    numer * that.denom < denom * that.numer

  def max(that: Rational): Rational =
    if (this < that) that else this
}

val o = new Rational(10)
val a = new Rational(6,9)
val b = new Rational(3,4)
val c = new Rational(4,5)
a - b
a - b - c
a.max(b)

//val s = new Rational(1, 0)
//val s = new Rational(1, -1)

//------------------------------------------------------------------------------
// Finding fixed points
//------------------------------------------------------------------------------

val tolerance = 0.001

def isCloseEnough(x: Double, y: Double): Boolean =
  Math.abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def loop(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(next, guess)) next else loop(next)
  }
  loop(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) =
  (x + f(x)) / 2

// sqrt(x) is fixed point of the function (y => x / y)
def sqrt(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1.0)

sqrt(2)
sqrt(4)


//------------------------------------------------------------------------------
// Currying
//------------------------------------------------------------------------------

def product(f: Int => Int)(a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc else loop(a + 1, acc * f(a))
  loop(a, 1)
}

def factorial(n: Int): Int =
  product(x => x)(1, n)

factorial(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, init: Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc else loop(a + 1, combine(acc, f(a)))
  loop(a, init)
}

def product2(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

def factorial2(n: Int): Int =
  product2(x => x)(1, n)

factorial2(5)

//------------------------------------------------------------------------------
// Higher-Order functions
//------------------------------------------------------------------------------

def sum(f: Int => Int)(a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc else loop(a + 1, acc + f(a))
  loop(a, 0)
}

def sumOfInt(a: Int, b: Int): Int =
  sum(x => x)(a, b)

sumOfInt(5,7)
sumOfInt(7,5)

def sumOfSquares(a: Int, b:Int): Int =
  sum(x => x * x)(a, b)

sumOfSquares(2,4)
sumOfSquares(4,2)

