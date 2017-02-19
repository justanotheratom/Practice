
def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)

factorial(5)

def factorial2(n: Int): Int = {

  def rec(n: Int, acc: Int): Int =
    if (n == 0) acc else rec(n - 1, acc * n)

  rec(n, 1)
}

factorial(5)

//------------------------------------------------------------------------------
// Euclid's algorithm
//------------------------------------------------------------------------------

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(8, 30)


//------------------------------------------------------------------------------
// Newton's method
//------------------------------------------------------------------------------

def sqrt(x: Double) = {

  def isGoodEnough(guess: Double) =
    Math.abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
    (guess + x / guess) / 2

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrt(4)
sqrt(100)
sqrt(1e-6)
sqrt(1e60)

//------------------------------------------------------------------------------

def radius = 10
def pi = 3.14
(2 * pi) * radius

def square(x : Double) = x * x
square(2)
square(5 + 4)

def sumOfSquares(x: Double, y: Double) = square(x) + square(y)
sumOfSquares(2, 3)

def abs(x: Int) = if (x >= 0) x else -x
abs(3)
abs(-3)

//------------------------------------------------------------------------------
