
//------------------------------------------------------------------------------
// Lists
//------------------------------------------------------------------------------

def isort(xs: List[Int]) : List[Int] = {

  def insert(x: Int, xs: List[Int]) : List[Int] = xs match {
    case Nil => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  xs match {
    case Nil => Nil
    case y :: ys => insert(y, isort(ys))
  }
}

isort(3::2::4::5::Nil)

//------------------------------------------------------------------------------
// Functional decomposition - pattern matching
//------------------------------------------------------------------------------

{
  trait Expr
  case class Number(n: Int)         extends Expr
  case class Var(v: String)         extends Expr
  case class Sum(l: Expr, r: Expr)  extends Expr
  case class Prod(l: Expr, r: Expr) extends Expr

  def eval(e: Expr) : Int = e match {
    case Number(n) => n
    case Var(v) => throw new Error("eval.Var")
    case Sum(l, r) => eval(l) + eval(r)
    case Prod(l, r) => eval(l) * eval(r)
  }

  def show(e: Expr) : String = e match {
    case Number(n) => n.toString
    case Var(v) => v
    case Sum(l, r) => "(" + show(l) + " + " + show(r) + ")"
    case Prod(l, r) => "(" + show(l) + " * " + show(r) + ")"
  }

  val s = Sum(Number(1),Number(2))
  show(s)
  eval(s)

  val p = Prod(Number(1),Number(2))
  show(p)
  eval(p)

  show(Sum(Prod(Number(2),Var("x")),Var("y")))
  show(Prod(Sum(Number(2),Var("x")),Var("y")))
}

//------------------------------------------------------------------------------
// Object-oriented decomposition
//------------------------------------------------------------------------------

{
  trait Expr {
    def isNumber : Boolean
    def isSum : Boolean
    def numValue : Int
    def leftOp : Expr
    def rightOp : Expr
  }

  class Number(n: Int) extends Expr {
    def isNumber = true
    def isSum = false
    def numValue = n
    def leftOp = throw new Error("Number.leftOp")
    def rightOp = throw new Error("Number.rightOp")
  }

  class Sum(l: Expr, r: Expr) extends Expr {
    def isNumber = false
    def isSum = false
    def numValue = throw new Error("Sum.numValue")
    def leftOp = l
    def rightOp = r
  }

  def eval(e: Expr) : Int = {
    if (e.isNumber) e.numValue
    else if (e.isSum) eval (e.leftOp) + eval (e.rightOp)
    else throw new Error ("Unknown expression" + e)
  }
}

//------------------------------------------------------------------------------
// Peano numbers
//------------------------------------------------------------------------------

{
  abstract class Nat {
    def isZero : Boolean
    def predecessor : Nat
    def successor = new Succ(this)
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero = true
    def predecessor = throw new Error("0.predecessor")
    def + (that: Nat) = that
    def - (that: Nat) = if (that.isZero) this else throw new Error("negative number")
  }

  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n
    def +(that: Nat) = new Succ(n + that)
    def -(that: Nat) = if (that.isZero) this else n - that.predecessor
  }
}
