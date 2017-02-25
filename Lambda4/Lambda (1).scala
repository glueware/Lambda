object Calculus {
  class Expr
  class Value

  case class Number(i: Int) extends Expr
  case class BinOp(f: (Int, Int) => Int, e1: Expr, e2: Expr) extends Expr
  case class Var(x: String) extends Expr
  case class Apply(f: Expr, e: Expr) extends Expr
  case class Lambda(x: String, e: Expr) extends Expr

  case class VLambda(e: Lambda, env: Env) extends Value
  case class VNumber(i: Int) extends Value

  type Env = String => Value

  def empty(x: String) = throw new Exception("unbound variable " + x)
  def update(env: Env, x: String, v: Value): Env =
    (y => if (y == x) v else env(y))

  def eval(e: Expr, env: Env): Value = e match {

    case Number(i) => VNumber(i)
    case BinOp(f, e1, e2) =>
      val VNumber(i1) = eval(e1, env)
      val VNumber(i2) = eval(e2, env)
      VNumber(f(i1, i2))
    case Var(x) =>
      env(x)
    case Apply(f, g) =>
      val VLambda(Lambda(x, e1), env1) = eval(f, env)
      val v = eval(g, env)
      eval(e1, update(env1, x, v))
    case Lambda(x, e1) =>
      VLambda(Lambda(x, e1), env)
  }
  
  val bad = Apply(Number(3), Number(1))
//  val crash = eval(bad, empty)

  def times(x: Int)(y: Int) = x * y
  def twice(f: Int => Int)(x: Int): Int = f(f(x))
  val test: Int = twice(times(3))(4)
  val test2: Int = (times(3))(4)
}