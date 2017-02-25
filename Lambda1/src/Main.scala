object Main {
  import Calculus._
  def main(args: Array[String]): Unit = {
    val program = Lambda("x", Var("x"))
    val env: Env = (y => VLambda(Lambda("x", Var("x")), null))
    println(eval(Var("x"), env))

    def plus(x: Int, y: Int) = { x + y }
    val plusE = Lambda("x", Lambda("y", BinOp(plus, Var("x"), Var("y"))))
    val plus3 = Apply(plusE, Number(3))

    println(eval(plus3, empty))
    println(eval(Apply(plus3, Number(1)), empty))
  }
}
