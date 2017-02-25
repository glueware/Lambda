object Main {
  import Calculus._
  def main(args: Array[String]): Unit = {
    val empty = Environment(Map())
    def run(e: Expression) { println(e.evaluate(empty)) }

    def plus(x: Int, y: Int) = { x + y }
    val x = new Variable
    val y = new Variable
    val plusE = Lambda(x, Lambda(y, BinOp(plus, x, y)))
    val plus3 = Apply(plusE, Number(3))
    //    run(plus3)
    run(Apply(plus3, Number(1))) // prints 4

    val idE = Lambda(x, x)
    val sappE = Lambda(x, Apply(x, x))
    val ididE = Apply(sappE, idE)
    val threeE = Apply(ididE, Number(3))
    run(threeE)
    run(Apply(Apply(Lambda(x, x), Lambda(y, y)), Number(3)))

    System.exit(0)
  }
}
