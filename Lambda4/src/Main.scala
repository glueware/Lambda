import Calculus._
object Main {
  def main(args: Array[String]): Unit = {
    def plus_(e1: Expression, e2: Expression) = BinOp({ (i, j) => i + j }, e1, e2)
    val i = Number(4)
    val j = Number(5)
    plus_(i, j)

//    println("Hello")
    val empty = Environment(Map())
    def run(e: Expression) { println(e, e.evaluate(empty)) }

    //    def plus(x: Int, y: Int) = { x + y }
    val x = new Variable
    //    val y = new Variable
    //    val plusE = Lambda(x, Lambda(y, BinOp(plus, x, y)))
    //    val plus3 = Apply(plusE, Number(3))
    //    //    run(plus3)
    //    run(Apply(plus3, Number(1))) // prints 4

    val idE = Lambda(x, x)
    val sappE = Lambda(x, Apply(x, x))
//    run(sappE)
    val ididE = Apply(sappE, idE)
    run(ididE)
    //    val threeE = Apply(ididE, Number(3))
    //    run(threeE)
    //    run(Apply(Apply(Lambda(x, x), Lambda(y, y)), Number(3)))

    System.exit(0)
  }
}
