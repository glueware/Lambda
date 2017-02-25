object Main {
  def main(args: Array[String]): Unit = {
    def plus_(e1: Expression[Int], e2: Expression[Int]) = BinOp[Int]({ (i, j) => i + j }, e1, e2)
    val i = Number(4)
    val j = Number(4)
    plus_(i, j)

    def run(e: Expression[_]) { println(e.getClass.getName, e.evaluate()) }

    //    def plus(x: Int, y: Int) = { x + y }
    //    val x = new Variable[Int]()
    //    val y = new Variable[Int]()
    //    val plus0 = BinOp(plus, x, y)
    //    val plus1 = Lambda(y, plus0)
    //    val plusE = Lambda(x, plus1)
    //    val plus3 = Apply(plusE, Number(3))
    //    //    run(plus3)
    //    run(Apply(plus3, Number(1))) // prints 4

    //    type T[U] = Variable[Lambda[U,U]]

    //    type A = Lambda[A, _]
        val x = new Variable[Lambda[Expression[_], Expression[_]]]()

//    val x = new Variable[Lambda[Lambda[Lambda[_, Expression[_]],Expression[_]], Expression[_]]]()
    

    val idE = Lambda(x, x)
    run(idE)
    val sappE = Lambda(x, Apply(x, x))
//    val ididE = Apply(sappE, idE)
//    val threeE = Apply(ididE, Number(3))
//    run(threeE)
//    val y = new Variable[Lambda[Expression[Any], Expression[Any]]]()
//    run(Apply(Apply(Lambda(x, x), Lambda(y, y)), Number(3)))

  }
}

