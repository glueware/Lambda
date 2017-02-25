object Main {
  def main(args: Array[String]): Unit = {

    def run[S](state: S, e: Expression[S, Any]) { println(e(state)) }

    def plus_(e1: Expression[Any, Int], e2: Expression[Any, Int]) = BinOp[Any, Int]({ (i, j) => i + j }, e1, e2)
    val i = Number(4)
    val j = Number(4)
    run(null, plus_(i, j))

    def plus(x: Int, y: Int) = { x + y }
    val x = new Variable[Int]()
    val y = new Variable[Int]()
    val plus0 = BinOp(plus, x, y)
    val plus1 = Lambda(y, plus0)
    val plusE = Lambda(x, plus1)
    val plus3 = Apply(plusE, Number(3))
    //    run(plus3)
    run(State(), Apply(plus3, Number(1)))

    run(State(), BinOp(plus, Query(Number(1)), Query(Number(2))))

    System.exit(0)
  }
}

case class Query(param: Expression[Any, Int]) extends Expression[State, Int] {
  def apply(state: State): Value[Int] = {
    Value(state.value(param(state).get))
  }
}

case class State() {
  val value = Array(1, 2, 4)
}
