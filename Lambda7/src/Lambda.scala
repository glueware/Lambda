case class Value[+A](value: A) {

}

abstract class Expression[-S, +A] {
  def apply(state: S): Value[_ <: A]
}

class Variable[A]() extends Expression[Any, A] {
  var value: Value[_ <: A] = _
  def apply(state: Any): Value[_ <: A] = {
    value
  }
}

case class Apply[S, A, B](f: Expression[S, Lambda[S, A, B]], e: Expression[S, A]) extends Expression[S, B] {
  def apply(state: S): Value[B] = {
    println("->Apply")
    val Value(Lambda(v1, e1)): Value[Lambda[S, A, B]] = f(state)
    val v = e(state)
    v1.value = v // nonfunctional
    val r = e1(state)
    println("<-Apply")
    r
  }
}

//case class VLambda[A, B](e: Lambda[A, B]) extends Value[Lambda[A, B]](e)
case class Lambda[S, A, B](v: Variable[A], e: Expression[S, B]) extends Expression[S, Lambda[S, A, B]] {
  def apply(state: S): Value[Lambda[S, A, B]] = {
    println("->Lambda")
    val l = Value(Lambda(v, e))
    println("<-Lambda")
    l
  }
}

object Test {
  val i = Value(0)
  val v = new Variable[Any]
  v.value = i
}


//  case class VNumber(i: Int) extends Value[Int](i)
case class Number(i: Int) extends Expression[Any, Int] {
  def apply(state: Any): Value[Int] = {
    println("->Number")
    val n = Value[Int](i)
    println("<-Number")
    n
  }
}

case class BinOp[S, A](f: (A, A) => A, e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A] {
  def apply(state: S): Value[A] = {
    println("->BinOp")
    val i1 = e1(state)
    val i2 = e2(state)
    val v = Value(f(i1.value, i2.value))
    println("<-BinOp")
    v
  }
}
