case class Value[+A](value: A)

abstract class Expression[+A] {
  def evaluate(): Value[_ <: A]
}

class Variable[A]() extends Expression[A] {
  var value: Value[_ <: A] = _
  def evaluate(): Value[_ <: A] = {
    value
  }
}

case class Apply[A, B](f: Expression[Lambda[A, B]], e: Expression[A]) extends Expression[B] {
  def evaluate(): Value[B] = {
    println("->Apply")
    val Value(Lambda(v1, e1)): Value[Lambda[A, B]] = f.evaluate()
    val v = e.evaluate()
    v1.value = v // nonfunctional
    val r = e1.evaluate()
    println("<-Apply")
    r
  }
}

//case class VLambda[A, B](e: Lambda[A, B]) extends Value[Lambda[A, B]](e)
case class Lambda[A, B](v: Variable[A], e: Expression[B]) extends Expression[Lambda[A, B]] {
  def evaluate(): Value[Lambda[A, B]] = {
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
case class Number(i: Int) extends Expression[Int] {
  def evaluate(): Value[Int] = {
    println("->Number")
    val n = Value[Int](i)
    println("<-Number")
    n
  }
}

case class BinOp[A](f: (A, A) => A, e1: Expression[A], e2: Expression[A]) extends Expression[A] {
  def evaluate(): Value[A] = {
    println("->BinOp")
    val i1 = e1.evaluate()
    val i2 = e2.evaluate()
    val v = Value(f(i1.value, i2.value))
    println("<-BinOp")
    v
  }
}
