//case class Value[+A](value: A) {
//
//}

object LambdaCalculus {

abstract class Entry {
  type Typ
  val entry : Value[Typ]
}

type Name = String
type Environment = Map[Name, Entry]

abstract class Expression[-S, +A] {
  def apply(state: S, environment: Environment): Value[A]
}

case class Variable[A](name: Name) extends Expression[Any, A] {
  def apply(state: Any, environment: Environment): Value[A] = {
    environment(name).entry.asInstanceOf[Value[A]]
  }
}

case class Apply[S, A, B](f: Expression[S, Lambda[S, A, B]], e: Expression[S, A]) extends Expression[S, B] {
  def apply(state: S, environment: Environment): Value[B] = {
    println("->Apply")
    val SomeValue(Lambda(Variable(x), e1)): Value[Lambda[S, A, B]] = f(state, environment)
    val v = e(state, environment)
    val r = e1(state, environment + (x -> new Entry{type Typ = A; val entry = v}))
    println("<-Apply")
    r
  }
}

//case class VLambda[A, B](e: Lambda[A, B]) extends Value[Lambda[A, B]](e)
case class Lambda[S, A, B](v: Variable[A], e: Expression[S, B]) extends Expression[S, Lambda[S, A, B]] {
  def apply(state: S, environment: Environment): Value[Lambda[S, A, B]] = {
    println("->Lambda")
    val l = Value(this)
    println("<-Lambda")
    l
  }
}

object Test {
  val i = Value(0)
  val v = Variable[Any]("x")
}

//  case class VNumber(i: Int) extends Value[Int](i)
case class Number(i: Int) extends Expression[Any, Int] {
  def apply(state: Any, environment: Environment): Value[Int] = {
    println("->Number")
    val n = Value[Int](i)
    println("<-Number")
    n
  }
}

case class BinOp[S, A](f: (A, A) => A, e1: Expression[S, A], e2: Expression[S, A]) extends Expression[S, A] {
  def apply(state: S, environment: Environment): Value[A] = {
    println("->BinOp")
    val i1 = e1(state, environment)
    val i2 = e2(state, environment)
    val v = Value(f(i1.get, i2.get))
    println("<-BinOp")
    v
  }
}
}
