//abstract class TupleExpression[-S] extends Expression[S, Product] {
//
//}
//
//abstract class __1[-S, +_1_](_1: Expression[S, _1_]) extends Expression[S, Tuple1[_1_]] {
//  def apply(s: S) = {
//    val a = _1.apply(s) 
//    a match {
//      case SomeValue(x) => Value(Tuple1(x))
//      case _ => NoValue(List(this.toString))
//    }
//  }
//}
//
//abstract class __2[-S, +_1_, +_2_](_1: Expression[S, _1_], _2: Expression[S, _2_]) extends Expression[S, Product2[_1_, _2_]]
//
//case class Value1[+_1_](_1: Value[_1_])
//case class Value2[+_1_, +_2_](_1: Value[_1_], _2: Value[_2_])
//
//abstract class Data[A]
//
//abstract class IX2(
//  val a: Data[Int],
//  val b: Data[String]) extends Tuple2(a, b)
//
//abstract class CX1(a: Int) extends Product1[Int]
//
//case class X1[S](a: Expression[S, Int]) extends __1[S, Int](a)
//
//
