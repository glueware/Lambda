object Calculus {

  case class Environment(map: Map[Variable[_], Value[_]]) // extends Value 
  {
    def apply(variable: Variable[_]): Value[_] = {
      val m = try {
        map(variable)
      } catch {
        case _ => throw new Exception("unbound variable " + variable)
      }
      m
    }

    def +(kvp: (Variable[_], Value[_])): Environment = {
      Environment(map + kvp)
    }
    def restrict(freeVariables: Set[Variable[_]]) = Environment(map.filterKeys(freeVariables.contains))
  }

  class Value[A]

  abstract class Expression[A] {

    def evaluate(environment: Environment): Value[A]
    //    val freeVariables: Set[Variable[_]]
  }

  class Variable[A] extends Expression[A] {
    //    lazy val freeVariables: Set[Variable[_]] = Set(this) // empty + this
    def evaluate(environment: Environment): Value[A] = {
      println("->Variable")
      val e: Value[A] = environment(this).asInstanceOf[Value[A]]
      println("<-Variable")
      e
    }
  }

  case class Apply[A](f: Lambda[A], e: Expression[A]) extends Expression[A] {
    //    lazy val freeVariables: Set[Variable[_]] = f.freeVariables ++ e.freeVariables
    def evaluate(environment: Environment): Value[A] = {
      println("->Apply")
      val VLambda(Lambda(v1, e1), environment1) = f.evaluate(environment)
      //      val v1 = Reference(CellClosure(e, environment.restrict(e.freeVariables))) // garbage collection
      val v = e.evaluate(environment)
      val r = e1.evaluate(environment1 + (v1 -> v))
      println("<-Apply")
      r
    }
  }

  case class VLambda[A](e: Lambda[A], environment: Environment) extends Value[A]
  case class Lambda[A](v: Variable[A], e: Expression[A]) extends Expression[A] {
    //    lazy val freeVariables = e.freeVariables - v
    def evaluate(environment: Environment): VLambda[A] = {
      println("->Lambda")
      val l = VLambda(Lambda(v, e), environment)
      println("<-Lambda")
      l
    }
  }

  case class VNumber(i: Int) extends Value[Int]
  case class Number(i: Int) extends Expression[Int] {
    //    lazy val freeVariables = Set[Variable[_]]()
    def evaluate(env: Environment): Value[Int] = {
      println("->Number")
      val n = VNumber(i)
      println("<-Number")
      n
    }
  }

  case class BinOp(f: (Int, Int) => Int, e1: Expression[Int], e2: Expression[Int]) extends Expression[Int] {
    //    lazy val freeVariables = e1.freeVariables ++ e2.freeVariables
    def evaluate(env: Environment): Value[Int] = {
      println("->BinOp")
      val VNumber(i1) = e1.evaluate(env)
      val VNumber(i2) = e2.evaluate(env)
      val v = VNumber(f(i1, i2))
      println("<-BinOp")
      v
    }
  }
}

//  abstract class Cell[A]
//  case class CellClosure[A](e: Expression[A], environment: Environment) extends Cell[A]
//  case class CellValue[A](v: Value[A]) extends Cell[A]
//
//  case class Reference[A](var c: Cell[A]) {
//    def get: Value[A] = {
//      c match {
//        case cc: CellClosure[A]=>
//          val v = cc.e.evaluate(cc.environment)
//          c = CellValue(v)
//          v
//        case CellValue(v) => v
//      }
//    }
//  }
