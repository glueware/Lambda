object Calculus {

  case class Environment(map: Map[Variable, Value]) // extends Value 
  {
    def apply(variable: Variable): Value = {
      val m = try {
        map(variable)
      } catch {
        case _ => throw new Exception("unbound variable " + variable)
      }
      m
    }

    def +(kvp: (Variable, Value)): Environment = {
      Environment(map + kvp)
    }
//    def restrict(freeVariables: Set[Variable]) = Environment(map.filterKeys(freeVariables.contains))
  }

  class Value

  abstract class Expression {
    def evaluate(environment: Environment): Value
//    val freeVariables: Set[Variable]
  }

  class Variable extends Expression {
//    lazy val freeVariables = Set(this) // empty + this
    def evaluate(environment: Environment): Value = {
      println("->Variable")
      val e = environment(this)
      println(environment)
      println("<-Variable")
      e
    }
  }

  case class Apply(f: Expression, e: Expression) extends Expression {
//    lazy val freeVariables: Set[Variable] = f.freeVariables ++ e.freeVariables
    def evaluate(environment: Environment): Value = {
      println("->Apply: " + f,  e)
      val VLambda(Lambda(v1, e1), environment1) = f.evaluate(environment)
      //      val v1 = Reference(CellClosure(e, environment.restrict(e.freeVariables))) // garbage collection
      val v = e.evaluate(environment)
      val r = e1.evaluate(environment1 + (v1 -> v))
      println(v1 -> v)
      println("<-Apply: " + r)
      r
    }
  }

  abstract class Cell
  case class CellClosure(e: Expression, environment: Environment) extends Cell
  case class CellValue(v: Value) extends Cell

  case class Reference(var c: Cell) {
    def get: Value = {
      c match {
        case CellClosure(e, environment) =>
          val v = e.evaluate(environment)
          c = CellValue(v)
          v
        case CellValue(v) => v
      }
    }
  }
  case class VLambda(e: Lambda, environment: Environment) extends Value
  case class Lambda(x: Variable, e: Expression) extends Expression {
//    lazy val freeVariables = e.freeVariables - x
    def evaluate(environment: Environment): Value = {
      println("->Lambda")
      val l = VLambda(Lambda(x, e), environment)
      println("<-Lambda")
      l
    }
  }

  case class VNumber(i: Int) extends Value
  case class Number(i: Int) extends Expression {
    lazy val freeVariables = Set[Variable]()
    def evaluate(env: Environment): Value = {
      println("->Number")
      val n = VNumber(i)
      println("<-Number")
      n
    }
  }

  case class BinOp(f: (Int, Int) => Int, e1: Expression, e2: Expression) extends Expression {
//    lazy val freeVariables = e1.freeVariables ++ e2.freeVariables
    def evaluate(env: Environment): Value = {
      println("->BinOp")
      val VNumber(i1) = e1.evaluate(env)
      val VNumber(i2) = e2.evaluate(env)
      val v = VNumber(f(i1, i2))
      println("<-BinOp")
      v
    }
  }
}