package com.glueware.application


object Indent {
  var indent = 0
  
  def increaseIndent[A] (a : => A) = {
    indent = indent + 4
    val result = a
    indent = indent - 4
    result
  }
}
import Indent._

/**
 * Component analog scalaz.Lens
 */
import scalaz._, Scalaz._

/**
 * Component is built analog to a lens
 *
 */
object Component {
  type IDType = Long

  ////////////////////////////////////////////
  /** Map of registered Components **/
  private var components = Map[Long, Component[_, _]]()
  private var id: IDType = _
  /** register components for retrieval by id **/
  def ID(c: Component[_, _]) = {
    id = id + 1
    components += (id -> c)
    id
  }
  def apply(id: IDType) = {
    components(id)
  }

  var replaceMode = false
  ////////////////////////////////////////////

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def asState[A, B](component: Component[A, B]): State[Component[_, A], Component[A, B]] = component.toState

  /** automatically lifting a value of type B into its representation as constant **/
  //  implicit def asConstant[A, B: Manifest](value: B) = Constant(value)

  /** automatically converting to its result. 
   * Important for Object and ObjectClass.
   **/
  implicit def asResult[A, B](component: Component[A, B]): B = component.result
}

abstract class Component[A, B: Manifest] {
  _this =>

  //  type OriginalType <: Component[A, B]
  //  implicit def asOriginalType = this.asInstanceOf[OriginalType]

  import Component._
  /** registered id in Components of Application **/
  lazy val id: String = _this.getClass.getName // TODO

  /**
   * work around for Java type erasure
   * type needed by e.g. presentation layer
   */
  val m = manifest[B]

  /**
   * result available after apply
   */
  var result: B = _

  /** place holder for a more complex annotation structure **/
  var contenteditable = false

  /**
   * A Lens[A,B] can be used as a function from A => B,
   * or implicitly via Lens.asState as a State[A,B] action
   *
   *  def apply(state: A): B
   *
   * Component apply does not return result but the component itsself
   * thus Component may be used as a communication channel between layers talking about the result
   * necessary for event management, attributes, ...
   *
   * getting layer stack of Components, see Application
   */
  protected def _apply(state: Component[_, A]): this.type
  final def apply(state: Component[_, A]): this.type = {
    // indent = indent + 4
    val r = increaseIndent[this.type](_apply(state))
    // indent = indent - 4
    r
  }

  /** to state monad */
  implicit def toState: State[Component[_, A], Component[A, B]] = {
    state[Component[_, A], Component[A, B]](a => (a, apply(a)))
  }
  /** flatMapping a Component yields a state action to avoid ambiguity */
  def flatMap[C](f: Component[A, B] => State[Component[_, A], Component[A, C]]): State[Component[_, A], Component[A, C]] = state[Component[_, A], Component[A, C]](a => f(apply(a))(a))
  /** Mapping a Component yields a state action to avoid ambiguity */
  def map[C](f: Component[A, B] => Component[A, C]): State[Component[_, A], Component[A, C]] = state[Component[_, A], Component[A, C]](a => (a, f(apply(a))))

  /** Evaluate all input _this and return changed if a parameter changed **/
  def changeParams(state: Component[_, A]) = {
    var _paramChanged = false
    _this match {
      case t: Product =>
        for (parameter <- t.productIterator) {
          parameter match {
            case c: Component[_, _] => {
              try {
                c.asInstanceOf[Component[A, _]].apply(state)
                _paramChanged = _paramChanged || c.changed
              }
            }
            case _ =>
          }
        }
      case _ =>
    }
    _paramChanged
  }

  var replacing = replaceMode
  def replace = changed && replaceMode

  var _changed = false
  def changed_=(v: Boolean) {
    _changed = true
  }
  def changed = _changed
}

/** Something like Nil, Zero, ... **/
object Nihil
  extends Component[Nothing, Nothing] {
  def _apply(state: Component[_, Nothing]) = this
}

/** Components may be composed to a Composition **/
abstract class Composition[A, B: Manifest]
  extends Component[A, B] {
  val compositum: State[Component[_, A], Component[A, B]]
  var component: Component[A, B] = _ // component from application
  def _apply(state: Component[_, A]): this.type = {
    changed = changeParams(state)
    if (changed) {
      component = compositum ! state
      //      component.apply(state)
      result = component.result
    }
    this
  }
}

abstract class Constant[A, B: Manifest](_result: B) extends Component[A, B] {
  result = _result
  def _apply(state: Component[_, A]) = this
}

/** Operations: Numeric, ... **/
abstract class Operation[A, B: Manifest] extends Component[A, B] {
  def get(a: A): B
  def _apply(state: Component[_, A]): this.type = {
    changed = changeParams(state) || state.changed
    if (changed) {
      result = get(state.result)
    }
    this
  }
}




