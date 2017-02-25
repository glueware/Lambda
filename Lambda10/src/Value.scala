object Value {
  /**
   * An implicit conversion that converts an option to an iterable value
   */
  implicit def option2Iterable[A](xo: Value[A]): Iterable[A] = xo.toList

  /**
   * An Value factory which creates SomeValue(x) if the argument is not null,
   *  and NoValue if it is null.
   *
   *  @param  x the value
   *  @return   SomeValue(value) if value != null, NoValue if value == null
   */
  def apply[A](x: A): Value[A] = if (x == null) NoValue(List("Value.empty: uninitialized Value")) else SomeValue(x)

  /**
   * An Value factory which returns `NoValue` in a manner consistent with
   *  the collections hierarchy.
   */
  def empty[A]: Value[A] = NoValue(List("Value.empty: uninitialized Value"))

  def enter[A](message: String)(c: Value[A]): Value[A] = {
    c match {
      case NoValue(callstack, e) => NoValue(message :: callstack, e)
      case SomeValue(_) => c
    }
  }
}

/**
 * Represents optional values. Instances of `Value`
 *  are either an instance of $some or the object $none.
 *
 *  The most idiomatic way to use an $option instance is to treat it
 *  as a collection or monad and use `map`,`flatMap`, `filter`, or
 *  `foreach`:
 *
 *  {{{
 *  val name:Value[String] = request.getParameter("name")
 *  val upper = name map { _.trim } filter { _.length != 0 } map { _.toUpperCase }
 *  println(upper.getOrElse(""))
 *  }}}
 *
 *  Note that this is equivalent to {{{
 *  val upper = for {
 *    name <- request.getParameter("name")
 *    trimmed <- SomeValue(name.trim)
 *    upper <- SomeValue(trimmed.toUpperCase) if trimmed.length != 0
 *  } yield upper
 *  println(upper.getOrElse(""))
 *  }}}
 *
 *  Because of how for comprehension works, if $none is returned
 *  from `request.getParameter`, the entire expression results in
 *  $none
 *
 *  This allows for sophisticated chaining of $option values without
 *  having to check for the existence of a value.
 *
 *  A less-idiomatic way to use $option values is via pattern matching: {{{
 *  val nameMaybe = request.getParameter("name")
 *  nameMaybe match {
 *    case SomeValue(name) => {
 *      println(name.trim.toUppercase)
 *    }
 *    case NoValue => {
 *      println("No name value")
 *    }
 *  }
 *  }}}
 *
 *  @note Many of the methods in here are duplicative with those
 *  in the Traversable hierarchy, but they are duplicated for a reason:
 *  the implicit conversion tends to leave one with an Iterable in
 *  situations where one could have retained an Value.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.1, 16/01/2007
 *  @define none `NoValue`
 *  @define some [[scala.SomeValue]]
 *  @define option [[scala.Value]]
 *  @define p `p`
 *  @define f `f`
 */
import LambdaCalculus._

trait Value[+A] extends Expression[Any, A] {
  self =>

  def apply(state: Any, enviroment: Environment): this.type = this

  /**
   * Returns true if the option is $none, false otherwise.
   */
  def isEmpty: Boolean

  /**
   * Returns true if the option is an instance of $some, false otherwise.
   */
  def isDefined: Boolean = !isEmpty

  /**
   * Returns the option's value.
   *  @note The option must be nonEmpty.
   *  @throws Predef.NoSuchElementException if the option is empty.
   */
  def get: A

  /**
   * Returns the option's value if the option is nonempty, otherwise
   * return the result of evaluating `default`.
   *
   *  @param default  the default expression.
   */
  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.get

  /**
   * Returns the option's value if it is nonempty,
   * or `null` if it is empty.
   * Although the use of null is discouraged, code written to use
   * $option must often interface with code that expects and returns nulls.
   * @example {{{
   * val initalText: Value[String] = getInitialText
   * val textField = new JComponent(initalText.orNull,20)
   * }}}
   */
  @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = this getOrElse null

  /**
   * Returns a $some containing the result of applying $f to this $option's
   * value if this $option is nonempty.
   * Otherwise return $none.
   *
   *  @note This is similar to `flatMap` except here,
   *  $f does not need to wrap its result in an $option.
   *
   *  @param  f   the function to apply
   *  @see flatMap
   *  @see foreach
   */
  @inline final def map[B](f: A => B): Value[B] =
    Value.enter("Value.map") {
      this match {
        case NoValue(callstack, e) => NoValue(callstack, e)
        case SomeValue(a) => SomeValue(f(a))
      }
    }

  /**
   * Returns the result of applying $f to this $option's value if
   * this $option is nonempty.
   * Returns $none if this $option is empty.
   * Slightly different from `map` in that $f is expected to
   * return an $option (which could be $none).
   *
   *  @param  f   the function to apply
   *  @see map
   *  @see foreach
   */
  @inline final def flatMap[B](f: A => Value[B]): Value[B] =
    Value.enter("Value.flatMap") {
      this match {
        case NoValue(callstack, e) => NoValue(callstack, e)
        case SomeValue(a) => f(a)
      }
    }

  /**
   * Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns true. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  @inline final def filter(p: A => Boolean): Value[A] =
    Value.enter("Value.filter") {
      this match {
        case NoValue(callstack, e) => NoValue(callstack, e)
        case SomeValue(a) => if (p(a)) this else NoValue(List("Value.filter failed"))
      }
    }

  /**
   * Returns this $option if it is nonempty '''and''' applying the predicate $p to
   * this $option's value returns false. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  @inline final def filterNot(p: A => Boolean): Value[A] = filter(a => !p(a))

  /**
   * Necessary to keep $option from being implicitly converted to
   *  [[scala.collection.Iterable]] in `for` comprehensions.
   */
  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /**
   * We need a whole WithFilter class to honor the "doesn't create a new
   *  collection" contract even though it seems unlikely to matter much in a
   *  collection with max size 1.
   */
  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Value[B] = self filter p map f
    def flatMap[B](f: A => Value[B]): Value[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /**
   * Returns true if this option is nonempty '''and''' the predicate
   * $p returns true when applied to this $option's value.
   * Otherwise, returns false.
   *
   *  @param  p   the predicate to test
   */
  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /**
   * Apply the given procedure $f to the option's value,
   *  if it is nonempty. Otherwise, do nothing.
   *
   *  @param  f   the procedure to apply.
   *  @see map
   *  @see flatMap
   */
  @inline final def foreach[U](f: A => U) {
    if (!isEmpty) f(this.get)
  }

  /**
   * Returns a $some containing the result of
   * applying `pf` to this $option's contained
   * value, '''if''' this option is
   * nonempty '''and''' `pf` is defined for that value.
   * Returns $none otherwise.
   *
   *  @param  pf   the partial function.
   *  @return the result of applying `pf` to this $option's
   *  value (if possible), or $none.
   */
  def collect[B](pf: PartialFunction[A, B]): Value[B] =
    Value.enter("Value.collect") {
      this match {
        case NoValue(callstack, e) => NoValue(callstack, e)
        case SomeValue(a) => if (pf.isDefinedAt(a)) SomeValue(pf(a)) else NoValue(List("Value.collect failed"))
      }
    }

  /**
   * Returns this $option if it is nonempty,
   *  otherwise return the result of evaluating `alternative`.
   *  @param alternative the alternative expression.
   */
  @inline final def orElse[B >: A](alternative: => Value[B]): Value[B] =
    if (isEmpty) alternative else this

  /**
   * Returns a singleton iterator returning the $option's value
   * if it is nonempty, or an empty iterator if the option is empty.
   */
  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(this.get)

  /**
   * Returns a singleton list containing the $option's value
   * if it is nonempty, or the empty list if the $option is empty.
   */
  def toList: List[A] =
    if (isEmpty) List() else List(this.get)

  /**
   * Returns a [[scala.Left]] containing the given
   * argument `left` if this $option is empty, or
   * a [[scala.Right]] containing this $option's value if
   * this is nonempty.
   *
   * @param left the expression to evaluate and return if this is empty
   * @see toLeft
   */
  @inline final def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(this.get)

  /**
   * Returns a [[scala.Right]] containing the given
   * argument `right` if this is empty, or
   * a [[scala.Left]] containing this $option's value
   * if this $option is nonempty.
   *
   * @param right the expression to evaluate and return if this is empty
   * @see toRight
   */
  @inline final def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(this.get)
}

/**
 * Class `SomeValue[A]` represents existing values of type
 *  `A`.
 *
 *  @author
 *  @version
 */
final case class SomeValue[+A](x: A) extends Value[A] {
  def isEmpty = false
  def get = x
}

/**
 * This case object represents non-existent values.
 *
 *  @author
 *  @version
 */
case class NoValue[+A](callstack: List[String], exception: Option[Throwable] = None) extends Value[A] {
  def isEmpty = true
  def get = throw new NoSuchElementException("Value.NoValue.get")
}

//case class EvaluationErrorMessage(message: String, expresssion: Option[Any] = None, exception: Option[Exception] = None)
