/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2014, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package squants.experimental

import scala.util.{ Failure, Success, Try }

/**
 * A base trait for measurable quantities
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
abstract class QuantityX[T <: QuantityX[T, N], N] extends Ordered[T] with Serializable { self: T ⇒

  implicit val num: SquantsNumeric[N]
  import num.mkSquantsNumericOps

  /**
   * The value of the quantity given the valueUnits
   * @return Double
   */
  def value: N

  /**
   * The Unit of Measure used for the quantity's underlying value
   * @return UnitOfMeasure[T]
   */
  def valueUnit: UnitOfMeasureX[T]

  private def createQty(n: N): T = valueUnit(n)

  /**
   * Add two like quantities
   * @param that Quantity
   * @return Quantity
   */
  def plus(that: T): T = createQty(value + that.value)
  def +(that: T): T = plus(that)

  /**
   * Subtract two like quantities
   * @param that Quantity
   * @return Quantity
   */
  def minus(that: T): T = plus(that.negate)
  def -(that: T): T = minus(that)

  /**
   * Multiply this quantity by some number
   * @param that Double
   * @return Quantity
   */
  def times(that: N): T = createQty(value * that)
  def *(that: N): T = times(that)

  /**
   * Divide this quantity by some number
   * @param that Double
   * @return Quantity
   */
  def divide(that: N): T = createQty(value / that)
  def /(that: N): T = divide(that)

  /**
   * Divide this quantity by a like quantity
   * @param that Quantity
   * @return Double
   */
  def divide(that: T): N = value / that.value
  def /(that: T): N = divide(that)

  /**
   * Returns the remainder of a division by a number
   * @param that Quantity
   * @return Quantity
   */
  def remainder(that: N): T = createQty(value % that)
  def %(that: N): T = remainder(that)

  /**
   * Returns the remainder of a division by a like quantity
   * @param that Quantity
   * @return Double
   */
  def remainder(that: T): N = value % that.value
  def %(that: T): N = remainder(that)

  /**
   * Returns a Pair that includes the result of divideToInteger and remainder
   * @param that Double
   * @return (Quantity, Quantity)
   */
  def divideAndRemainder(that: N): (T, T) = value /% that match {
    case (q, r) ⇒ (createQty(q), createQty(r))
  }
  def /%(that: N) = divideAndRemainder(that)

  /**
   * Returns a Pair that includes the result of divideToInteger and remainder
   * @param that Quantity
   * @return (Double, Quantity)
   */
  def divideAndRemainder(that: T): (N, T) = value /% that.value match {
    case (q, r) ⇒ (q, createQty(r))
  }
  def /%(that: T) = divideAndRemainder(that)

  /**
   * Returns the negative value of this Quantity
   * @return Quantity
   */
  def negate: T = createQty(-value)
  def unary_-(): T = negate

  /**
   * Returns the absolute value of this Quantity
   * @return Quantity
   */
  def abs: T = createQty(num.abs(value))

  /**
   * Override of equals method
   *
   * @param that must be of matching value and unit
   * @return
   */
  override def equals(that: Any) = that match {
    // TODO Refactor so it also works for UnitBoxed types like Temperature, which may require change to UOM def
    // Currently this is satisfied by overrides in those classes
    case x: QuantityX[T, _] ⇒ value == x.value && valueUnit == x.valueUnit
    case _                  ⇒ false
  }

  /**
   * Override of hashCode
   *
   * @return
   */
  override def hashCode() = toString.hashCode()

  /**
   * Returns boolean result of approximate equality comparison
   * @param that Quantity
   * @param tolerance Quantity
   * @return
   */
  def approx(that: T)(implicit tolerance: T) = that within this.plusOrMinus(tolerance)
  /** approx */
  def =~(that: T)(implicit tolerance: T) = approx(that)
  /** approx */
  def ≈(that: T)(implicit tolerance: T) = approx(that)
  /** approx */
  def ~=(that: T)(implicit tolerance: T) = approx(that)

  /**
   * Implements Ordered.compare
   * @param that Quantity
   * @return Int
   */
  def compare(that: T) = if (value > that.value) 1 else if (value < that.value) -1 else 0

  /**
   * Returns the max of this and that Quantity
   * @param that Quantity
   * @return Quantity
   */
  def max(that: T): T = if (value >= that.value) this else that

  /**
   * Returns the min of this and that Quantity
   * @param that Quantity
   * @return Quantity
   */
  def min(that: T): T = if (value <= that.value) this else that

  /**
   * Returns a QuantityRange representing the range for this value +- that
   * @param that Quantity
   * @return QuantityRange
   */
  def plusOrMinus(that: T): QuantityRangeX[T, N] = QuantityRangeX[T, N](this - that, this + that)
  def +-(that: T) = plusOrMinus(that)

  /**
   * Returns a QuantityRange that goes from this to that
   * @param that Quantity
   * @return QuantityRange
   */
  def to(that: T): QuantityRangeX[T, N] = QuantityRangeX[T, N](this / num.fromInt(1), that)

  /**
   * Returns true if this value is within (contains) the range
   * @param range QuantityRange
   * @return Boolean
   */
  def within(range: QuantityRangeX[T, N]) = range.contains(self)

  /**
   * Returns true if this value is not within (contains) the range
   * @param range QuantityRange
   * @return Boolean
   */
  def notWithin(range: QuantityRangeX[T, N]) = !range.contains(self)

  /**
   * Returns a Double representing the quantity in terms of the supplied unit
   * {{{
   *   val d = Feet(3)
   *   (d to Inches) should be(36)
   * }}}
   * @param unit UnitOfMeasure[A]
   * @return Double
   */
  def to(unit: UnitOfMeasureX[T]): N = unit.convertTo(valueUnit.convertFrom(value))

  /**
   * Returns an equivalent Quantity boxed with the supplied Unit
   *
   * This is really only useful for Quantity classes that box at the UOM level
   * e.g. Temperature and currently Time
   *
   * @param unit UnitOfMeasure[A]
   * @return Quantity
   */
  def in(unit: UnitOfMeasureX[T]) = unit(unit.convertTo(valueUnit.convertFrom(value)))

  /**
   * Returns a string representing the quantity's value in valueUnits
   * @return String
   */
  override def toString = value.toString + " " + valueUnit.symbol

  /**
   * Returns a string representing the quantity's value in the given `unit`
   * @param unit UnitOfMeasure[A] with UnitConverter
   * @return String
   */
  def toString(unit: UnitOfMeasureX[T]): String = to(unit).toString + " " + unit.symbol

  /**
   * Returns a string representing the quantity's value in the given `unit` in the given `format`
   * @param unit UnitOfMeasure[A] with UnitConverter
   * @param format String containing the format for the value (ie "%.3f")
   * @return String
   */
  def toString(unit: UnitOfMeasureX[T], format: String): String = "%s %s".format(format.format(to(unit)), unit.symbol)
}

/**
 * Base class for creating objects to manage quantities as Numeric.
 *
 * One limitation is the `times` operation which is not supported by every quantity type
 *
 * @tparam A Quantity type
 */
abstract class AbstractQuantityNumericX[A <: QuantityX[A, B], B](val valueUnit: UnitOfMeasureX[A] with ValueUnitX) extends Numeric[A] {

  implicit val num: SquantsNumeric[B]
  import num.mkSquantsNumericOps

  def plus(x: A, y: A) = x + y
  def minus(x: A, y: A) = x - y

  /**
   * `times` is not a supported Numeric operation for Quantities.
   * It is not possible to multiply a dimensional quantity by a like quantity and get another like quantity.
   * Applying this class in a way that uses this method will result in an UnsupportedOperationException being thrown.
   *
   * @param x Quantity[A]
   * @param y Quantity[A]
   * @return
   * @throws UnsupportedOperationException for most types
   */
  def times(x: A, y: A): A = throw new UnsupportedOperationException("Numeric.times not supported for Quantities")
  def negate(x: A) = -x
  def fromInt(x: Int) = valueUnit(num.fromInt(x))
  def toInt(x: A) = x.value.toInt()
  def toLong(x: A) = x.value.toLong()
  def toFloat(x: A) = x.value.toFloat()
  def toDouble(x: A) = x.value.toDouble()
  def compare(x: A, y: A) = if (x.value > y.value) 1 else if (x.value < y.value) -1 else 0
}

case class QuantityStringParseException(message: String, expression: String) extends Exception

trait QuantityCompanionX[A <: QuantityX[A, _]] {
  def name: String
  def valueUnit: UnitOfMeasureX[A] with ValueUnitX
  def units: Set[UnitOfMeasureX[A]]

  def symbolToUnit(symbol: String): Option[UnitOfMeasureX[A]] = units.find(u ⇒ u.symbol == symbol)

  private lazy val QuantityString = ("([-+]?[0-9]*\\.?[0-9]+) *(" + units.map { u: UnitOfMeasureX[A] ⇒ u.symbol }.reduceLeft(_ + "|" + _) + ")").r
  protected def parseString(s: String): Try[A] = {
    s match {
      case QuantityString(value, symbol) ⇒ Success(symbolToUnit(symbol).get(BigDecimal(value)))
      case _                             ⇒ Failure(QuantityStringParseException(s"Unable to parse $name", s))
    }
  }
}

/**
 * SI Base Quantity
 */
trait BaseQuantityX { self: QuantityCompanionX[_] ⇒
  /**
   * SI Base Unit for this Quantity
   * @return
   */
  def baseUnit: BaseUnitX

  /**
   * SI Dimension Symbol
   * @return
   */
  def dimensionSymbol: String
}
