/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2014, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package squants.experimental

/**
 * A Unit of Measure is used to define the scale of a quantity measurement
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @tparam T The type of Quantity being measured
 */
trait UnitOfMeasureX[T <: QuantityX[T, _]] extends Serializable {
  /**
   * Factory method for creating instances of a Quantity in this UnitOfMeasure
   * @param n N - the Quantity's value in terms of this UnitOfMeasure
   * @return
   */
  def apply[N](n: N)(implicit num: SquantsNumeric[N]): T

  /**
   * Extractor method for getting the Numeric value of a Quantity in this UnitOfMeasure
   * @param q A - The Quantity being matched
   * @return
   */
  def unapply(q: T) = Some(q.to(this))

  /**
   * Symbol used when representing Quantities in this UnitOfMeasure
   * @return
   */
  def symbol: String

  /**
   * Defines a multiplier value relative to the Quantity's [[squants.ValueUnit]]
   *
   * @return
   */
  protected def conversionFactor: Double

  /**
   * Applies the converterTo method to a value
   * @param n N value in terms of the ValueUnit
   * @param num SquantsNumeric[N]
   * @tparam N Type
   * @return
   */
  final def convertTo[N](n: N)(implicit num: SquantsNumeric[N]) = num.divide(n, num.fromDouble(conversionFactor))

  /**
   * Applies the converterFrom method to a value
   *
   * @param n N value in terms of this Unit
   * @param num SquantsNumeric[N]
   * @tparam N Type
   * @return
   */
  final def convertFrom[N](n: N)(implicit num: SquantsNumeric[N]) = num.times(n, num.fromDouble(conversionFactor))
}

/**
 * Identifies the Unit of Measure used for storing the quantity's underlying value
 *
 * Each Quantity should have one and only one ValueUnit
 */
trait ValueUnitX { uom: UnitOfMeasureX[_] ⇒

  /**
   * Value unit multiplier is always equal to 1
   */
  final val conversionFactor = 1d
}

/**
 * A marker trait identifying SI Base Units
 */
trait BaseUnitX { uom: UnitOfMeasureX[_] ⇒ }
