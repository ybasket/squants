/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2014, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package squants.experimental

import scala.language.implicitConversions

trait SquantsNumeric[B] extends Ordering[B] {

  def plus(a: B, b: B): B
  def minus(a: B, b: B): B
  def times(a: B, b: B): B
  def divide(a: B, b: B): B
  def remainder(a: B, b: B): B
  def divideAndRemainder(a: B, b: B): (B, B)
  def negate(a: B): B

  def zero: B = fromInt(0)
  def one: B = fromInt(1)

  def abs(x: B): B = if (lt(x, zero)) negate(x) else x
  def signum(x: B): Int =
    if (lt(x, zero)) -1
    else if (gt(x, zero)) 1
    else 0

  def fromDouble(x: Double): B
  def fromFloat(x: Float): B
  def fromLong(x: Long): B
  def fromInt(x: Int): B

  def toDouble(a: B): Double
  def toFloat(a: B): Float
  def toLong(a: B): Long
  def toInt(a: B): Int

  class Ops(lhs: B) {
    def +(rhs: B) = plus(lhs, rhs)
    def -(rhs: B) = minus(lhs, rhs)
    def *(rhs: B) = times(lhs, rhs)
    def /(rhs: B) = divide(lhs, rhs)
    def %(rhs: B) = remainder(lhs, rhs)
    def /%(rhs: B) = divideAndRemainder(lhs, rhs)
    def unary_-() = negate(lhs)
    def abs(): B = SquantsNumeric.this.abs(lhs)
    def signum(): Int = SquantsNumeric.this.signum(lhs)
    def toInt(): Int = SquantsNumeric.this.toInt(lhs)
    def toLong(): Long = SquantsNumeric.this.toLong(lhs)
    def toFloat(): Float = SquantsNumeric.this.toFloat(lhs)
    def toDouble(): Double = SquantsNumeric.this.toDouble(lhs)

    def >(rhs: B) = compare(lhs, rhs) > 0
    def >=(rhs: B) = compare(lhs, rhs) >= 0
    def <(rhs: B) = compare(lhs, rhs) < 0
    def <=(rhs: B) = compare(lhs, rhs) <= 0
  }
  implicit def mkSquantsNumericOps(lhs: B): Ops = new Ops(lhs)
}

object SquantsNumeric {

  trait IntIsSquantsNumeric extends SquantsNumeric[Int] {
    def plus(a: Int, b: Int): Int = a + b
    def minus(a: Int, b: Int): Int = a - b
    def times(a: Int, b: Int): Int = a * b
    def divide(a: Int, b: Int): Int = a / b
    def negate(a: Int): Int = -a

    def remainder(a: Int, b: Int) = a % b
    def divideAndRemainder(a: Int, b: Int) = a /% b
    def fromDouble(x: Double) = x.toInt
    def fromFloat(x: Float) = x.toInt
    def fromLong(x: Long) = x.toInt
    def fromInt(x: Int) = x.toInt
    def toDouble(a: Int) = a.toDouble
    def toFloat(a: Int) = a.toFloat
    def toLong(a: Int) = a.toLong
    def toInt(a: Int) = a.toInt
  }

  trait LongIsSquantsNumeric extends SquantsNumeric[Long] {
    def plus(a: Long, b: Long): Long = a + b
    def minus(a: Long, b: Long): Long = a - b
    def times(a: Long, b: Long): Long = a * b
    def divide(a: Long, b: Long): Long = a / b
    def negate(a: Long): Long = -a

    def remainder(a: Long, b: Long) = a % b
    def divideAndRemainder(a: Long, b: Long) = a /% b
    def fromDouble(x: Double) = x.toLong
    def fromFloat(x: Float) = x.toLong
    def fromLong(x: Long) = x.toLong
    def fromInt(x: Int) = x.toLong
    def toDouble(a: Long) = a.toDouble
    def toFloat(a: Long) = a.toFloat
    def toLong(a: Long) = a.toLong
    def toInt(a: Long) = a.toInt
  }

  trait DoubleIsSquantsNumeric extends SquantsNumeric[Double] {
    def plus(a: Double, b: Double): Double = a + b
    def minus(a: Double, b: Double): Double = a - b
    def times(a: Double, b: Double): Double = a * b
    def divide(a: Double, b: Double): Double = a / b
    def negate(a: Double): Double = -a

    def remainder(a: Double, b: Double) = a % b
    def divideAndRemainder(a: Double, b: Double) = a /% b
    def fromDouble(x: Double) = x
    def fromFloat(x: Float) = x
    def fromLong(x: Long) = x
    def fromInt(x: Int) = x
    def toDouble(a: Double) = a.toDouble
    def toFloat(a: Double) = a.toFloat
    def toLong(a: Double) = a.toLong
    def toInt(a: Double) = a.toInt
  }

  trait BigDecimalIsSquantsNumeric extends SquantsNumeric[BigDecimal] {
    def plus(a: BigDecimal, b: BigDecimal): BigDecimal = a + b
    def minus(a: BigDecimal, b: BigDecimal): BigDecimal = a - b
    def times(a: BigDecimal, b: BigDecimal): BigDecimal = a * b
    def divide(a: BigDecimal, b: BigDecimal): BigDecimal = a / b
    def negate(a: BigDecimal): BigDecimal = -a

    def remainder(a: BigDecimal, b: BigDecimal) = a % b
    def divideAndRemainder(a: BigDecimal, b: BigDecimal) = a /% b
    def fromDouble(x: Double) = BigDecimal(x)
    def fromFloat(x: Float) = BigDecimal(x.toDouble)
    def fromLong(x: Long) = BigDecimal(x)
    def fromInt(x: Int) = BigDecimal(x)
    def toDouble(a: BigDecimal) = a.toDouble
    def toFloat(a: BigDecimal) = a.toFloat
    def toLong(a: BigDecimal) = a.toLong
    def toInt(a: BigDecimal) = a.toInt
  }

  implicit object IntIsSquantsNumeric extends IntIsSquantsNumeric with Ordering.IntOrdering
  implicit object LongIsSquantsNumeric extends LongIsSquantsNumeric with Ordering.LongOrdering
  implicit object DoubleIsSquantsNumeric extends DoubleIsSquantsNumeric with Ordering.DoubleOrdering

  implicit object BigDecimalIsSquantsNumeric extends BigDecimalIsSquantsNumeric with Ordering.BigDecimalOrdering
}
