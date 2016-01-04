package org.waman.multiverse

import spire.implicits._
import spire.math.{Fractional, Real}

trait TimePostfixOps[A]{
  def ns    : A
  def µs    : A
  def ms    : A
  def s     : A
  def minute: A
  def h     : A
  def d     : A
}

trait DivisibleByTime[A]{
  def /(timeUnit: TimeUnit): A
}

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends ValueWithUnit[A, TimeUnit]
    with TimePostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TimeUnit): A =
    if(evalUnit == unit) value
    else value * real(unit.inSecond) / real(evalUnit.inSecond)

  override def ns     = apply(TimeUnit.Nanosecond)
  override def µs     = apply(TimeUnit.Microsecond)
  override def ms     = apply(TimeUnit.Millisecond)
  override def s      = apply(TimeUnit.Second)
  override def minute = apply(TimeUnit.Minute)
  override def h      = apply(TimeUnit.Hour)
  override def d      = apply(TimeUnit.Day)
}

abstract class TimeUnit(val name: String, val symbol: String, val inSecond: Real) extends PhysicalUnit

object TimeUnit{

  case object Nanosecond  extends TimeUnit("Nanosecond" , "ns"    , r"1e-9")
  case object Microsecond extends TimeUnit("Microsecond", "µs"    , r"1e-6")
  case object Millisecond extends TimeUnit("Millisecond", "ms"    , r"1e-3")
  case object Second      extends TimeUnit("Second"     , "s"     , r"1")
  case object Minute      extends TimeUnit("Minute"     , "minute", r"60")
  case object Hour        extends TimeUnit("Hour"       , "h"     , r"3600")
  case object Day         extends TimeUnit("Day"        , "d"     , r"3600" * r"24")
}

trait TimeUnitInterpreter[A] extends TimePostfixOps[Time[A]]{

  def apply(unit: TimeUnit): Time[A]

  override def ns     = apply(TimeUnit.Nanosecond)
  override def µs     = apply(TimeUnit.Microsecond)
  override def ms     = apply(TimeUnit.Millisecond)
  override def s      = apply(TimeUnit.Second)
  override def minute = apply(TimeUnit.Minute)
  override def h      = apply(TimeUnit.Hour)
  override def d      = apply(TimeUnit.Day)
}