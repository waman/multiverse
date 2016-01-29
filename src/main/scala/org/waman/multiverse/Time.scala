package org.waman.multiverse

import spire.implicits._
import spire.math.{Fractional, Real}

trait TimePostfixOps[A]{
  def ns    : A
  def μs    : A
  def ms    : A
  def s     : A
  def minute: A
  def h     : A
  def d     : A
}

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends Quantity[A, TimeUnit]
    with TimePostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TimeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.inSecond) / real(evalUnit.inSecond)

  override def ns     = apply(TimeUnit.NanoSecond)
  override def μs     = apply(TimeUnit.MicroSecond)
  override def ms     = apply(TimeUnit.MilliSecond)
  override def s      = apply(TimeUnit.Second)
  override def minute = apply(TimeUnit.Minute)
  override def h      = apply(TimeUnit.Hour)
  override def d      = apply(TimeUnit.Day)
}

sealed abstract class TimeUnit(val name: String, val symbol: String, val inSecond: Real) extends PhysicalUnit

object TimeUnit{

  case object NanoSecond  extends TimeUnit("NanoSecond" , "ns"    , r"1e-9")
  case object MicroSecond extends TimeUnit("MicroSecond", "μs"    , r"1e-6")
  case object MilliSecond extends TimeUnit("MilliSecond", "ms"    , r"1e-3")
  case object Second      extends TimeUnit("Second"     , "s"     , r"1")
  case object Minute      extends TimeUnit("Minute"     , "minute", r"60")
  case object Hour        extends TimeUnit("Hour"       , "h"     , r"3600")
  case object Day         extends TimeUnit("Day"        , "d"     , r"3600" * r"24")
}

trait PredefinedTimeUnit{

  val ns  = TimeUnit.NanoSecond
  val μs  = TimeUnit.MicroSecond
  val ms  = TimeUnit.MilliSecond
  val s   = TimeUnit.Second
  val min = TimeUnit.Minute
  val h   = TimeUnit.Hour
  val d   = TimeUnit.Day
}

object PredefinedTimeUnit extends PredefinedTimeUnit

trait TimeUnitInterpreter[A] extends TimePostfixOps[Time[A]]{

  def apply(unit: TimeUnit): Time[A]

  override def ns     = apply(TimeUnit.NanoSecond)
  override def μs     = apply(TimeUnit.MicroSecond)
  override def ms     = apply(TimeUnit.MilliSecond)
  override def s      = apply(TimeUnit.Second)
  override def minute = apply(TimeUnit.Minute)
  override def h      = apply(TimeUnit.Hour)
  override def d      = apply(TimeUnit.Day)
}