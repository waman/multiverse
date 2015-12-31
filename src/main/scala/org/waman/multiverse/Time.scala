package org.waman.multiverse

import spire.implicits._
import spire.math.{Fractional, Real}

abstract class Time[A: Fractional] extends UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  def ms : A = div(s, TimeUnit.ms.inSecond)
  def s  : A
  def min: A = div(s, TimeUnit.min.inSecond)
  def h  : A = div(s, TimeUnit.h.inSecond)
}

sealed abstract class TimeUnit(name: String, val inSecond: Real = r"1")

object TimeUnit{
  case object ms  extends TimeUnit("millisecond", r"0.001")
  case object s   extends TimeUnit("second")
  case object min extends TimeUnit("minute", r"60")
  case object h   extends TimeUnit("hour", r"60" * r"60")
  case object day extends TimeUnit("day", r"60" * r"60" * r"24")
}

trait TimeUnitInterpreter[A]{

  def ms    : Time[A]
  def s     : Time[A]
  def minute: Time[A]
  def h     : Time[A]

  def apply(unit: TimeUnit): Time[A] = unit match {
    case _ if unit == TimeUnit.ms  => ms
    case _ if unit == TimeUnit.s   => s
    case _ if unit == TimeUnit.min => minute
    case _ if unit == TimeUnit.h   => h
  }
}