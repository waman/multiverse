package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

abstract class Time[A: Fractional]{

  def ms : A = s * 1000
  def s  : A
  def min: A = s / 60
  def h  : A = s / 3600
}

object Time{
  val msInSecond = r"0.001"
  val minInSecond = r"60"
  val hInSecond = minInSecond * 60
  val dInSecond = hInSecond * 24
//  val yrInSecond =
}

sealed abstract class TimeUnit(code: String)

object TimeUnit{
  case object ms extends TimeUnit("millisecond")
  case object s  extends TimeUnit("second")
  case object min  extends TimeUnit("minute")
  case object h    extends TimeUnit("hour")
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