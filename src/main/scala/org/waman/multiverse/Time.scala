package org.waman.multiverse

import spire.implicits._
import spire.math.{Fractional, Real}

abstract class Time[A: Fractional] extends UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  def ns : A = div(s, TimeUnit.NanoSecond.inSecond)
  def µs : A = div(s, TimeUnit.MicroSecond.inSecond)
  def ms : A = div(s, TimeUnit.MilliSecond.inSecond)
  def s  : A
  def min: A = div(s, TimeUnit.Minute.inSecond)
  def h  : A = div(s, TimeUnit.Hour.inSecond)
  def d  : A = div(s, TimeUnit.Day.inSecond)

  def apply(unit: TimeUnit): A = unit.accept(this)
}

sealed abstract class TimeUnit(val inSecond: Real = r"1"){
  def accept[A](t: Time[A]): A
  def accept[A](tui: TimeUnitInterpreter[A]): Time[A]
}

object TimeUnit{

  case object NanoSecond  extends TimeUnit(r"1e-9"){
    override def accept[A](t: Time[A]): A = t.ns
    override def accept[A](tui: TimeUnitInterpreter[A]): Time[A] = tui.ns
  }

  case object MicroSecond extends TimeUnit(r"1e-6"){
    override def accept[A](t: Time[A]): A = t.µs
    override def accept[A](tui: TimeUnitInterpreter[A]): Time[A] = tui.µs
  }

  case object MilliSecond  extends TimeUnit(r"1e-3"){
    override def accept[A](t: Time[A]): A = t.ms
    override def accept[A](tui: TimeUnitInterpreter[A]): Time[A] = tui.ms
  }

  case object Second extends TimeUnit(){
    override def accept[A](t: Time[A]): A = t.s
    override def accept[A](tui: TimeUnitInterpreter[A]): Time[A] = tui.s
  }

  case object Minute extends TimeUnit(r"60"){
    override def accept[A](t: Time[A]): A = t.min
    override def accept[A](tui: TimeUnitInterpreter[A]): Time[A] = tui.minute
  }

  case object Hour extends TimeUnit(r"3600"){
    override def accept[A](t: Time[A]): A = t.h
    override def accept[A](tui: TimeUnitInterpreter[A]): Time[A] = tui.h
  }

  case object Day extends TimeUnit(r"3600" * r"24"){
    override def accept[A](t: Time[A]): A = t.d
    override def accept[A](tui: TimeUnitInterpreter[A]): Time[A] = tui.d
  }
}

trait TimeUnitInterpreter[A]{

  def ns    : Time[A]
  def µs    : Time[A]
  def ms    : Time[A]
  def s     : Time[A]
  def minute: Time[A]
  def h     : Time[A]
  def d     : Time[A]

  def apply(unit: TimeUnit): Time[A] = unit.accept(this)
}