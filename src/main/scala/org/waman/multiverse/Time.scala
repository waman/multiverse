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

abstract class Time[A: Fractional] extends TimePostfixOps[A] with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  def ns    : A = div(s, TimeUnit.NanoSecond.inSecond)
  def µs    : A = div(s, TimeUnit.MicroSecond.inSecond)
  def ms    : A = div(s, TimeUnit.MilliSecond.inSecond)
  def s     : A
  def minute: A = div(s, TimeUnit.Minute.inSecond)
  def h     : A = div(s, TimeUnit.Hour.inSecond)
  def d     : A = div(s, TimeUnit.Day.inSecond)

  def apply(unit: TimeUnit): A = unit.accept(this)
}

abstract class TimeUnit(val inSecond: Real = r"1"){
  def accept[A](t: Time[A]): A
  def accept[A](ui: TimeUnitInterpreter[A]): Time[A]
}

object TimeUnit{

  case object NanoSecond  extends TimeUnit(r"1e-9"){
    override def accept[A](t: Time[A]): A = t.ns
    override def accept[A](ui: TimeUnitInterpreter[A]): Time[A] = ui.ns
  }

  case object MicroSecond extends TimeUnit(r"1e-6"){
    override def accept[A](t: Time[A]): A = t.µs
    override def accept[A](ui: TimeUnitInterpreter[A]): Time[A] = ui.µs
  }

  case object MilliSecond  extends TimeUnit(r"1e-3"){
    override def accept[A](t: Time[A]): A = t.ms
    override def accept[A](ui: TimeUnitInterpreter[A]): Time[A] = ui.ms
  }

  case object Second extends TimeUnit(){
    override def accept[A](t: Time[A]): A = t.s
    override def accept[A](ui: TimeUnitInterpreter[A]): Time[A] = ui.s
  }

  case object Minute extends TimeUnit(r"60"){
    override def accept[A](t: Time[A]): A = t.minute
    override def accept[A](ui: TimeUnitInterpreter[A]): Time[A] = ui.minute
  }

  case object Hour extends TimeUnit(r"3600"){
    override def accept[A](t: Time[A]): A = t.h
    override def accept[A](ui: TimeUnitInterpreter[A]): Time[A] = ui.h
  }

  case object Day extends TimeUnit(r"3600" * r"24"){
    override def accept[A](t: Time[A]): A = t.d
    override def accept[A](ui: TimeUnitInterpreter[A]): Time[A] = ui.d
  }
}

trait TimeUnitInterpreter[A] extends TimePostfixOps[Time[A]]{

  val value: A
  def apply(unit: TimeUnit): Time[A] = unit.accept(this)
}