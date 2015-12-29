package org.waman.multiverse

import spire.implicits._
import spire.math.{Real, Fractional}

abstract class Length[A: Fractional] extends UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  def mm: A = convertTo(m, LengthUnit.mm.inMetre)
  def cm: A = convertTo(m, LengthUnit.cm.inMetre)
  def m : A
  def km: A = convertTo(m, LengthUnit.km.inMetre)

  def au: A = convertTo(m, LengthUnit.au.inMetre)
  def ly: A = convertTo(m, LengthUnit.ly.inMetre)
  def pc: A = convertTo(m, LengthUnit.pc.inMetre)

  def in: A = convertTo(m, LengthUnit.in.inMetre)
  def ft: A = convertTo(m, LengthUnit.ft.inMetre)
  def yd: A = convertTo(m, LengthUnit.yd.inMetre)
  def mi: A = convertTo(m, LengthUnit.mi.inMetre)


  def /(timeUnit: TimeUnit): Velocity[A] = {
    val timeInSecond: Double = timeUnit match{
      case _ if timeUnit == TimeUnit.ms  => 0.001
      case _ if timeUnit == TimeUnit.s   => 1
      case _ if timeUnit == TimeUnit.min => 60
      case _ if timeUnit == TimeUnit.h   => 3600
    }
    new UnitInterpreter(m / timeInSecond).`m/s`
  }
}

sealed abstract class LengthUnit(name: String, val inMetre: Real = r"1")

object LengthUnit{
  case object mm extends LengthUnit("millimetre", r"0.001")
  case object cm extends LengthUnit("centimetre", r"0.01")
  case object m  extends LengthUnit("metre")
  case object km extends LengthUnit("kilometre", r"1000")

  case object au extends LengthUnit("astronomical unit", r"149597870700")
  case object ly extends LengthUnit("light year", r"9.4607304725808e15")
  case object pc extends LengthUnit("parsec", r"3.08567782e16")

  case object in extends LengthUnit("inch", r"0.0254")
  case object ft extends LengthUnit("feet", r"0.3048")
  case object yd extends LengthUnit("yard", r"0.9144")
  case object mi extends LengthUnit("mile", r"1609.344")
}

trait LengthUnitInterpreter[A] extends UnitConverter[A]{

  val value: A

  def mm: Length[A]
  def cm: Length[A]
  def m : Length[A]
  def km: Length[A]

  def au: Length[A]
  def ly: Length[A]
  def pc: Length[A]

  def in: Length[A]
  def ft: Length[A]
  def yd: Length[A]
  def mi: Length[A]

  def apply(unit: LengthUnit): Length[A] = unit match {
    case _ if unit == LengthUnit.mm => mm
    case _ if unit == LengthUnit.cm => cm
    case _ if unit == LengthUnit.m  => m
    case _ if unit == LengthUnit.km => km

    case _ if unit == LengthUnit.au => au
    case _ if unit == LengthUnit.ly => ly
    case _ if unit == LengthUnit.pc => pc

    case _ if unit == LengthUnit.in => in
    case _ if unit == LengthUnit.ft => ft
    case _ if unit == LengthUnit.yd => yd
    case _ if unit == LengthUnit.mi => mi
  }

  def m(per: Per): LengthPer

  trait LengthPer{
    def s: Velocity[A]
  }
}