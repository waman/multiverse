package org.waman.multiverse

import spire.implicits._
import spire.math.{Real, Fractional}

abstract class Length[A: Fractional] extends UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  def nm: A = div(m, LengthUnit.NanoMetre.inMetre)
  def µm: A = div(m, LengthUnit.MicroMetre.inMetre)
  def mm: A = div(m, LengthUnit.MilliMetre.inMetre)
  def cm: A = div(m, LengthUnit.CentiMetre.inMetre)
  def m : A
  def km: A = div(m, LengthUnit.KiloMetre.inMetre)
  def Mm: A = div(m, LengthUnit.MegaMetre.inMetre)
  def Gm: A = div(m, LengthUnit.GigaMetre.inMetre)
  def Tm: A = div(m, LengthUnit.TeraMetre.inMetre)

  // astronomy
  def au: A = div(m, LengthUnit.AstronomicalUnit.inMetre)
  def ly: A = div(m, LengthUnit.LightYear.inMetre)
  def pc: A = div(m, LengthUnit.Parsec.inMetre)

  // yard-pond
  def in: A = div(m, LengthUnit.Inch.inMetre)
  def ft: A = div(m, LengthUnit.Feet.inMetre)
  def yd: A = div(m, LengthUnit.Yard.inMetre)
  def mi: A = div(m, LengthUnit.Mile.inMetre)

  def apply(unit: LengthUnit): A = unit.accept(this)

//  def apply(unit: LengthUnit): A = unit match {
//    case _ if unit == LengthUnit.NanoMetre => nm
//    case _ if unit == LengthUnit.MicroMetre => µm
//    case _ if unit == LengthUnit.MilliMetre => mm
//    case _ if unit == LengthUnit.CentiMetre => cm
//    case _ if unit == LengthUnit.Metre  => m
//    case _ if unit == LengthUnit.KiloMetre => km
//    case _ if unit == LengthUnit.MegaMetre  => Mm
//    case _ if unit == LengthUnit.GigaMetre => Gm
//    case _ if unit == LengthUnit.TeraMetre => Tm
//
//    case _ if unit == LengthUnit.AstronomicalUnit => au
//    case _ if unit == LengthUnit.LightYear => ly
//    case _ if unit == LengthUnit.Parsec => pc
//
//    case _ if unit == LengthUnit.Inch => in
//    case _ if unit == LengthUnit.Feet => ft
//    case _ if unit == LengthUnit.Yard => yd
//    case _ if unit == LengthUnit.Mile => mi
//  }

  def /(timeUnit: TimeUnit): Velocity[A] = {
    val timeInSecond: Real = timeUnit match{
      case _ if timeUnit == TimeUnit.ms  => TimeUnit.ms.inSecond
      case _ if timeUnit == TimeUnit.s   => TimeUnit.s.inSecond
      case _ if timeUnit == TimeUnit.min => TimeUnit.min.inSecond
      case _ if timeUnit == TimeUnit.h   => TimeUnit.h.inSecond
    }
    new UnitInterpreter(m / real(timeInSecond)).`m/s`
  }
}

sealed abstract class LengthUnit(val inMetre: Real = r"1"){
  def accept[A](l: Length[A]): A
  def accept[A](lui: LengthUnitInterpreter[A]): Length[A]
}

object LengthUnit{

  case object NanoMetre extends LengthUnit(r"1e-9") { // 1e-9 m/nm
    override def accept[A](l: Length[A]) = l.nm
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.nm
  }

  case object MicroMetre extends LengthUnit(r"1e-6") {
    override def accept[A](l: Length[A]) = l.µm
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.µm
  }

  case object MilliMetre extends LengthUnit(r"1e-3") {
    override def accept[A](l: Length[A]) = l.mm
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.mm
  }

  case object CentiMetre extends LengthUnit(r"1e-2") {
    override def accept[A](l: Length[A]) = l.cm
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.cm
  }

  case object Metre  extends LengthUnit() {
    override def accept[A](l: Length[A]) = l.m
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.m
  }

  case object KiloMetre extends LengthUnit(r"1e3") {
    override def accept[A](l: Length[A]) = l.km
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.km
  }

  case object MegaMetre extends LengthUnit(r"1e6") {
    override def accept[A](l: Length[A]) = l.Mm
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.Mm
  }

  case object GigaMetre extends LengthUnit(r"1e9") {
    override def accept[A](l: Length[A]) = l.Gm
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.Gm
  }

  case object TeraMetre extends LengthUnit(r"1e12") {
    override def accept[A](l: Length[A]) = l.Tm
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.Tm
  }


  // astronomy
  case object AstronomicalUnit extends LengthUnit(r"149597870700") {
    override def accept[A](l: Length[A]) = l.au
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.au
  }

  case object LightYear extends LengthUnit(r"9.4607304725808e15") {
    override def accept[A](l: Length[A]) = l.ly
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.ly
  }

  case object Parsec extends LengthUnit(r"3.08567782e16") {
    override def accept[A](l: Length[A]) = l.pc
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.pc
  }

  // yard-pond
  case object Inch extends LengthUnit(r"0.0254") {
    override def accept[A](l: Length[A]) = l.in
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.in
  }

  case object Feet extends LengthUnit(r"0.3048") {
    override def accept[A](l: Length[A]) = l.ft
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.ft
  }

  case object Yard extends LengthUnit(r"0.9144") {
    override def accept[A](l: Length[A]) = l.yd
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.yd
  }

  case object Mile extends LengthUnit(r"1609.344") {
    override def accept[A](l: Length[A]) = l.mi
    override def accept[A](lui: LengthUnitInterpreter[A]) = lui.mi
  }
}

trait LengthUnitInterpreter[A] extends UnitConverter[A]{

  val value: A

  def nm: Length[A]
  def µm: Length[A]
  def mm: Length[A]
  def cm: Length[A]
  def m : Length[A]
  def km: Length[A]
  def Mm: Length[A]
  def Gm: Length[A]
  def Tm: Length[A]

  // astronomy
  def au: Length[A]
  def ly: Length[A]
  def pc: Length[A]

  // yard-pond
  def in: Length[A]
  def ft: Length[A]
  def yd: Length[A]
  def mi: Length[A]

  def apply(unit: LengthUnit): Length[A] = unit.accept(this)

//  def apply(unit: LengthUnit): Length[A] = unit match {
//    case _ if unit == LengthUnit.NanoMetre => nm
//    case _ if unit == LengthUnit.MicroMetre => µm
//    case _ if unit == LengthUnit.MilliMetre => mm
//    case _ if unit == LengthUnit.CentiMetre => cm
//    case _ if unit == LengthUnit.Metre  => m
//    case _ if unit == LengthUnit.KiloMetre => km
//    case _ if unit == LengthUnit.MegaMetre  => Mm
//    case _ if unit == LengthUnit.GigaMetre => Gm
//    case _ if unit == LengthUnit.TeraMetre => Tm
//
//    case _ if unit == LengthUnit.AstronomicalUnit => au
//    case _ if unit == LengthUnit.LightYear => ly
//    case _ if unit == LengthUnit.Parsec => pc
//
//    case _ if unit == LengthUnit.Inch => in
//    case _ if unit == LengthUnit.Feet => ft
//    case _ if unit == LengthUnit.Yard => yd
//    case _ if unit == LengthUnit.Mile => mi
//  }

  def m(per: Per): LengthPer

  trait LengthPer{
    def s: Velocity[A]
  }
}