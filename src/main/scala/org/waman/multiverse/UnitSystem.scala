package org.waman.multiverse

import java.{util => jcf}

import spire.math.Fractional

import scala.language.implicitConversions

class Per

trait UnitSystem{

  implicit def convertDoubleToUnitInterpreter[A: Fractional](value: A): UnitInterpreter[A] =
    new UnitInterpreter(value)

  val / = new Per
}

object UnitSystem{

  def getSupportedQuantities: Seq[String] =
    Seq("Length", "Time", "Velocity", "Angle", "AngularVelocity")

  def getSupportedUnits[U <: PhysicalUnit](quantityName: String): Seq[U] = {
    val unitType = Class.forName(s"org.waman.multiverse.${quantityName}Unit").asInstanceOf[Class[U]]
    getSupportedUnits(unitType)
  }

  def getSupportedUnits[U <: PhysicalUnit](unitType: Class[U]): Seq[U]  =
    UnitSystem.getClass.getMethods
      .filter(m => unitType.isAssignableFrom(m.getReturnType))
      .map(_.invoke(this))
      .map(u => unitType.cast(u))

  //***** Length *****
  val nm = LengthUnit.Nanometre
  val µm = LengthUnit.Micrometre
  val mm = LengthUnit.Millimetre
  val cm = LengthUnit.Centimetre
  val m  = LengthUnit.Metre
  val km = LengthUnit.Kilometre
  val Mm = LengthUnit.Megametre
  val Gm = LengthUnit.Gigametre
  val Tm = LengthUnit.Terametre

  // astronomy
  val au = LengthUnit.AstronomicalUnit
  val ly = LengthUnit.LightYear
  val pc = LengthUnit.Parsec

  // yard-pond
  val in = LengthUnit.Inch
  val ft = LengthUnit.Feet
  val yd = LengthUnit.Yard
  val mi = LengthUnit.Mile

  // Time
  val ns  = TimeUnit.Nanosecond
  val µs  = TimeUnit.Microsecond
  val ms  = TimeUnit.Millisecond
  val s   = TimeUnit.Second
  val min = TimeUnit.Minute
  val h   = TimeUnit.Hour
  val d   = TimeUnit.Day

  // Velocity
  val `m/s`  = VelocityUnit.MetrePerSecond
  val `km/h` = VelocityUnit.KilometrePerHour

  // Angle
  val deg = AngleUnit.Degree
  val rad = AngleUnit.Radian

  // Angular Velocity
  val `rad/s` = AngularVelocityUnit.RadianPerSecond
  val `deg/s` = AngularVelocityUnit.DegreePerSecond
}