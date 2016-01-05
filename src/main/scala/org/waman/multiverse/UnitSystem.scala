package org.waman.multiverse

import java.{util => jcf}

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import spire.math.{Real, Fractional}

trait ValueWithUnit[A, U <: PhysicalUnit]{
  val value: A
  val unit: U
  override def toString = s"$value (${unit.symbol})"
}

class Per

trait UnitSystem{

  implicit def convertDoubleToUnitInterpreter[A: Fractional](value: A): UnitInterpreter[A] =
    new UnitInterpreter(value)

  val / = new Per
}

object UnitSystem{

  def loadSystemResource(resource: String): Map[String, Real] = {
    val prop = new jcf.Properties()
    prop.load(ClassLoader.getSystemResourceAsStream(resource))
    prop.mapValues(Real(_)).toMap
  }

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