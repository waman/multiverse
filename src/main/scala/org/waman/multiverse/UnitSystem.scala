package org.waman.multiverse

import java.{util => jcf}

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import spire.math.{Real, Fractional}

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
  val nm = LengthUnit.NanoMetre
  val µm = LengthUnit.MicroMetre
  val mm = LengthUnit.MilliMetre
  val cm = LengthUnit.CentiMetre
  val m  = LengthUnit.Metre
  val km = LengthUnit.KiloMetre
  val Mm = LengthUnit.MegaMetre
  val Gm = LengthUnit.GigaMetre
  val Tm = LengthUnit.TeraMetre

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
  val ms  = TimeUnit.ms
  val s   = TimeUnit.s
  val min = TimeUnit.min
  val h   = TimeUnit.h

  // Velocity
  val `m/s`  = VelocityUnit.`m/s`
  val `km/h` = VelocityUnit.`km/h`

  // Angle
  val deg = AngleUnit.deg
  val rad = AngleUnit.rad

  // Angular Velocity
  val `rad/s` = AngularVelocityUnit.`rad/s`
}