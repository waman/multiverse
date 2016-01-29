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

object UnitSystem extends PredefinedLengthUnit
  with PredefinedAreaUnit
  with PredefinedVolumeUnit
  with PredefinedTimeUnit
  with PredefinedVelocityUnit
  with PredefinedAngleUnit
  with PredefinedAngularVelocityUnit
  with PredefinedSolidAngleUnit{

  def getSupportedQuantities: Seq[String] =
    Seq("Length", "Area", "Volume", "Time", "Velocity", "Angle", "AngularVelocity", "SolidAngle")

  def getSupportedUnits[U <: PhysicalUnit](quantityName: String): Seq[U] = {
    val unitType = Class.forName(s"org.waman.multiverse.${quantityName}Unit").asInstanceOf[Class[U]]
    getSupportedUnits(unitType)
  }

  def getSupportedUnits[U <: PhysicalUnit](unitType: Class[U]): Seq[U]  = {
    val predef = Class.forName(s"org.waman.multiverse.Predefined${unitType.getSimpleName}")
    predef.getMethods
      .filter(m => unitType.isAssignableFrom(m.getReturnType))
      .map(_.invoke(this))
      .map(u => unitType.cast(u))
  }
}