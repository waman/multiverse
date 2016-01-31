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
  with PredefinedAngleUnit
  with PredefinedSolidAngleUnit
  with PredefinedMassUnit
  with PredefinedDensityUnit
  with PredefinedTimeUnit
  with PredefinedFrequencyUnit
  with PredefinedVelocityUnit
  with PredefinedAngularVelocityUnit{

  def getSupportedQuantities: Seq[String] =
    Seq("Length",
      "Area",
      "Volume",
      "Angle",
      "SolidAngle",
      "Mass",
      "Density",
      "Time",
      "Frequency",
      "Velocity",
      "AngularVelocity"//,
//      "VolumeFlow",
//      "Acceleration",
//      "Force",
//      "Pressure",
//      "Torque",
//      "Energy",
//      "Power",
//      "Action",
//      "DynamicViscosity",
//      "KinematicViscosity",
//      "Temperature"
    )

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