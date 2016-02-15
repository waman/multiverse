package org.waman.multiverse

import java.lang.reflect.Method
import java.{util => jcf}

import spire.math.Fractional

import scala.language.implicitConversions

class Per

trait UnitSystem extends PredefinedLengthUnit
    with PredefinedAreaUnit
    with PredefinedVolumeUnit
    with PredefinedAngleUnit
    with PredefinedSolidAngleUnit
    with PredefinedMassUnit
    with PredefinedDensityUnit
    with PredefinedTimeUnit
    with PredefinedFrequencyUnit
    with PredefinedVelocityUnit
    with PredefinedAngularVelocityUnit
    with PredefinedVolumeFlowUnit
    with PredefinedTimeSquaredUnit
    with PredefinedAccelerationUnit
    with PredefinedForceUnit
    with HasContext{

  implicit def convertFractionalToUnitInterpreter[A: Fractional](value: A): UnitInterpreter[A] =
    new UnitInterpreter(value)

  implicit def convertAngularVelocityToFrequency[A: Fractional](av: AngularVelocity[A]): Frequency[A] =
    av.toFrequency

  implicit def convertFrequencyToAngularVelocity[A: Fractional](f: Frequency[A]): AngularVelocity[A] =
    f.toAngularVelocity

  val / = new Per
}

object UnitSystem extends UnitSystem{

  val getSupportedQuantities: Seq[String] =
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
      "AngularVelocity",
      "VolumeFlow"//,
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

  def getSupportedUnits[U <: PhysicalUnit[U]](quantityName: String): Seq[U] = {
    val unitType = Class.forName(s"org.waman.multiverse.${quantityName}Unit").asInstanceOf[Class[U]]
    getSupportedUnits(unitType)
  }

  def getSupportedUnits[U <: PhysicalUnit[U]](unitType: Class[U]): Seq[U]  = {
    getPostfixOpsClass(unitType, isClass=true).getMethods
      .filterNot(_.getName.endsWith("PostfixOps"))
      .flatMap(m => getUnits(m, unitType))
      .distinct
    }

  def getSupportedUnits[U <: PhysicalUnit[U]](unitType: Class[U], ord: U => String): Seq[U] =
    getSupportedUnits(unitType).sortBy(ord)

  private def getUnits[U <: PhysicalUnit[U]](m: Method, unitType: Class[U]): Seq[U] =
    m.getParameterCount match{
      case 0 => Seq(unitType.cast(m.invoke(this)))
      case 1 => getSupportedContext(unitType, m.getName).map(c => m.invoke(this, c).asInstanceOf[U])
  }

  def getSupportedContext[U <: PhysicalUnit[U]](unitType: Class[U], unitSymbol: String): Seq[Context] = {
    val c = getPostfixOpsClass(unitType, isClass = false)
    val instance = c.getField("MODULE$").get(null)
    val m = c.getMethod(s"_$unitSymbol")
    val pf = m.invoke(instance).asInstanceOf[PartialFunction[Context, _]]
    Context.values.filter(c => pf.isDefinedAt(c))
  }

  private def getPostfixOpsClass[U <: PhysicalUnit[U]](unitType: Class[U], isClass: Boolean): Class[_] = {
    val dollar = if(isClass) "" else "$"
    val unitName = unitType.getSimpleName
    Class.forName(s"org.waman.multiverse.${unitName.substring(0, unitName.length - 4)}PostfixOps$dollar")
  }
}