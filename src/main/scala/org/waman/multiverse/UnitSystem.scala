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

  def getSupportedUnits[U <: PhysicalUnit](quantityName: String): Seq[U] = {
    val unitType = Class.forName(s"org.waman.multiverse.${quantityName}Unit").asInstanceOf[Class[U]]
    getSupportedUnits(unitType)
  }

  def getSupportedUnits[U <: PhysicalUnit](unitType: Class[U]): Seq[U]  = {
    getPostfixOpsClass(unitType).getMethods
      .filterNot(_.getName.endsWith("PostfixOps"))
      .filterNot(_.getName.startsWith("_"))
      .flatMap(m => getUnits(m, unitType))
      .distinct
    }

  def getSupportedUnits[U <: PhysicalUnit](unitType: Class[U], ord: U => String): Seq[U] =
    getSupportedUnits(unitType).sortBy(ord)

  private def getUnits[U <: PhysicalUnit](m: Method, unitType: Class[U]): Seq[U] =
    m.getParameterCount match{
      case 0 => Seq(unitType.cast(m.invoke(this)))
      case 1 => getSupportedContext(unitType, m.getName).map(c => m.invoke(this, c).asInstanceOf[U])
  }

  lazy val getSupportedContext: Seq[Context] = {
    val cc = classOf[Context]
    getClass.getMethods
      .filter(m => cc.isAssignableFrom(m.getReturnType))
      .map(m => cc.cast(m.invoke(this)))
  }

  def getSupportedContext[U <: PhysicalUnit](unitType: Class[U], unitSymbol: String): Seq[Context] = {
    val m = getPostfixOpsClass(unitType).getMethods.find(_.getName == s"_$unitSymbol").get
    val pf = m.invoke(this).asInstanceOf[PartialFunction[Context, _]]
    getSupportedContext.filter(c => pf.isDefinedAt(c))
  }

  private def getPostfixOpsClass[U <: PhysicalUnit](unitType: Class[U]): Class[_] = {
    val unitName = unitType.getSimpleName
    Class.forName(s"org.waman.multiverse.${unitName.substring(0, unitName.length - 4)}PostfixOps")
  }
}