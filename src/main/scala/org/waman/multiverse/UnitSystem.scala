package org.waman.multiverse

import java.lang.reflect.Method
import java.{util => jcf}

import org.waman.multiverse.angle._
import org.waman.multiverse.energy.{PredefinedActionUnit, PredefinedEnergyUnit, PredefinedPowerUnit}
import org.waman.multiverse.fluid.{PredefinedPressureUnit, PredefinedVolumeFlowUnit}
import org.waman.multiverse.mass.PredefinedMassUnit
import org.waman.multiverse.mechanics._
import org.waman.multiverse.metric.{PredefinedAreaUnit, PredefinedLengthUnit, PredefinedVolumeUnit}
import org.waman.multiverse.time.{Frequency, PredefinedFrequencyUnit, PredefinedTimeSquaredUnit, PredefinedTimeUnit}
import spire.math.Fractional

import scala.language.implicitConversions

class Dot
class Per

trait UnitSystemImplicits{

  implicit def convertFractionalToUnitInterpreter[A: Fractional](value: A): UnitInterpreter[A] =
    new UnitInterpreter(value)

  implicit def convertAngularVelocityToFrequency[A: Fractional](av: AngularVelocity[A]): Frequency[A] =
    av.toFrequency

  implicit def convertFrequencyToAngularVelocity[A: Fractional](f: Frequency[A]): AngularVelocity[A] =
    f.toAngularVelocity

  val * = new Dot
  val / = new Per
}

trait UnitSystem extends UnitSystemImplicits
    with PredefinedLengthUnit
    with PredefinedAreaUnit
    with PredefinedVolumeUnit
    with PredefinedAngleUnit
    with PredefinedSolidAngleUnit
    with PredefinedMassUnit
//    with PredefinedDensityUnit
    with PredefinedTimeUnit
    with PredefinedFrequencyUnit
    with PredefinedVelocityUnit
    with PredefinedAngularVelocityUnit
    with PredefinedVolumeFlowUnit
    with PredefinedTimeSquaredUnit
    with PredefinedAccelerationUnit
    with PredefinedForceUnit
    with PredefinedPressureUnit
//    with PredefinedTorqueUnit
    with PredefinedEnergyUnit
    with PredefinedPowerUnit
    with PredefinedActionUnit
    with HasContext

object UnitSystem extends UnitSystem{

  private val packageMap = Map(
    "Length"          -> "metric",
    "Area"            -> "metric",
    "Volume"          -> "metric",
    "Angle"           -> "angle",
    "SolidAngle"      -> "angle",
    "Mass"            -> "mass",
    "Density"         -> "mass",
    "Time"            -> "time",
    "TimeSquared"     -> "time",
    "Frequency"       -> "time",
    "Velocity"        -> "mechanics",
    "AngularVelocity" -> "angle",
    "VolumeFlow"      -> "fluid",
    "Acceleration"    -> "mechanics",
    "Force"           -> "mechanics",
    "Pressure"        -> "fluid",
    "Torque"          -> "mechanics",
    "Energy"          -> "energy",
    "Power"           -> "energy",
    "Action"          -> "energy"//,
    //      "DynamicViscosity",
    //      "KinematicViscosity",
    //      "Temperature"


  )

  val getSupportedQuantities: Set[String] = packageMap.keySet

  def getSupportedUnits[U <: PhysicalUnit[U]](quantityName: String): Seq[U] = {
    val className = s"org.waman.multiverse.${packageMap(quantityName)}.${quantityName}Unit"
    val unitType = Class.forName(className).asInstanceOf[Class[U]]
    getSupportedUnits(unitType)
  }

  def getSupportedUnits[U <: PhysicalUnit[U]](unitType: Class[U]): Seq[U]  = {
    getPostfixOpsClass(unitType, isClass=true) match{
      case None => Nil
      case Some(c) =>
        c.getMethods
        .filterNot(_.getName.endsWith("PostfixOps"))
        .flatMap(m => getUnits(m, unitType))
        .distinct
    }
  }

  def getSupportedUnits[U <: PhysicalUnit[U]](unitType: Class[U], ord: U => String): Seq[U] =
    getSupportedUnits(unitType).sortBy(ord)

  private def getUnits[U <: PhysicalUnit[U]](m: Method, unitType: Class[U]): Seq[U] =
    m.getParameterCount match{
      case 0 => Seq(unitType.cast(m.invoke(this)))
      case 1 => getSupportedContext(unitType, m.getName).map(c => m.invoke(this, c).asInstanceOf[U])
  }

  def getSupportedContext[U <: PhysicalUnit[U]](unitType: Class[U], unitSymbol: String): Seq[Context] = {
    getPostfixOpsClass(unitType, isClass = false) match {
      case None => Nil
      case Some(c) =>
        val instance = c.getField("MODULE$").get(null)
        val m = c.getMethod(s"_$unitSymbol")
        val pf = m.invoke(instance).asInstanceOf[PartialFunction[Context, _]]
        Context.values.filter(c => pf.isDefinedAt(c))
    }
  }

  private def getPostfixOpsClass[U <: PhysicalUnit[U]](unitType: Class[U], isClass: Boolean): Option[Class[_]] = {
    val dollar = if(isClass) "" else "$"
    val unitName = unitType.getSimpleName
    val packageName = unitType.getPackage.getName
    try {
      Some(Class.forName(s"$packageName.${unitName.substring(0, unitName.length - 4)}PostfixOps$dollar"))
    }catch{
      case ex: Exception => None
    }
  }
}