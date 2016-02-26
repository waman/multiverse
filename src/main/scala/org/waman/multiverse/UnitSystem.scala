package org.waman.multiverse

import java.lang.reflect.Method
import java.{util => jcf}

import org.waman.multiverse.angle._
import org.waman.multiverse.electric._
import org.waman.multiverse.energy.{PredefinedActionUnit, PredefinedEnergyUnit, PredefinedPowerUnit}
import org.waman.multiverse.fluid.{PredefinedDynamicViscosityUnit, PredefinedKinematicViscosityUnit, PredefinedPressureUnit, PredefinedVolumeFlowUnit}
import org.waman.multiverse.luminous.{PredefinedIlluminanceUnit, PredefinedLuminanceUnit, PredefinedLuminousFluxUnit, PredefinedLuminousIntensityUnit}
import org.waman.multiverse.magnetic.{PredefinedFluxDensityUnit, PredefinedFluxUnit, PredefinedInductanceUnit}
import org.waman.multiverse.mass.PredefinedMassUnit
import org.waman.multiverse.mechanics._
import org.waman.multiverse.metric.{PredefinedAreaUnit, PredefinedLengthUnit, PredefinedVolumeUnit}
import org.waman.multiverse.radiation._
import org.waman.multiverse.thermal.{PredefinedEntropyUnit, PredefinedTemperatureUnit}
import org.waman.multiverse.time.{Frequency, PredefinedFrequencyUnit, PredefinedTimeSquaredUnit, PredefinedTimeUnit}
import spire.math.Fractional

import scala.language.implicitConversions

class Dot
class Per

trait UnitSystemImplicits{

  implicit def convertFractionalToUnitInterpreter[A: Fractional](value: A): QuantityFactory[A] =
    new QuantityFactory(value)

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
    with PredefinedTimeSquaredUnit
    with PredefinedFrequencyUnit

    with PredefinedVelocityUnit
    with PredefinedAngularVelocityUnit
    with PredefinedVolumeFlowUnit
    with PredefinedAccelerationUnit
    with PredefinedForceUnit
    with PredefinedPressureUnit
//    with PredefinedTorqueUnit
    with PredefinedEnergyUnit
    with PredefinedPowerUnit
    with PredefinedActionUnit

    with PredefinedDynamicViscosityUnit
    with PredefinedKinematicViscosityUnit
    with PredefinedCurrentUnit
    with PredefinedChargeUnit
    with PredefinedDipoleUnit
    with PredefinedVoltageUnit
    with PredefinedResistanceUnit
    with PredefinedCapacitanceUnit
    with PredefinedFluxUnit
    with PredefinedFluxDensityUnit

    with PredefinedInductanceUnit
    with PredefinedTemperatureUnit
    with PredefinedEntropyUnit
    with PredefinedLuminousIntensityUnit
    with PredefinedLuminanceUnit
    with PredefinedLuminousFluxUnit
    with PredefinedIlluminanceUnit
    with PredefinedRadioactivityUnit
    with PredefinedExposureUnit
    with PredefinedAbsorbedDoseUnit

    with PredefinedEquivalentDoseUnit
//    with PredefinedEquivalentDoseRateUnit
    with HasContext

object UnitSystem extends UnitSystem{

  lazy val supportedQuantities: Set[Class[_]] = Set(
    classOf[metric.Length[_]],
    classOf[metric.Area[_]],
    classOf[metric.Volume[_]],
    classOf[angle.Angle[_]],
    classOf[angle.SolidAngle[_]],
    classOf[mass.Mass[_]],
    classOf[mass.Density[_]],
    classOf[time.Time[_]],
    classOf[time.TimeSquared[_]],
    classOf[time.Frequency[_]],

    classOf[mechanics.Velocity[_]],
    classOf[angle.AngularVelocity[_]],
    classOf[fluid.VolumeFlow[_]],
    classOf[mechanics.Acceleration[_]],
    classOf[mechanics.Force[_]],
    classOf[fluid.Pressure[_]],
    classOf[mechanics.Torque[_]],
    classOf[energy.Energy[_]],
    classOf[energy.Power[_]],
    classOf[energy.Action[_]],

    classOf[fluid.DynamicViscosity[_]],
    classOf[fluid.KinematicViscosity[_]],
    classOf[electric.Current[_]],
    classOf[electric.Charge[_]],
    classOf[electric.Dipole[_]],
    classOf[electric.Voltage[_]],
    classOf[electric.Resistance[_]],
    classOf[electric.Capacitance[_]],
    classOf[magnetic.Flux[_]],
    classOf[magnetic.FluxDensity[_]],

    classOf[magnetic.Inductance[_]],
    classOf[thermal.Temperature[_]],
    classOf[thermal.Entropy[_]],
    classOf[luminous.LuminousIntensity[_]],
    classOf[luminous.Luminance[_]],
    classOf[luminous.LuminousFlux[_]],
    classOf[luminous.Illuminance[_]],
    classOf[radiation.Radioactivity[_]],
    classOf[radiation.Exposure[_]],
    classOf[radiation.AbsorbedDose[_]],

    classOf[radiation.EquivalentDose[_]],
    classOf[radiation.EquivalentDoseRate[_]]
  )

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