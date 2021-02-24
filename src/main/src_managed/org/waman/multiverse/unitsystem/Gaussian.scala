package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.electromagnetism.ElectricCharge
import org.waman.multiverse.unit.electromagnetism.ElectricCurrent
import org.waman.multiverse.unit.electromagnetism.Voltage
import org.waman.multiverse.unit.electromagnetism.ElectricDipole
import org.waman.multiverse.unit.electromagnetism.ElectricalResistance
import org.waman.multiverse.unit.electromagnetism.Capacitance
import org.waman.multiverse.unit.electromagnetism.MagneticFlux
import org.waman.multiverse.unit.electromagnetism.MagneticFluxDensity
import org.waman.multiverse.unit.electromagnetism.MagneticFieldStrength
import org.waman.multiverse.unit.electromagnetism.Inductance
import org.waman.multiverse.unit.radioactivity.Exposure

import org.waman.multiverse.unit.electromagnetism.ElectricChargeUnitObjects.statcoulomb
import org.waman.multiverse.unit.basic.TimeUnitObjects.second
import org.waman.multiverse.unit.electromagnetism.VoltageUnitObjects.statvolt
import org.waman.multiverse.unit.electromagnetism.ElectricDipoleUnitObjects.debye
import org.waman.multiverse.unit.electromagnetism.ElectricalResistanceUnitObjects.statohm
import org.waman.multiverse.unit.electromagnetism.CapacitanceUnitObjects.statfarad
import org.waman.multiverse.unit.electromagnetism.MagneticFluxUnitObjects.weber
import org.waman.multiverse.unit.electromagnetism.MagneticFluxDensityUnitObjects.tesla
import org.waman.multiverse.unit.electromagnetism.MagneticFieldStrengthUnitObjects.oersted
import org.waman.multiverse.unit.electromagnetism.InductanceUnitObjects.stathenry
import org.waman.multiverse.unit.basic.MassUnitObjects.gram

trait Gaussian extends CGS{
  implicit def evaluateElectricCharge[A: Fractional](q: ElectricCharge[A]): A = q(statcoulomb)
  implicit def evaluateElectricCurrent[A: Fractional](q: ElectricCurrent[A]): A = q(statcoulomb / second)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(statvolt)
  implicit def evaluateElectricDipole[A: Fractional](q: ElectricDipole[A]): A = q(debye)
  implicit def evaluateElectricalResistance[A: Fractional](q: ElectricalResistance[A]): A = q(statohm)
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(statfarad)
  implicit def evaluateMagneticFlux[A: Fractional](q: MagneticFlux[A]): A = q(weber)
  implicit def evaluateMagneticFluxDensity[A: Fractional](q: MagneticFluxDensity[A]): A = q(tesla)
  implicit def evaluateMagneticFieldStrength[A: Fractional](q: MagneticFieldStrength[A]): A = q(oersted)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(stathenry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(statcoulomb / gram)
}

object Gaussian extends Gaussian