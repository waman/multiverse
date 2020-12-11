package multiverse.unitsystem

import scala.language.implicitConversions

import multiverse.unit.electromagnetism.ElectricCharge
import multiverse.unit.electromagnetism.ElectricCurrent
import multiverse.unit.electromagnetism.Voltage
import multiverse.unit.electromagnetism.ElectricDipole
import multiverse.unit.electromagnetism.ElectricalResistance
import multiverse.unit.electromagnetism.Capacitance
import multiverse.unit.electromagnetism.MagneticFlux
import multiverse.unit.electromagnetism.MagneticFluxDensity
import multiverse.unit.electromagnetism.MagneticFieldStrength
import multiverse.unit.electromagnetism.Inductance
import multiverse.unit.radioactivity.Exposure

import multiverse.unit.electromagnetism.ElectricChargeUnitObjects.statcoulomb
import multiverse.unit.basic.TimeUnitObjects.second
import multiverse.unit.electromagnetism.VoltageUnitObjects.statvolt
import multiverse.unit.electromagnetism.ElectricDipoleUnitObjects.debye
import multiverse.unit.electromagnetism.ElectricalResistanceUnitObjects.statohm
import multiverse.unit.electromagnetism.CapacitanceUnitObjects.statfarad
import multiverse.unit.electromagnetism.MagneticFluxUnitObjects.weber
import multiverse.unit.electromagnetism.MagneticFluxDensityUnitObjects.tesla
import multiverse.unit.electromagnetism.MagneticFieldStrengthUnitObjects.oersted
import multiverse.unit.electromagnetism.InductanceUnitObjects.stathenry
import multiverse.unit.basic.MassUnitObjects.gram

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
