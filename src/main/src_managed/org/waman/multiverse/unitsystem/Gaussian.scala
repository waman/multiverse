package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.radioactivity._


trait Gaussian extends CGS {
  implicit def evaluateElectricCharge[A: Fractional](q: ElectricCharge[A]): A = q(ElectricChargeUnitObjects.statcoulomb)
  implicit def evaluateElectricCurrent[A: Fractional](q: ElectricCurrent[A]): A = q(ElectricChargeUnitObjects.statcoulomb / TimeUnitObjects.second)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(VoltageUnitObjects.statvolt)
  implicit def evaluateElectricDipole[A: Fractional](q: ElectricDipole[A]): A = q(ElectricDipoleUnitObjects.debye)
  implicit def evaluateElectricalResistance[A: Fractional](q: ElectricalResistance[A]): A = q(ElectricalResistanceUnitObjects.statohm)
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(CapacitanceUnitObjects.statfarad)
  implicit def evaluateMagneticFlux[A: Fractional](q: MagneticFlux[A]): A = q(MagneticFluxUnitObjects.weber)
  implicit def evaluateMagneticFluxDensity[A: Fractional](q: MagneticFluxDensity[A]): A = q(MagneticFluxDensityUnitObjects.tesla)
  implicit def evaluateMagneticFieldStrength[A: Fractional](q: MagneticFieldStrength[A]): A = q(MagneticFieldStrengthUnitObjects.oersted)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(InductanceUnitObjects.stathenry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(ElectricChargeUnitObjects.statcoulomb / MassUnitObjects.gram)
}

object Gaussian extends Gaussian
