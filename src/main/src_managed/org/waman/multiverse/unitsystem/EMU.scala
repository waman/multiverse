package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.radioactivity._


trait EMU extends CGS {
  implicit def evaluateElectricCharge[A: Fractional](q: ElectricCharge[A]): A = q(ElectricChargeUnitObjects.abcoulomb)
  implicit def evaluateElectricCurrent[A: Fractional](q: ElectricCurrent[A]): A = q(ElectricCurrentUnitObjects.abampere)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(VoltageUnitObjects.abvolt)
  implicit def evaluateElectricDipole[A: Fractional](q: ElectricDipole[A]): A = q(ElectricChargeUnitObjects.abcoulomb * LengthUnitObjects.centimetre)
  implicit def evaluateElectricalResistance[A: Fractional](q: ElectricalResistance[A]): A = q(ElectricalResistanceUnitObjects.abohm)
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(CapacitanceUnitObjects.abfarad)
  implicit def evaluateMagneticFlux[A: Fractional](q: MagneticFlux[A]): A = q(MagneticFluxUnitObjects.maxwell)
  implicit def evaluateMagneticFluxDensity[A: Fractional](q: MagneticFluxDensity[A]): A = q(MagneticFluxDensityUnitObjects.gauss)
  implicit def evaluateMagneticFieldStrength[A: Fractional](q: MagneticFieldStrength[A]): A = q(MagneticFieldStrengthUnitObjects.oersted)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(InductanceUnitObjects.abhenry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(ElectricChargeUnitObjects.abcoulomb / MassUnitObjects.gram)
}

object EMU extends EMU
