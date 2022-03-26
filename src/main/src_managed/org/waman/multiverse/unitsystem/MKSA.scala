package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.ra._


trait MKSA extends MKS {
  implicit def evaluateElectricCharge[A: Fractional](q: ElectricCharge[A]): A = q(ElectricChargeUnitObjects.coulomb)
  implicit def evaluateElectricCurrent[A: Fractional](q: ElectricCurrent[A]): A = q(ElectricCurrentUnitObjects.ampere)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(VoltageUnitObjects.volt)
  implicit def evaluateElectricDipole[A: Fractional](q: ElectricDipole[A]): A = q(ElectricDipoleUnitObjects.debye)
  implicit def evaluateElectricalResistance[A: Fractional](q: ElectricalResistance[A]): A = q(ElectricalResistanceUnitObjects.ohm)
  implicit def evaluateElectricalConductance[A: Fractional](q: ElectricalConductance[A]): A = q(ElectricalConductanceUnitObjects.siemens)
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(CapacitanceUnitObjects.farad)
  implicit def evaluateMagneticFlux[A: Fractional](q: MagneticFlux[A]): A = q(MagneticFluxUnitObjects.weber)
  implicit def evaluateMagneticFluxDensity[A: Fractional](q: MagneticFluxDensity[A]): A = q(MagneticFluxDensityUnitObjects.tesla)
  implicit def evaluateMagneticFieldStrength[A: Fractional](q: MagneticFieldStrength[A]): A = q(ElectricCurrentUnitObjects.ampere / LengthUnitObjects.metre)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(InductanceUnitObjects.henry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(ElectricChargeUnitObjects.coulomb / MassUnitObjects.kilogram)
}

object MKSA extends MKSA
