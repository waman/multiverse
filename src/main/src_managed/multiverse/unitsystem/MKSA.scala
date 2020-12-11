package multiverse.unitsystem

import scala.language.implicitConversions

import multiverse.unit.electromagnetism.ElectricCharge
import multiverse.unit.electromagnetism.ElectricCurrent
import multiverse.unit.electromagnetism.Voltage
import multiverse.unit.electromagnetism.ElectricDipole
import multiverse.unit.electromagnetism.ElectricalResistance
import multiverse.unit.electromagnetism.ElectricalConductance
import multiverse.unit.electromagnetism.Capacitance
import multiverse.unit.electromagnetism.MagneticFlux
import multiverse.unit.electromagnetism.MagneticFluxDensity
import multiverse.unit.electromagnetism.MagneticFieldStrength
import multiverse.unit.electromagnetism.Inductance
import multiverse.unit.radioactivity.Exposure

import multiverse.unit.electromagnetism.ElectricChargeUnitObjects.coulomb
import multiverse.unit.electromagnetism.ElectricCurrentUnitObjects.ampere
import multiverse.unit.electromagnetism.VoltageUnitObjects.volt
import multiverse.unit.electromagnetism.ElectricDipoleUnitObjects.debye
import multiverse.unit.electromagnetism.ElectricalResistanceUnitObjects.ohm
import multiverse.unit.electromagnetism.ElectricalConductanceUnitObjects.siemens
import multiverse.unit.electromagnetism.CapacitanceUnitObjects.farad
import multiverse.unit.electromagnetism.MagneticFluxUnitObjects.weber
import multiverse.unit.electromagnetism.MagneticFluxDensityUnitObjects.tesla
import multiverse.unit.basic.LengthUnitObjects.metre
import multiverse.unit.electromagnetism.InductanceUnitObjects.henry
import multiverse.unit.basic.MassUnitObjects.kilogram

trait MKSA extends MKS{
  implicit def evaluateElectricCharge[A: Fractional](q: ElectricCharge[A]): A = q(coulomb)
  implicit def evaluateElectricCurrent[A: Fractional](q: ElectricCurrent[A]): A = q(ampere)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(volt)
  implicit def evaluateElectricDipole[A: Fractional](q: ElectricDipole[A]): A = q(debye)
  implicit def evaluateElectricalResistance[A: Fractional](q: ElectricalResistance[A]): A = q(ohm)
  implicit def evaluateElectricalConductance[A: Fractional](q: ElectricalConductance[A]): A = q(siemens)
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(farad)
  implicit def evaluateMagneticFlux[A: Fractional](q: MagneticFlux[A]): A = q(weber)
  implicit def evaluateMagneticFluxDensity[A: Fractional](q: MagneticFluxDensity[A]): A = q(tesla)
  implicit def evaluateMagneticFieldStrength[A: Fractional](q: MagneticFieldStrength[A]): A = q(ampere / metre)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(henry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(coulomb / kilogram)
}

object MKSA extends MKSA
