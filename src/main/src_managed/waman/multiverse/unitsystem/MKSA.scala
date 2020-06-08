package waman.multiverse.unitsystem

import scala.language.implicitConversions

import waman.multiverse.unit.electromagnetism.Capacitance
import waman.multiverse.unit.electromagnetism.ElectricCharge
import waman.multiverse.unit.electromagnetism.ElectricalConductance
import waman.multiverse.unit.electromagnetism.ElectricCurrent
import waman.multiverse.unit.electromagnetism.ElectricDipole
import waman.multiverse.unit.electromagnetism.ElectricalResistance
import waman.multiverse.unit.electromagnetism.Voltage
import waman.multiverse.unit.electromagnetism.MagneticFlux
import waman.multiverse.unit.electromagnetism.MagneticFluxDensity
import waman.multiverse.unit.electromagnetism.Inductance
import waman.multiverse.unit.radioactivity.Exposure

import waman.multiverse.unit.electromagnetism.CapacitanceUnitObjects.farad
import waman.multiverse.unit.electromagnetism.ElectricChargeUnitObjects.coulomb
import waman.multiverse.unit.electromagnetism.ElectricalConductanceUnitObjects.siemens
import waman.multiverse.unit.electromagnetism.ElectricCurrentUnitObjects.ampere
import waman.multiverse.unit.electromagnetism.ElectricDipoleUnitObjects.debye
import waman.multiverse.unit.electromagnetism.ElectricalResistanceUnitObjects.ohm
import waman.multiverse.unit.electromagnetism.VoltageUnitObjects.volt
import waman.multiverse.unit.electromagnetism.MagneticFluxUnitObjects.weber
import waman.multiverse.unit.electromagnetism.MagneticFluxDensityUnitObjects.tesla
import waman.multiverse.unit.electromagnetism.InductanceUnitObjects.henry
import waman.multiverse.unit.basic.MassUnitObjects.kilogram

trait MKSA extends MKS{
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(farad)
  implicit def evaluateElectricCharge[A: Fractional](q: ElectricCharge[A]): A = q(coulomb)
  implicit def evaluateElectricalConductance[A: Fractional](q: ElectricalConductance[A]): A = q(siemens)
  implicit def evaluateElectricCurrent[A: Fractional](q: ElectricCurrent[A]): A = q(ampere)
  implicit def evaluateElectricDipole[A: Fractional](q: ElectricDipole[A]): A = q(debye)
  implicit def evaluateElectricalResistance[A: Fractional](q: ElectricalResistance[A]): A = q(ohm)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(volt)
  implicit def evaluateMagneticFlux[A: Fractional](q: MagneticFlux[A]): A = q(weber)
  implicit def evaluateMagneticFluxDensity[A: Fractional](q: MagneticFluxDensity[A]): A = q(tesla)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(henry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(coulomb / kilogram)
}

object MKSA extends MKSA
