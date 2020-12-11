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

import multiverse.unit.electromagnetism.ElectricChargeUnitObjects.abcoulomb
import multiverse.unit.electromagnetism.ElectricCurrentUnitObjects.abampere
import multiverse.unit.electromagnetism.VoltageUnitObjects.abvolt
import multiverse.unit.basic.LengthUnitObjects.centimetre
import multiverse.unit.electromagnetism.ElectricalResistanceUnitObjects.abohm
import multiverse.unit.electromagnetism.CapacitanceUnitObjects.abfarad
import multiverse.unit.electromagnetism.MagneticFluxUnitObjects.maxwell
import multiverse.unit.electromagnetism.MagneticFluxDensityUnitObjects.gauss
import multiverse.unit.electromagnetism.MagneticFieldStrengthUnitObjects.oersted
import multiverse.unit.electromagnetism.InductanceUnitObjects.abhenry
import multiverse.unit.basic.MassUnitObjects.gram

trait EMU extends CGS{
  implicit def evaluateElectricCharge[A: Fractional](q: ElectricCharge[A]): A = q(abcoulomb)
  implicit def evaluateElectricCurrent[A: Fractional](q: ElectricCurrent[A]): A = q(abampere)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(abvolt)
  implicit def evaluateElectricDipole[A: Fractional](q: ElectricDipole[A]): A = q(abcoulomb * centimetre)
  implicit def evaluateElectricalResistance[A: Fractional](q: ElectricalResistance[A]): A = q(abohm)
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(abfarad)
  implicit def evaluateMagneticFlux[A: Fractional](q: MagneticFlux[A]): A = q(maxwell)
  implicit def evaluateMagneticFluxDensity[A: Fractional](q: MagneticFluxDensity[A]): A = q(gauss)
  implicit def evaluateMagneticFieldStrength[A: Fractional](q: MagneticFieldStrength[A]): A = q(oersted)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(abhenry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(abcoulomb / gram)
}

object EMU extends EMU
