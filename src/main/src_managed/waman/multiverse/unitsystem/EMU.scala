package waman.multiverse.unitsystem

import scala.language.implicitConversions

import waman.multiverse.unit.electromagnetism.ElectricCharge
import waman.multiverse.unit.electromagnetism.ElectricCurrent
import waman.multiverse.unit.electromagnetism.Voltage
import waman.multiverse.unit.electromagnetism.ElectricDipole
import waman.multiverse.unit.electromagnetism.ElectricalResistance
import waman.multiverse.unit.electromagnetism.Capacitance
import waman.multiverse.unit.electromagnetism.MagneticFlux
import waman.multiverse.unit.electromagnetism.MagneticFluxDensity
import waman.multiverse.unit.electromagnetism.MagneticFieldStrength
import waman.multiverse.unit.electromagnetism.Inductance
import waman.multiverse.unit.radioactivity.Exposure

import waman.multiverse.unit.electromagnetism.ElectricChargeUnitObjects.abcoulomb
import waman.multiverse.unit.electromagnetism.ElectricCurrentUnitObjects.abampere
import waman.multiverse.unit.electromagnetism.VoltageUnitObjects.abvolt
import waman.multiverse.unit.basic.LengthUnitObjects.centimetre
import waman.multiverse.unit.electromagnetism.ElectricalResistanceUnitObjects.abohm
import waman.multiverse.unit.electromagnetism.CapacitanceUnitObjects.abfarad
import waman.multiverse.unit.electromagnetism.MagneticFluxUnitObjects.maxwell
import waman.multiverse.unit.electromagnetism.MagneticFluxDensityUnitObjects.gauss
import waman.multiverse.unit.electromagnetism.MagneticFieldStrengthUnitObjects.oersted
import waman.multiverse.unit.electromagnetism.InductanceUnitObjects.abhenry
import waman.multiverse.unit.basic.MassUnitObjects.gram

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
