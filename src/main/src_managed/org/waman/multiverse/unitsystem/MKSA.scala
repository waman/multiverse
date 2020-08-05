package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.electromagnetism.ElectricCharge
import org.waman.multiverse.unit.electromagnetism.ElectricCurrent
import org.waman.multiverse.unit.electromagnetism.Voltage
import org.waman.multiverse.unit.electromagnetism.ElectricDipole
import org.waman.multiverse.unit.electromagnetism.ElectricalResistance
import org.waman.multiverse.unit.electromagnetism.ElectricalConductance
import org.waman.multiverse.unit.electromagnetism.Capacitance
import org.waman.multiverse.unit.electromagnetism.MagneticFlux
import org.waman.multiverse.unit.electromagnetism.MagneticFluxDensity
import org.waman.multiverse.unit.electromagnetism.MagneticFieldStrength
import org.waman.multiverse.unit.electromagnetism.Inductance
import org.waman.multiverse.unit.radioactivity.Exposure

import org.waman.multiverse.unit.electromagnetism.ElectricChargeUnitObjects.coulomb
import org.waman.multiverse.unit.electromagnetism.ElectricCurrentUnitObjects.ampere
import org.waman.multiverse.unit.electromagnetism.VoltageUnitObjects.volt
import org.waman.multiverse.unit.electromagnetism.ElectricDipoleUnitObjects.debye
import org.waman.multiverse.unit.electromagnetism.ElectricalResistanceUnitObjects.ohm
import org.waman.multiverse.unit.electromagnetism.ElectricalConductanceUnitObjects.siemens
import org.waman.multiverse.unit.electromagnetism.CapacitanceUnitObjects.farad
import org.waman.multiverse.unit.electromagnetism.MagneticFluxUnitObjects.weber
import org.waman.multiverse.unit.electromagnetism.MagneticFluxDensityUnitObjects.tesla
import org.waman.multiverse.unit.basic.LengthUnitObjects.metre
import org.waman.multiverse.unit.electromagnetism.InductanceUnitObjects.henry
import org.waman.multiverse.unit.basic.MassUnitObjects.kilogram

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
