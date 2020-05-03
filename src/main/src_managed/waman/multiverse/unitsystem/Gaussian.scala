package waman.multiverse.unitsystem

import scala.language.implicitConversions

import waman.multiverse.unit.electrics.Capacitance
import waman.multiverse.unit.electrics.Charge
import waman.multiverse.unit.electrics.Conductance
import waman.multiverse.unit.electrics.Current
import waman.multiverse.unit.electrics.Dipole
import waman.multiverse.unit.electrics.Resistance
import waman.multiverse.unit.electrics.Voltage
import waman.multiverse.unit.magnetics.Flux
import waman.multiverse.unit.magnetics.FluxDensity
import waman.multiverse.unit.magnetics.Inductance
import waman.multiverse.unit.radioactivity.Exposure

import waman.multiverse.unit.electrics.CapacitanceUnitObjects.farad
import waman.multiverse.unit.electrics.ChargeUnitObjects.statcoulomb
import waman.multiverse.unit.electrics.ConductanceUnitObjects.siemens
import waman.multiverse.unit.electrics.CurrentUnitObjects.ampere
import waman.multiverse.unit.electrics.DipoleUnitObjects.debye
import waman.multiverse.unit.electrics.ResistanceUnitObjects.ohm
import waman.multiverse.unit.electrics.VoltageUnitObjects.volt
import waman.multiverse.unit.magnetics.FluxUnitObjects.weber
import waman.multiverse.unit.magnetics.FluxDensityUnitObjects.tesla
import waman.multiverse.unit.magnetics.InductanceUnitObjects.henry
import waman.multiverse.unit.electrics.ChargeUnitObjects.coulomb
import waman.multiverse.unit.basic.MassUnitObjects.kilogram

trait Gaussian extends CGS{
  implicit def evaluateCapacitance[A: Fractional](q: Capacitance[A]): A = q(farad)
  implicit def evaluateCharge[A: Fractional](q: Charge[A]): A = q(statcoulomb)
  implicit def evaluateConductance[A: Fractional](q: Conductance[A]): A = q(siemens)
  implicit def evaluateCurrent[A: Fractional](q: Current[A]): A = q(ampere)
  implicit def evaluateDipole[A: Fractional](q: Dipole[A]): A = q(debye)
  implicit def evaluateResistance[A: Fractional](q: Resistance[A]): A = q(ohm)
  implicit def evaluateVoltage[A: Fractional](q: Voltage[A]): A = q(volt)
  implicit def evaluateFlux[A: Fractional](q: Flux[A]): A = q(weber)
  implicit def evaluateFluxDensity[A: Fractional](q: FluxDensity[A]): A = q(tesla)
  implicit def evaluateInductance[A: Fractional](q: Inductance[A]): A = q(henry)
  implicit def evaluateExposure[A: Fractional](q: Exposure[A]): A = q(coulomb / kilogram)
}

object Gaussian extends Gaussian
