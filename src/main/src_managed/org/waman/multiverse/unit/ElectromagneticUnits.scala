package org.waman.multiverse.unit

import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._
import org.waman.multiverse.unit.electromagnetism._

/**
 * Usually used units of electromagnetism
 */

object ElectromagneticUnits{

  def C: ElectricChargeUnit = ElectricChargeUnitObjects.coulomb
  def abC: ElectricChargeUnit = ElectricChargeUnitObjects.abcoulomb
  def statC: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  def Fr: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  def esu: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  def emu: ElectricChargeUnit = ElectricChargeUnitObjects.abcoulomb
  def au: ElectricChargeUnit = ElectricChargeUnitObjects.atomic_unit_of_charge
  def e: ElectricChargeUnit = ElectricChargeUnitObjects.atomic_unit_of_charge
  def A: ElectricCurrentUnit = ElectricCurrentUnitObjects.ampere
  def mA: ElectricCurrentUnit = ElectricCurrentUnitObjects.milliampere
  def kA: ElectricCurrentUnit = ElectricCurrentUnitObjects.kiloampere
  def abA: ElectricCurrentUnit = ElectricCurrentUnitObjects.abampere
  def statA: ElectricCurrentUnit = ElectricCurrentUnitObjects.statampere
  def V: VoltageUnit = VoltageUnitObjects.volt
  def mV: VoltageUnit = VoltageUnitObjects.millivolt
  def kV: VoltageUnit = VoltageUnitObjects.kilovolt
  def abV: VoltageUnit = VoltageUnitObjects.abvolt
  def statV: VoltageUnit = VoltageUnitObjects.statvolt
  def D: ElectricDipoleUnit = ElectricDipoleUnitObjects.debye
  def ea0: ElectricDipoleUnit = ElectricDipoleUnitObjects.atomic_unit_of_electric_dipole_moment
  def Ω: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.ohm
  def ohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.ohm
  def mΩ: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.milliohm
  def μΩ: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.microohm
  def mcΩ: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.microohm
  def kΩ: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.kiloohm
  def MΩ: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.megaohm
  def GΩ: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.gigaohm
  def abΩ: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.abohm
  def abOhm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.abohm
  def S: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.siemens
  def mho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.siemens
  def F: CapacitanceUnit = CapacitanceUnitObjects.farad
  def mF: CapacitanceUnit = CapacitanceUnitObjects.millifarad
  def μF: CapacitanceUnit = CapacitanceUnitObjects.microfarad
  def mcF: CapacitanceUnit = CapacitanceUnitObjects.microfarad
  def nF: CapacitanceUnit = CapacitanceUnitObjects.nanofarad
  def pF: CapacitanceUnit = CapacitanceUnitObjects.picofarad
  def abF: CapacitanceUnit = CapacitanceUnitObjects.abfarad
  def H: InductanceUnit = InductanceUnitObjects.henry
  def mH: InductanceUnit = InductanceUnitObjects.millihenry
  def μH: InductanceUnit = InductanceUnitObjects.microhenry
  def mcH: InductanceUnit = InductanceUnitObjects.microhenry
  def abH: InductanceUnit = InductanceUnitObjects.abhenry
  def Wb: MagneticFluxUnit = MagneticFluxUnitObjects.weber
  def mWb: MagneticFluxUnit = MagneticFluxUnitObjects.milliweber
  def μWb: MagneticFluxUnit = MagneticFluxUnitObjects.microweber
  def mcWb: MagneticFluxUnit = MagneticFluxUnitObjects.microweber
  def nWb: MagneticFluxUnit = MagneticFluxUnitObjects.nanoweber
  def Mx: MagneticFluxUnit = MagneticFluxUnitObjects.maxwell
  def statWb: MagneticFluxUnit = MagneticFluxUnitObjects.statweber
  def T: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.tesla
  def mT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.millitesla
  def μT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microtesla
  def mcT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microtesla
  def nT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.nanotesla
  def G: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gauss
  def Gs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gauss
  def statT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.stattesla
  def Oe: MagneticFieldStrengthUnit = MagneticFieldStrengthUnitObjects.oersted
}