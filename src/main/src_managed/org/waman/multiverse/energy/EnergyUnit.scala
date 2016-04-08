package org.waman.multiverse.energy

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.mass._
import org.waman.multiverse.time._
import org.waman.multiverse.mechanics._
import org.waman.multiverse.fluid._
import org.waman.multiverse.electric._
import org.waman.multiverse.thermal._
import org.waman.multiverse.radiation._
import org.waman.multiverse.metric.VolumeUnit._
import org.waman.multiverse.electric.ChargeUnit._
import org.waman.multiverse.fluid.PressureUnit._

sealed trait EnergyUnit extends PhysicalUnit[EnergyUnit]
  with MultiplicativeByTimeUnit[ActionUnit]
  with DivisibleByTimeUnit[PowerUnit]
  with DivisibleByTemperatureUnit[EntropyUnit]
  with DivisibleByMassUnit[AbsorbedDoseUnit]{

  override def getSIUnit = org.waman.multiverse.energy.EnergyUnit.Joule

  override def *(unit: TimeUnit) = ActionUnit(this, unit)

  override def /(unit: TimeUnit) = PowerUnit(this, unit)

  override def /(unit: TemperatureUnit) = EntropyUnit(this, unit)

  override def /(unit: MassUnit) = AbsorbedDoseUnit(this, unit)
}

object EnergyUnit extends ConstantsDefined[EnergyUnit]{

  // intrinsic
  private[EnergyUnit]
  class IntrinsicEnergyUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends EnergyUnit{

    def this(name: String, symbols: Seq[String], unit: EnergyUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: EnergyUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoJoule extends IntrinsicEnergyUnit("YoctoJoule", Seq("yJ"), r"1e-24")
  case object ZeptoJoule extends IntrinsicEnergyUnit("ZeptoJoule", Seq("zJ"), r"1e-21")
  case object AttoJoule extends IntrinsicEnergyUnit("AttoJoule", Seq("aJ"), r"1e-18")
  case object FemtoJoule extends IntrinsicEnergyUnit("FemtoJoule", Seq("fJ"), r"1e-15")
  case object PicoJoule extends IntrinsicEnergyUnit("PicoJoule", Seq("pJ"), r"1e-12")
  case object NanoJoule extends IntrinsicEnergyUnit("NanoJoule", Seq("nJ"), r"1e-9")
  case object MicroJoule extends IntrinsicEnergyUnit("MicroJoule", Seq("μJ", "mcJ"), r"1e-6")
  case object MilliJoule extends IntrinsicEnergyUnit("MilliJoule", Seq("mJ"), r"1e-3")
  case object CentiJoule extends IntrinsicEnergyUnit("CentiJoule", Seq("cJ"), r"1e-2")
  case object DeciJoule extends IntrinsicEnergyUnit("DeciJoule", Seq("dJ"), r"1e-1")
  case object Joule extends IntrinsicEnergyUnit("Joule", Seq("J"), r"1")
  case object DecaJoule extends IntrinsicEnergyUnit("DecaJoule", Seq("daJ"), r"1e1")
  case object HectoJoule extends IntrinsicEnergyUnit("HectoJoule", Seq("hJ"), r"1e2")
  case object KiloJoule extends IntrinsicEnergyUnit("KiloJoule", Seq("kJ"), r"1e3")
  case object MegaJoule extends IntrinsicEnergyUnit("MegaJoule", Seq("MJ"), r"1e6")
  case object GigaJoule extends IntrinsicEnergyUnit("GigaJoule", Seq("GJ"), r"1e9")
  case object TeraJoule extends IntrinsicEnergyUnit("TeraJoule", Seq("TJ"), r"1e12")
  case object PetaJoule extends IntrinsicEnergyUnit("PetaJoule", Seq("PJ"), r"1e15")
  case object ExaJoule extends IntrinsicEnergyUnit("ExaJoule", Seq("EJ"), r"1e18")
  case object ZettaJoule extends IntrinsicEnergyUnit("ZettaJoule", Seq("ZJ"), r"1e21")
  case object YottaJoule extends IntrinsicEnergyUnit("YottaJoule", Seq("YJ"), r"1e24")
  case object Erg extends IntrinsicEnergyUnit("Erg", Seq("erg"), r"1e-7")
  case object YoctoElectronVolt extends IntrinsicEnergyUnit("YoctoElectronVolt", Seq("yeV"), r"1e-24" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object ZeptoElectronVolt extends IntrinsicEnergyUnit("ZeptoElectronVolt", Seq("zeV"), r"1e-21" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object AttoElectronVolt extends IntrinsicEnergyUnit("AttoElectronVolt", Seq("aeV"), r"1e-18" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object FemtoElectronVolt extends IntrinsicEnergyUnit("FemtoElectronVolt", Seq("feV"), r"1e-15" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object PicoElectronVolt extends IntrinsicEnergyUnit("PicoElectronVolt", Seq("peV"), r"1e-12" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object NanoElectronVolt extends IntrinsicEnergyUnit("NanoElectronVolt", Seq("neV"), r"1e-9" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object MicroElectronVolt extends IntrinsicEnergyUnit("MicroElectronVolt", Seq("μeV", "mceV"), r"1e-6" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object MilliElectronVolt extends IntrinsicEnergyUnit("MilliElectronVolt", Seq("meV"), r"1e-3" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object CentiElectronVolt extends IntrinsicEnergyUnit("CentiElectronVolt", Seq("ceV"), r"1e-2" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object DeciElectronVolt extends IntrinsicEnergyUnit("DeciElectronVolt", Seq("deV"), r"1e-1" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object ElectronVolt extends IntrinsicEnergyUnit("ElectronVolt", Seq("eV"), r"1" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object DecaElectronVolt extends IntrinsicEnergyUnit("DecaElectronVolt", Seq("daeV"), r"1e1" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object HectoElectronVolt extends IntrinsicEnergyUnit("HectoElectronVolt", Seq("heV"), r"1e2" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object KiloElectronVolt extends IntrinsicEnergyUnit("KiloElectronVolt", Seq("keV"), r"1e3" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object MegaElectronVolt extends IntrinsicEnergyUnit("MegaElectronVolt", Seq("MeV"), r"1e6" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object GigaElectronVolt extends IntrinsicEnergyUnit("GigaElectronVolt", Seq("GeV"), r"1e9" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object TeraElectronVolt extends IntrinsicEnergyUnit("TeraElectronVolt", Seq("TeV"), r"1e12" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object PetaElectronVolt extends IntrinsicEnergyUnit("PetaElectronVolt", Seq("PeV"), r"1e15" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object ExaElectronVolt extends IntrinsicEnergyUnit("ExaElectronVolt", Seq("EeV"), r"1e18" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object ZettaElectronVolt extends IntrinsicEnergyUnit("ZettaElectronVolt", Seq("ZeV"), r"1e21" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object YottaElectronVolt extends IntrinsicEnergyUnit("YottaElectronVolt", Seq("YeV"), r"1e24" * ElementaryCharge.unitValueInSIUnit) with NotExact
  case object Rydberg extends IntrinsicEnergyUnit("Rydberg", Seq("Ry"), r"13.6056925330", ElectronVolt) with NotExact
  case object AtomicUnitOfEnergy extends IntrinsicEnergyUnit("AtomicUnitOfEnergy", Seq("E_h"), 2, Rydberg) with NotExact
  case object Calorie_IT extends IntrinsicEnergyUnit("Calorie_IT", Seq("cal", "cal(IT)", "cal_IT"), r"4.1868")
  case object Calorie_thermochemical extends IntrinsicEnergyUnit("Calorie_thermochemical", Seq("cal(th)", "cal_th"), r"4.184")
  case object KiloCalorie extends IntrinsicEnergyUnit("KiloCalorie", Seq("kcal", "Cal"), 1000, Calorie_thermochemical)
  case object Calorie_15C extends IntrinsicEnergyUnit("Calorie_15C", Seq("cal_15C"), r"4.1855")
  case object BritishThermalUnit_ISO extends IntrinsicEnergyUnit("BritishThermalUnit_ISO", Seq("BTU", "BTU(ISO)", "BTU_ISO"), r"1055.056")
  case object BritishThermalUnit_IT extends IntrinsicEnergyUnit("BritishThermalUnit_IT", Seq("BTU(IT)", "BTU_IT"), r"105505585262")
  case object BritishThermalUnit_thermochemical extends IntrinsicEnergyUnit("BritishThermalUnit_thermochemical", Seq("BTU(th)", "BTU_th"), r"1054.35026444")
  case object BritishThermalUnit_59F extends IntrinsicEnergyUnit("BritishThermalUnit_59F", Seq("BTU_59F"), r"1054.804")
  case object LitreAtmosphere extends IntrinsicEnergyUnit("LitreAtmosphere", Seq("l_atm", "ls"), Litre * Atmosphere)
  case object CubicCentiMetreOfAtmosphere extends IntrinsicEnergyUnit("CubicCentiMetreOfAtmosphere", Seq("cc_atm", "scc"), CubicCentiMetre * Atmosphere)
  case object CubicFootOfAtmosphere extends IntrinsicEnergyUnit("CubicFootOfAtmosphere", Seq("cu_ft_atm", "scf"), CubicFoot * Atmosphere)
  case object CubicYardOfAtmosphere extends IntrinsicEnergyUnit("CubicYardOfAtmosphere", Seq("cu_yd_atm", "scy"), CubicYard * Atmosphere)
  case object GallonAtmosphere_imperial extends IntrinsicEnergyUnit("GallonAtmosphere_imperial", Seq("imp_gal_atm"), Gallon_imperial * Atmosphere)
  case object GallonAtmosphere_US extends IntrinsicEnergyUnit("GallonAtmosphere_US", Seq("US_gal_atm"), Gallon_US_fluid * Atmosphere)
  case object TonOfTNT extends IntrinsicEnergyUnit("TonOfTNT", Seq("tTNT"), r"1e9", Calorie_IT)
  case object BarrelOfOilEquivalent extends IntrinsicEnergyUnit("BarrelOfOilEquivalent", Seq("BOE"), r"5.8e6", BritishThermalUnit_59F)
  case object TonneOfOilEquivalent extends IntrinsicEnergyUnit("TonneOfOilEquivalent", Seq("toe"), r"1e10", Calorie_IT)
  case object TonOfCoalEquivalent extends IntrinsicEnergyUnit("TonOfCoalEquivalent", Seq("TCE"), r"7e9", Calorie_thermochemical)
  case object CelsiusHeatUnit extends IntrinsicEnergyUnit("CelsiusHeatUnit", Seq("CHU", "CHU(IT)", "CHU_IT"), r"1.899100534716e3")
  case object Therm_US extends IntrinsicEnergyUnit("Therm_US", Seq("thm", "thm_US"), r"1e5", BritishThermalUnit_59F)
  case object Quad extends IntrinsicEnergyUnit("Quad", Seq("quad"), r"1e15", BritishThermalUnit_IT)

  override lazy val values = Seq(YoctoJoule, ZeptoJoule, AttoJoule, FemtoJoule, PicoJoule, NanoJoule, MicroJoule, MilliJoule, CentiJoule, DeciJoule, Joule, DecaJoule, HectoJoule, KiloJoule, MegaJoule, GigaJoule, TeraJoule, PetaJoule, ExaJoule, ZettaJoule, YottaJoule, Erg, YoctoElectronVolt, ZeptoElectronVolt, AttoElectronVolt, FemtoElectronVolt, PicoElectronVolt, NanoElectronVolt, MicroElectronVolt, MilliElectronVolt, CentiElectronVolt, DeciElectronVolt, ElectronVolt, DecaElectronVolt, HectoElectronVolt, KiloElectronVolt, MegaElectronVolt, GigaElectronVolt, TeraElectronVolt, PetaElectronVolt, ExaElectronVolt, ZettaElectronVolt, YottaElectronVolt, Rydberg, AtomicUnitOfEnergy, Calorie_IT, Calorie_thermochemical, KiloCalorie, Calorie_15C, BritishThermalUnit_ISO, BritishThermalUnit_IT, BritishThermalUnit_thermochemical, BritishThermalUnit_59F, LitreAtmosphere, CubicCentiMetreOfAtmosphere, CubicFootOfAtmosphere, CubicYardOfAtmosphere, GallonAtmosphere_imperial, GallonAtmosphere_US, TonOfTNT, BarrelOfOilEquivalent, TonneOfOilEquivalent, TonOfCoalEquivalent, CelsiusHeatUnit, Therm_US, Quad)

  // LengthUnit * ForceUnit -> Energy
  private[EnergyUnit]
  class ProductLengthDotForceUnit(val firstUnit: LengthUnit, val secondUnit: ForceUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, LengthUnit, ForceUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: LengthUnit, unit2: ForceUnit): EnergyUnit =
    new ProductLengthDotForceUnit(unit1, unit2)

  // ChargeUnit * VoltageUnit -> Energy
  private[EnergyUnit]
  class ProductChargeDotVoltageUnit(val firstUnit: ChargeUnit, val secondUnit: VoltageUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, ChargeUnit, VoltageUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: ChargeUnit, unit2: VoltageUnit): EnergyUnit =
    new ProductChargeDotVoltageUnit(unit1, unit2)

  // PowerUnit * TimeUnit -> Energy
  private[EnergyUnit]
  class ProductPowerDotTimeUnit(val firstUnit: PowerUnit, val secondUnit: TimeUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, PowerUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: PowerUnit, unit2: TimeUnit): EnergyUnit =
    new ProductPowerDotTimeUnit(unit1, unit2)

  // VolumeUnit * PressureUnit -> Energy
  private[EnergyUnit]
  class ProductVolumeDotPressureUnit(val firstUnit: VolumeUnit, val secondUnit: PressureUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, VolumeUnit, PressureUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: VolumeUnit, unit2: PressureUnit): EnergyUnit =
    new ProductVolumeDotPressureUnit(unit1, unit2)
}

trait MultiplicativeByEnergyUnit[R]{
  def *(unit: EnergyUnit): R
}

trait DivisibleByEnergyUnit[R]{
  def /(unit: EnergyUnit): R
}

trait EnergyPostfixOps[A]{
  import EnergyUnit._

  protected def energyPostfixOps(unit: EnergyUnit): A


  def yJ : A = energyPostfixOps(YoctoJoule)
  def zJ : A = energyPostfixOps(ZeptoJoule)
  def aJ : A = energyPostfixOps(AttoJoule)
  def fJ : A = energyPostfixOps(FemtoJoule)
  def pJ : A = energyPostfixOps(PicoJoule)
  def nJ : A = energyPostfixOps(NanoJoule)
  def μJ : A = energyPostfixOps(MicroJoule)
  def mcJ : A = energyPostfixOps(MicroJoule)
  def mJ : A = energyPostfixOps(MilliJoule)
  def cJ : A = energyPostfixOps(CentiJoule)
  def dJ : A = energyPostfixOps(DeciJoule)
  def J : A = energyPostfixOps(Joule)
  def daJ : A = energyPostfixOps(DecaJoule)
  def hJ : A = energyPostfixOps(HectoJoule)
  def kJ : A = energyPostfixOps(KiloJoule)
  def MJ : A = energyPostfixOps(MegaJoule)
  def GJ : A = energyPostfixOps(GigaJoule)
  def TJ : A = energyPostfixOps(TeraJoule)
  def PJ : A = energyPostfixOps(PetaJoule)
  def EJ : A = energyPostfixOps(ExaJoule)
  def ZJ : A = energyPostfixOps(ZettaJoule)
  def YJ : A = energyPostfixOps(YottaJoule)
  def erg : A = energyPostfixOps(Erg)
  def yeV : A = energyPostfixOps(YoctoElectronVolt)
  def zeV : A = energyPostfixOps(ZeptoElectronVolt)
  def aeV : A = energyPostfixOps(AttoElectronVolt)
  def feV : A = energyPostfixOps(FemtoElectronVolt)
  def peV : A = energyPostfixOps(PicoElectronVolt)
  def neV : A = energyPostfixOps(NanoElectronVolt)
  def μeV : A = energyPostfixOps(MicroElectronVolt)
  def mceV : A = energyPostfixOps(MicroElectronVolt)
  def meV : A = energyPostfixOps(MilliElectronVolt)
  def ceV : A = energyPostfixOps(CentiElectronVolt)
  def deV : A = energyPostfixOps(DeciElectronVolt)
  def eV : A = energyPostfixOps(ElectronVolt)
  def daeV : A = energyPostfixOps(DecaElectronVolt)
  def heV : A = energyPostfixOps(HectoElectronVolt)
  def keV : A = energyPostfixOps(KiloElectronVolt)
  def MeV : A = energyPostfixOps(MegaElectronVolt)
  def GeV : A = energyPostfixOps(GigaElectronVolt)
  def TeV : A = energyPostfixOps(TeraElectronVolt)
  def PeV : A = energyPostfixOps(PetaElectronVolt)
  def EeV : A = energyPostfixOps(ExaElectronVolt)
  def ZeV : A = energyPostfixOps(ZettaElectronVolt)
  def YeV : A = energyPostfixOps(YottaElectronVolt)
  def Ry : A = energyPostfixOps(Rydberg)
  def E_h : A = energyPostfixOps(AtomicUnitOfEnergy)
  def cal : A = energyPostfixOps(Calorie_IT)
  def cal_IT : A = energyPostfixOps(Calorie_IT)
  def cal_th : A = energyPostfixOps(Calorie_thermochemical)
  def kcal : A = energyPostfixOps(KiloCalorie)
  def Cal : A = energyPostfixOps(KiloCalorie)
  def cal_15C : A = energyPostfixOps(Calorie_15C)
  def BTU : A = energyPostfixOps(BritishThermalUnit_ISO)
  def BTU_ISO : A = energyPostfixOps(BritishThermalUnit_ISO)
  def BTU_IT : A = energyPostfixOps(BritishThermalUnit_IT)
  def BTU_th : A = energyPostfixOps(BritishThermalUnit_thermochemical)
  def BTU_59F : A = energyPostfixOps(BritishThermalUnit_59F)
  def l_atm : A = energyPostfixOps(LitreAtmosphere)
  def ls : A = energyPostfixOps(LitreAtmosphere)
  def cc_atm : A = energyPostfixOps(CubicCentiMetreOfAtmosphere)
  def scc : A = energyPostfixOps(CubicCentiMetreOfAtmosphere)
  def cu_ft_atm : A = energyPostfixOps(CubicFootOfAtmosphere)
  def scf : A = energyPostfixOps(CubicFootOfAtmosphere)
  def cu_yd_atm : A = energyPostfixOps(CubicYardOfAtmosphere)
  def scy : A = energyPostfixOps(CubicYardOfAtmosphere)
  def imp_gal_atm : A = energyPostfixOps(GallonAtmosphere_imperial)
  def US_gal_atm : A = energyPostfixOps(GallonAtmosphere_US)
  def tTNT : A = energyPostfixOps(TonOfTNT)
  def BOE : A = energyPostfixOps(BarrelOfOilEquivalent)
  def toe : A = energyPostfixOps(TonneOfOilEquivalent)
  def TCE : A = energyPostfixOps(TonOfCoalEquivalent)
  def CHU : A = energyPostfixOps(CelsiusHeatUnit)
  def CHU_IT : A = energyPostfixOps(CelsiusHeatUnit)
  def thm : A = energyPostfixOps(Therm_US)
  def thm_US : A = energyPostfixOps(Therm_US)
  def quad : A = energyPostfixOps(Quad)

  import EnergyPostfixOps._
  import org.waman.multiverse.energy.EnergyContext
  import EnergyContext._

  def cal(c: EnergyContext): A = energyPostfixOps(_cal(c))
  def BTU(c: EnergyContext): A = energyPostfixOps(_BTU(c))
  def CHU(c: EnergyContext): A = energyPostfixOps(_CHU(c))
}

object EnergyPostfixOps{
  import EnergyUnit._
  import org.waman.multiverse.energy.EnergyContext
  import EnergyContext._


  lazy val _CHU : PartialFunction[EnergyContext, EnergyUnit] = {
    case InternationalTable => CelsiusHeatUnit
  }

  lazy val _BTU : PartialFunction[EnergyContext, EnergyUnit] = {
    case InternationalOrganizationForStandardization => BritishThermalUnit_ISO
    case InternationalTable => BritishThermalUnit_IT
    case ThermoChemical => BritishThermalUnit_thermochemical
  }

  lazy val _cal : PartialFunction[EnergyContext, EnergyUnit] = {
    case InternationalTable => Calorie_IT
    case ThermoChemical => Calorie_thermochemical
  }
}

trait EnergyDot[A]{
  import EnergyUnit._

  protected def energyDot(unit: EnergyUnit): A

  def yJ(dot: Dot): A = energyDot(YoctoJoule)
  def zJ(dot: Dot): A = energyDot(ZeptoJoule)
  def aJ(dot: Dot): A = energyDot(AttoJoule)
  def fJ(dot: Dot): A = energyDot(FemtoJoule)
  def pJ(dot: Dot): A = energyDot(PicoJoule)
  def nJ(dot: Dot): A = energyDot(NanoJoule)
  def μJ(dot: Dot): A = energyDot(MicroJoule)
  def mcJ(dot: Dot): A = energyDot(MicroJoule)
  def mJ(dot: Dot): A = energyDot(MilliJoule)
  def cJ(dot: Dot): A = energyDot(CentiJoule)
  def dJ(dot: Dot): A = energyDot(DeciJoule)
  def J(dot: Dot): A = energyDot(Joule)
  def daJ(dot: Dot): A = energyDot(DecaJoule)
  def hJ(dot: Dot): A = energyDot(HectoJoule)
  def kJ(dot: Dot): A = energyDot(KiloJoule)
  def MJ(dot: Dot): A = energyDot(MegaJoule)
  def GJ(dot: Dot): A = energyDot(GigaJoule)
  def TJ(dot: Dot): A = energyDot(TeraJoule)
  def PJ(dot: Dot): A = energyDot(PetaJoule)
  def EJ(dot: Dot): A = energyDot(ExaJoule)
  def ZJ(dot: Dot): A = energyDot(ZettaJoule)
  def YJ(dot: Dot): A = energyDot(YottaJoule)
  def erg(dot: Dot): A = energyDot(Erg)
  def yeV(dot: Dot): A = energyDot(YoctoElectronVolt)
  def zeV(dot: Dot): A = energyDot(ZeptoElectronVolt)
  def aeV(dot: Dot): A = energyDot(AttoElectronVolt)
  def feV(dot: Dot): A = energyDot(FemtoElectronVolt)
  def peV(dot: Dot): A = energyDot(PicoElectronVolt)
  def neV(dot: Dot): A = energyDot(NanoElectronVolt)
  def μeV(dot: Dot): A = energyDot(MicroElectronVolt)
  def mceV(dot: Dot): A = energyDot(MicroElectronVolt)
  def meV(dot: Dot): A = energyDot(MilliElectronVolt)
  def ceV(dot: Dot): A = energyDot(CentiElectronVolt)
  def deV(dot: Dot): A = energyDot(DeciElectronVolt)
  def eV(dot: Dot): A = energyDot(ElectronVolt)
  def daeV(dot: Dot): A = energyDot(DecaElectronVolt)
  def heV(dot: Dot): A = energyDot(HectoElectronVolt)
  def keV(dot: Dot): A = energyDot(KiloElectronVolt)
  def MeV(dot: Dot): A = energyDot(MegaElectronVolt)
  def GeV(dot: Dot): A = energyDot(GigaElectronVolt)
  def TeV(dot: Dot): A = energyDot(TeraElectronVolt)
  def PeV(dot: Dot): A = energyDot(PetaElectronVolt)
  def EeV(dot: Dot): A = energyDot(ExaElectronVolt)
  def ZeV(dot: Dot): A = energyDot(ZettaElectronVolt)
  def YeV(dot: Dot): A = energyDot(YottaElectronVolt)
  def Ry(dot: Dot): A = energyDot(Rydberg)
  def E_h(dot: Dot): A = energyDot(AtomicUnitOfEnergy)
  def cal(dot: Dot): A = energyDot(Calorie_IT)
  def cal_IT(dot: Dot): A = energyDot(Calorie_IT)
  def cal_th(dot: Dot): A = energyDot(Calorie_thermochemical)
  def kcal(dot: Dot): A = energyDot(KiloCalorie)
  def Cal(dot: Dot): A = energyDot(KiloCalorie)
  def cal_15C(dot: Dot): A = energyDot(Calorie_15C)
  def BTU(dot: Dot): A = energyDot(BritishThermalUnit_ISO)
  def BTU_ISO(dot: Dot): A = energyDot(BritishThermalUnit_ISO)
  def BTU_IT(dot: Dot): A = energyDot(BritishThermalUnit_IT)
  def BTU_th(dot: Dot): A = energyDot(BritishThermalUnit_thermochemical)
  def BTU_59F(dot: Dot): A = energyDot(BritishThermalUnit_59F)
  def l_atm(dot: Dot): A = energyDot(LitreAtmosphere)
  def ls(dot: Dot): A = energyDot(LitreAtmosphere)
  def cc_atm(dot: Dot): A = energyDot(CubicCentiMetreOfAtmosphere)
  def scc(dot: Dot): A = energyDot(CubicCentiMetreOfAtmosphere)
  def cu_ft_atm(dot: Dot): A = energyDot(CubicFootOfAtmosphere)
  def scf(dot: Dot): A = energyDot(CubicFootOfAtmosphere)
  def cu_yd_atm(dot: Dot): A = energyDot(CubicYardOfAtmosphere)
  def scy(dot: Dot): A = energyDot(CubicYardOfAtmosphere)
  def imp_gal_atm(dot: Dot): A = energyDot(GallonAtmosphere_imperial)
  def US_gal_atm(dot: Dot): A = energyDot(GallonAtmosphere_US)
  def tTNT(dot: Dot): A = energyDot(TonOfTNT)
  def BOE(dot: Dot): A = energyDot(BarrelOfOilEquivalent)
  def toe(dot: Dot): A = energyDot(TonneOfOilEquivalent)
  def TCE(dot: Dot): A = energyDot(TonOfCoalEquivalent)
  def CHU(dot: Dot): A = energyDot(CelsiusHeatUnit)
  def CHU_IT(dot: Dot): A = energyDot(CelsiusHeatUnit)
  def thm(dot: Dot): A = energyDot(Therm_US)
  def thm_US(dot: Dot): A = energyDot(Therm_US)
  def quad(dot: Dot): A = energyDot(Quad)
}

trait EnergyPer[A]{
  import EnergyUnit._

  protected def energyPer(unit: EnergyUnit): A

  def yJ(per: Per): A = energyPer(YoctoJoule)
  def zJ(per: Per): A = energyPer(ZeptoJoule)
  def aJ(per: Per): A = energyPer(AttoJoule)
  def fJ(per: Per): A = energyPer(FemtoJoule)
  def pJ(per: Per): A = energyPer(PicoJoule)
  def nJ(per: Per): A = energyPer(NanoJoule)
  def μJ(per: Per): A = energyPer(MicroJoule)
  def mcJ(per: Per): A = energyPer(MicroJoule)
  def mJ(per: Per): A = energyPer(MilliJoule)
  def cJ(per: Per): A = energyPer(CentiJoule)
  def dJ(per: Per): A = energyPer(DeciJoule)
  def J(per: Per): A = energyPer(Joule)
  def daJ(per: Per): A = energyPer(DecaJoule)
  def hJ(per: Per): A = energyPer(HectoJoule)
  def kJ(per: Per): A = energyPer(KiloJoule)
  def MJ(per: Per): A = energyPer(MegaJoule)
  def GJ(per: Per): A = energyPer(GigaJoule)
  def TJ(per: Per): A = energyPer(TeraJoule)
  def PJ(per: Per): A = energyPer(PetaJoule)
  def EJ(per: Per): A = energyPer(ExaJoule)
  def ZJ(per: Per): A = energyPer(ZettaJoule)
  def YJ(per: Per): A = energyPer(YottaJoule)
  def erg(per: Per): A = energyPer(Erg)
  def yeV(per: Per): A = energyPer(YoctoElectronVolt)
  def zeV(per: Per): A = energyPer(ZeptoElectronVolt)
  def aeV(per: Per): A = energyPer(AttoElectronVolt)
  def feV(per: Per): A = energyPer(FemtoElectronVolt)
  def peV(per: Per): A = energyPer(PicoElectronVolt)
  def neV(per: Per): A = energyPer(NanoElectronVolt)
  def μeV(per: Per): A = energyPer(MicroElectronVolt)
  def mceV(per: Per): A = energyPer(MicroElectronVolt)
  def meV(per: Per): A = energyPer(MilliElectronVolt)
  def ceV(per: Per): A = energyPer(CentiElectronVolt)
  def deV(per: Per): A = energyPer(DeciElectronVolt)
  def eV(per: Per): A = energyPer(ElectronVolt)
  def daeV(per: Per): A = energyPer(DecaElectronVolt)
  def heV(per: Per): A = energyPer(HectoElectronVolt)
  def keV(per: Per): A = energyPer(KiloElectronVolt)
  def MeV(per: Per): A = energyPer(MegaElectronVolt)
  def GeV(per: Per): A = energyPer(GigaElectronVolt)
  def TeV(per: Per): A = energyPer(TeraElectronVolt)
  def PeV(per: Per): A = energyPer(PetaElectronVolt)
  def EeV(per: Per): A = energyPer(ExaElectronVolt)
  def ZeV(per: Per): A = energyPer(ZettaElectronVolt)
  def YeV(per: Per): A = energyPer(YottaElectronVolt)
  def Ry(per: Per): A = energyPer(Rydberg)
  def E_h(per: Per): A = energyPer(AtomicUnitOfEnergy)
  def cal(per: Per): A = energyPer(Calorie_IT)
  def cal_IT(per: Per): A = energyPer(Calorie_IT)
  def cal_th(per: Per): A = energyPer(Calorie_thermochemical)
  def kcal(per: Per): A = energyPer(KiloCalorie)
  def Cal(per: Per): A = energyPer(KiloCalorie)
  def cal_15C(per: Per): A = energyPer(Calorie_15C)
  def BTU(per: Per): A = energyPer(BritishThermalUnit_ISO)
  def BTU_ISO(per: Per): A = energyPer(BritishThermalUnit_ISO)
  def BTU_IT(per: Per): A = energyPer(BritishThermalUnit_IT)
  def BTU_th(per: Per): A = energyPer(BritishThermalUnit_thermochemical)
  def BTU_59F(per: Per): A = energyPer(BritishThermalUnit_59F)
  def l_atm(per: Per): A = energyPer(LitreAtmosphere)
  def ls(per: Per): A = energyPer(LitreAtmosphere)
  def cc_atm(per: Per): A = energyPer(CubicCentiMetreOfAtmosphere)
  def scc(per: Per): A = energyPer(CubicCentiMetreOfAtmosphere)
  def cu_ft_atm(per: Per): A = energyPer(CubicFootOfAtmosphere)
  def scf(per: Per): A = energyPer(CubicFootOfAtmosphere)
  def cu_yd_atm(per: Per): A = energyPer(CubicYardOfAtmosphere)
  def scy(per: Per): A = energyPer(CubicYardOfAtmosphere)
  def imp_gal_atm(per: Per): A = energyPer(GallonAtmosphere_imperial)
  def US_gal_atm(per: Per): A = energyPer(GallonAtmosphere_US)
  def tTNT(per: Per): A = energyPer(TonOfTNT)
  def BOE(per: Per): A = energyPer(BarrelOfOilEquivalent)
  def toe(per: Per): A = energyPer(TonneOfOilEquivalent)
  def TCE(per: Per): A = energyPer(TonOfCoalEquivalent)
  def CHU(per: Per): A = energyPer(CelsiusHeatUnit)
  def CHU_IT(per: Per): A = energyPer(CelsiusHeatUnit)
  def thm(per: Per): A = energyPer(Therm_US)
  def thm_US(per: Per): A = energyPer(Therm_US)
  def quad(per: Per): A = energyPer(Quad)
}

trait PredefinedEnergyUnit extends EnergyPostfixOps[EnergyUnit]{
  override protected def energyPostfixOps(unit: EnergyUnit) = unit
  
}

object PredefinedEnergyUnit extends PredefinedEnergyUnit
