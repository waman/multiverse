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

sealed trait EnergyUnit extends PhysicalUnit[EnergyUnit]
  with MultiplicativeByTimeUnit[ActionUnit]
  with DivisibleByTimeUnit[PowerUnit]
  with DivisibleByTemperatureUnit[EntropyUnit]
  with DivisibleByMassUnit[AbsorbedDoseUnit]{

  def unitInJoule: Real

  override def baseUnit = org.waman.multiverse.energy.EnergyUnit.Joule
  override def valueInBaseUnit = unitInJoule

  override def *(unit: TimeUnit) = ActionUnit(this, unit)

  override def /(unit: TimeUnit) = PowerUnit(this, unit)

  override def /(unit: TemperatureUnit) = EntropyUnit(this, unit)

  override def /(unit: MassUnit) = AbsorbedDoseUnit(this, unit)
}

object EnergyUnit extends ConstantsDefined[EnergyUnit]{

  // intrinsic
  private[EnergyUnit]
  class IntrinsicEnergyUnit(name: String, val symbols: Seq[String], val unitInJoule: Real)
      extends EnergyUnit{

    def this(name: String, symbols: Seq[String], unit: EnergyUnit) =
      this(name, symbols, unit.unitInJoule)

    def this(name: String, symbols: Seq[String], factor: Real, unit: EnergyUnit) =
      this(name, symbols, factor * unit.unitInJoule)
  }


  case object YoctoJoule extends IntrinsicEnergyUnit("YoctoJoule", Seq("yJ"), r"1e-24")
  case object ZeptoJoule extends IntrinsicEnergyUnit("ZeptoJoule", Seq("zJ"), r"1e-21")
  case object AttoJoule extends IntrinsicEnergyUnit("AttoJoule", Seq("aJ"), r"1e-18")
  case object FemtoJoule extends IntrinsicEnergyUnit("FemtoJoule", Seq("fJ"), r"1e-15")
  case object PicoJoule extends IntrinsicEnergyUnit("PicoJoule", Seq("pJ"), r"1e-12")
  case object NanoJoule extends IntrinsicEnergyUnit("NanoJoule", Seq("nJ"), r"1e-9")
  case object MicroJoule extends IntrinsicEnergyUnit("MicroJoule", Seq("microJoule", "microJ", "μJ"), r"1e-6")
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
  case object YoctoElectronVolt extends IntrinsicEnergyUnit("YoctoElectronVolt", Seq("yeV"), r"1e-24" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object ZeptoElectronVolt extends IntrinsicEnergyUnit("ZeptoElectronVolt", Seq("zeV"), r"1e-21" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object AttoElectronVolt extends IntrinsicEnergyUnit("AttoElectronVolt", Seq("aeV"), r"1e-18" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object FemtoElectronVolt extends IntrinsicEnergyUnit("FemtoElectronVolt", Seq("feV"), r"1e-15" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object PicoElectronVolt extends IntrinsicEnergyUnit("PicoElectronVolt", Seq("peV"), r"1e-12" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object NanoElectronVolt extends IntrinsicEnergyUnit("NanoElectronVolt", Seq("neV"), r"1e-9" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object MicroElectronVolt extends IntrinsicEnergyUnit("MicroElectronVolt", Seq("microElectronVolt", "microEV", "μeV"), r"1e-6" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object MilliElectronVolt extends IntrinsicEnergyUnit("MilliElectronVolt", Seq("meV"), r"1e-3" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object CentiElectronVolt extends IntrinsicEnergyUnit("CentiElectronVolt", Seq("ceV"), r"1e-2" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object DeciElectronVolt extends IntrinsicEnergyUnit("DeciElectronVolt", Seq("deV"), r"1e-1" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object ElectronVolt extends IntrinsicEnergyUnit("ElectronVolt", Seq("eV"), r"1" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object DecaElectronVolt extends IntrinsicEnergyUnit("DecaElectronVolt", Seq("daeV"), r"1e1" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object HectoElectronVolt extends IntrinsicEnergyUnit("HectoElectronVolt", Seq("heV"), r"1e2" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object KiloElectronVolt extends IntrinsicEnergyUnit("KiloElectronVolt", Seq("keV"), r"1e3" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object MegaElectronVolt extends IntrinsicEnergyUnit("MegaElectronVolt", Seq("MeV"), r"1e6" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object GigaElectronVolt extends IntrinsicEnergyUnit("GigaElectronVolt", Seq("GeV"), r"1e9" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object TeraElectronVolt extends IntrinsicEnergyUnit("TeraElectronVolt", Seq("TeV"), r"1e12" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object PetaElectronVolt extends IntrinsicEnergyUnit("PetaElectronVolt", Seq("PeV"), r"1e15" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object ExaElectronVolt extends IntrinsicEnergyUnit("ExaElectronVolt", Seq("EeV"), r"1e18" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object ZettaElectronVolt extends IntrinsicEnergyUnit("ZettaElectronVolt", Seq("ZeV"), r"1e21" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact
  case object YottaElectronVolt extends IntrinsicEnergyUnit("YottaElectronVolt", Seq("YeV"), r"1e24" * ChargeUnit.ElementaryCharge.unitInCoulomb) with NotExact

  override lazy val values = Seq(YoctoJoule, ZeptoJoule, AttoJoule, FemtoJoule, PicoJoule, NanoJoule, MicroJoule, MilliJoule, CentiJoule, DeciJoule, Joule, DecaJoule, HectoJoule, KiloJoule, MegaJoule, GigaJoule, TeraJoule, PetaJoule, ExaJoule, ZettaJoule, YottaJoule, YoctoElectronVolt, ZeptoElectronVolt, AttoElectronVolt, FemtoElectronVolt, PicoElectronVolt, NanoElectronVolt, MicroElectronVolt, MilliElectronVolt, CentiElectronVolt, DeciElectronVolt, ElectronVolt, DecaElectronVolt, HectoElectronVolt, KiloElectronVolt, MegaElectronVolt, GigaElectronVolt, TeraElectronVolt, PetaElectronVolt, ExaElectronVolt, ZettaElectronVolt, YottaElectronVolt)

  // LengthUnit * ForceUnit -> Energy
  private[EnergyUnit]
  class ProductLengthDotForceUnit(val firstUnit: LengthUnit, val secondUnit: ForceUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, LengthUnit, ForceUnit]{

    override lazy val unitInJoule: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: LengthUnit, unit2: ForceUnit): EnergyUnit =
    new ProductLengthDotForceUnit(unit1, unit2)

  // ChargeUnit * VoltageUnit -> Energy
  private[EnergyUnit]
  class ProductChargeDotVoltageUnit(val firstUnit: ChargeUnit, val secondUnit: VoltageUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, ChargeUnit, VoltageUnit]{

    override lazy val unitInJoule: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: ChargeUnit, unit2: VoltageUnit): EnergyUnit =
    new ProductChargeDotVoltageUnit(unit1, unit2)

  // PowerUnit * TimeUnit -> Energy
  private[EnergyUnit]
  class ProductPowerDotTimeUnit(val firstUnit: PowerUnit, val secondUnit: TimeUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, PowerUnit, TimeUnit]{

    override lazy val unitInJoule: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: PowerUnit, unit2: TimeUnit): EnergyUnit =
    new ProductPowerDotTimeUnit(unit1, unit2)

  // VolumeUnit * PressureUnit -> Energy
  private[EnergyUnit]
  class ProductVolumeDotPressureUnit(val firstUnit: VolumeUnit, val secondUnit: PressureUnit)
      extends EnergyUnit with ProductUnit[EnergyUnit, VolumeUnit, PressureUnit]{

    override lazy val unitInJoule: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
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
  def microJoule : A = energyPostfixOps(MicroJoule)
  def microJ : A = energyPostfixOps(MicroJoule)
  def μJ : A = energyPostfixOps(MicroJoule)
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
  def yeV : A = energyPostfixOps(YoctoElectronVolt)
  def zeV : A = energyPostfixOps(ZeptoElectronVolt)
  def aeV : A = energyPostfixOps(AttoElectronVolt)
  def feV : A = energyPostfixOps(FemtoElectronVolt)
  def peV : A = energyPostfixOps(PicoElectronVolt)
  def neV : A = energyPostfixOps(NanoElectronVolt)
  def microElectronVolt : A = energyPostfixOps(MicroElectronVolt)
  def microEV : A = energyPostfixOps(MicroElectronVolt)
  def μeV : A = energyPostfixOps(MicroElectronVolt)
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
  def microJoule(dot: Dot): A = energyDot(MicroJoule)
  def microJ(dot: Dot): A = energyDot(MicroJoule)
  def μJ(dot: Dot): A = energyDot(MicroJoule)
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
  def yeV(dot: Dot): A = energyDot(YoctoElectronVolt)
  def zeV(dot: Dot): A = energyDot(ZeptoElectronVolt)
  def aeV(dot: Dot): A = energyDot(AttoElectronVolt)
  def feV(dot: Dot): A = energyDot(FemtoElectronVolt)
  def peV(dot: Dot): A = energyDot(PicoElectronVolt)
  def neV(dot: Dot): A = energyDot(NanoElectronVolt)
  def microElectronVolt(dot: Dot): A = energyDot(MicroElectronVolt)
  def microEV(dot: Dot): A = energyDot(MicroElectronVolt)
  def μeV(dot: Dot): A = energyDot(MicroElectronVolt)
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
  def microJoule(per: Per): A = energyPer(MicroJoule)
  def microJ(per: Per): A = energyPer(MicroJoule)
  def μJ(per: Per): A = energyPer(MicroJoule)
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
  def yeV(per: Per): A = energyPer(YoctoElectronVolt)
  def zeV(per: Per): A = energyPer(ZeptoElectronVolt)
  def aeV(per: Per): A = energyPer(AttoElectronVolt)
  def feV(per: Per): A = energyPer(FemtoElectronVolt)
  def peV(per: Per): A = energyPer(PicoElectronVolt)
  def neV(per: Per): A = energyPer(NanoElectronVolt)
  def microElectronVolt(per: Per): A = energyPer(MicroElectronVolt)
  def microEV(per: Per): A = energyPer(MicroElectronVolt)
  def μeV(per: Per): A = energyPer(MicroElectronVolt)
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
}

trait PredefinedEnergyUnit extends EnergyPostfixOps[EnergyUnit]{
  override protected def energyPostfixOps(unit: EnergyUnit) = unit
  
}

object PredefinedEnergyUnit extends PredefinedEnergyUnit
