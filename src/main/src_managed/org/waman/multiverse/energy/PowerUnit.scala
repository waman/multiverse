package org.waman.multiverse.energy

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.time._
import org.waman.multiverse.electric._
import org.waman.multiverse.time.TimeUnit._
import org.waman.multiverse.metric.LengthUnit._
import org.waman.multiverse.metric.VolumeUnit._
import org.waman.multiverse.mechanics.ForceUnit._
import org.waman.multiverse.fluid.PressureUnit._
import org.waman.multiverse.energy.EnergyUnit._

sealed trait PowerUnit extends PhysicalUnit[PowerUnit]
  with MultiplicativeByTimeUnit[EnergyUnit]
  with DivisibleByCurrentUnit[VoltageUnit]{

  override def getSIUnit = org.waman.multiverse.energy.PowerUnit.Watt

  override def *(unit: TimeUnit) = EnergyUnit(this, unit)

  override def /(unit: CurrentUnit) = VoltageUnit(this, unit)
}

object PowerUnit extends ConstantsDefined[PowerUnit]{

  // intrinsic
  private[PowerUnit]
  class IntrinsicPowerUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends PowerUnit{

    def this(name: String, symbols: Seq[String], unit: PowerUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: PowerUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoWatt extends IntrinsicPowerUnit("YoctoWatt", Seq("yW"), r"1e-24")
  case object ZeptoWatt extends IntrinsicPowerUnit("ZeptoWatt", Seq("zW"), r"1e-21")
  case object AttoWatt extends IntrinsicPowerUnit("AttoWatt", Seq("aW"), r"1e-18")
  case object FemtoWatt extends IntrinsicPowerUnit("FemtoWatt", Seq("fW"), r"1e-15")
  case object PicoWatt extends IntrinsicPowerUnit("PicoWatt", Seq("pW"), r"1e-12")
  case object NanoWatt extends IntrinsicPowerUnit("NanoWatt", Seq("nW"), r"1e-9")
  case object MicroWatt extends IntrinsicPowerUnit("MicroWatt", Seq("μW", "mcW"), r"1e-6")
  case object MilliWatt extends IntrinsicPowerUnit("MilliWatt", Seq("mW"), r"1e-3")
  case object CentiWatt extends IntrinsicPowerUnit("CentiWatt", Seq("cW"), r"1e-2")
  case object DeciWatt extends IntrinsicPowerUnit("DeciWatt", Seq("dW"), r"1e-1")
  case object Watt extends IntrinsicPowerUnit("Watt", Seq("W"), r"1")
  case object DecaWatt extends IntrinsicPowerUnit("DecaWatt", Seq("daW"), r"1e1")
  case object HectoWatt extends IntrinsicPowerUnit("HectoWatt", Seq("hW"), r"1e2")
  case object KiloWatt extends IntrinsicPowerUnit("KiloWatt", Seq("kW"), r"1e3")
  case object MegaWatt extends IntrinsicPowerUnit("MegaWatt", Seq("MW"), r"1e6")
  case object GigaWatt extends IntrinsicPowerUnit("GigaWatt", Seq("GW"), r"1e9")
  case object TeraWatt extends IntrinsicPowerUnit("TeraWatt", Seq("TW"), r"1e12")
  case object PetaWatt extends IntrinsicPowerUnit("PetaWatt", Seq("PW"), r"1e15")
  case object ExaWatt extends IntrinsicPowerUnit("ExaWatt", Seq("EW"), r"1e18")
  case object ZettaWatt extends IntrinsicPowerUnit("ZettaWatt", Seq("ZW"), r"1e21")
  case object YottaWatt extends IntrinsicPowerUnit("YottaWatt", Seq("YW"), r"1e24")
  case object Horsepower_boiler extends IntrinsicPowerUnit("Horsepower_boiler", Seq("hp(boiler)"), r"9812.5")
  case object Horsepower_electrical extends IntrinsicPowerUnit("Horsepower_electrical", Seq("hp(electrical)"), 746)
  case object Horsepower_mechanical extends IntrinsicPowerUnit("Horsepower_mechanical", Seq("hp(mechanical)"), 550, Foot * PoundForce / Second)
  case object Horsepower_metric extends IntrinsicPowerUnit("Horsepower_metric", Seq("hp(metric)", "PS"), 75, Metre * KiloGramForce / Second)
  case object Poncelet extends IntrinsicPowerUnit("Poncelet", Seq("p"), 100, Metre * KiloGramForce / Second)
  case object Lusec extends IntrinsicPowerUnit("Lusec", Seq("lusec"), Litre * MicroMetreOfMercury / Second)
  case object SquareFootEquivalentDirectRadiation extends IntrinsicPowerUnit("SquareFootEquivalentDirectRadiation", Seq("sq_ft_EDR"), 240, BritishThermalUnit_IT / Hour)

  override lazy val values = Seq(YoctoWatt, ZeptoWatt, AttoWatt, FemtoWatt, PicoWatt, NanoWatt, MicroWatt, MilliWatt, CentiWatt, DeciWatt, Watt, DecaWatt, HectoWatt, KiloWatt, MegaWatt, GigaWatt, TeraWatt, PetaWatt, ExaWatt, ZettaWatt, YottaWatt, Horsepower_boiler, Horsepower_electrical, Horsepower_mechanical, Horsepower_metric, Poncelet, Lusec, SquareFootEquivalentDirectRadiation)

  // EnergyUnit / TimeUnit -> Power
  private[PowerUnit]
  class QuotientEnergyPerTimeUnit(val numeratorUnit: EnergyUnit, val denominatorUnit: TimeUnit)
      extends PowerUnit with QuotientUnit[PowerUnit, EnergyUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: EnergyUnit, dUnit: TimeUnit): PowerUnit =
    new QuotientEnergyPerTimeUnit(nUnit, dUnit)
}

trait MultiplicativeByPowerUnit[R]{
  def *(unit: PowerUnit): R
}

trait DivisibleByPowerUnit[R]{
  def /(unit: PowerUnit): R
}

trait PowerPostfixOps[A]{
  import PowerUnit._

  protected def powerPostfixOps(unit: PowerUnit): A


  def yW : A = powerPostfixOps(YoctoWatt)
  def zW : A = powerPostfixOps(ZeptoWatt)
  def aW : A = powerPostfixOps(AttoWatt)
  def fW : A = powerPostfixOps(FemtoWatt)
  def pW : A = powerPostfixOps(PicoWatt)
  def nW : A = powerPostfixOps(NanoWatt)
  def μW : A = powerPostfixOps(MicroWatt)
  def mcW : A = powerPostfixOps(MicroWatt)
  def mW : A = powerPostfixOps(MilliWatt)
  def cW : A = powerPostfixOps(CentiWatt)
  def dW : A = powerPostfixOps(DeciWatt)
  def W : A = powerPostfixOps(Watt)
  def daW : A = powerPostfixOps(DecaWatt)
  def hW : A = powerPostfixOps(HectoWatt)
  def kW : A = powerPostfixOps(KiloWatt)
  def MW : A = powerPostfixOps(MegaWatt)
  def GW : A = powerPostfixOps(GigaWatt)
  def TW : A = powerPostfixOps(TeraWatt)
  def PW : A = powerPostfixOps(PetaWatt)
  def EW : A = powerPostfixOps(ExaWatt)
  def ZW : A = powerPostfixOps(ZettaWatt)
  def YW : A = powerPostfixOps(YottaWatt)
  def PS : A = powerPostfixOps(Horsepower_metric)
  def p : A = powerPostfixOps(Poncelet)
  def lusec : A = powerPostfixOps(Lusec)
  def sq_ft_EDR : A = powerPostfixOps(SquareFootEquivalentDirectRadiation)

  import PowerPostfixOps._
  import org.waman.multiverse.energy.EnergyContext
  import EnergyContext._

  def hp(c: EnergyContext): A = powerPostfixOps(_hp(c))
}

object PowerPostfixOps{
  import PowerUnit._
  import org.waman.multiverse.energy.EnergyContext
  import EnergyContext._


  lazy val _hp : PartialFunction[EnergyContext, PowerUnit] = {
    case Boiler => Horsepower_boiler
    case Electrical => Horsepower_electrical
    case Mechanical => Horsepower_mechanical
    case Metric => Horsepower_metric
  }
}

trait PowerDot[A]{
  import PowerUnit._

  protected def powerDot(unit: PowerUnit): A

  def yW(dot: Dot): A = powerDot(YoctoWatt)
  def zW(dot: Dot): A = powerDot(ZeptoWatt)
  def aW(dot: Dot): A = powerDot(AttoWatt)
  def fW(dot: Dot): A = powerDot(FemtoWatt)
  def pW(dot: Dot): A = powerDot(PicoWatt)
  def nW(dot: Dot): A = powerDot(NanoWatt)
  def μW(dot: Dot): A = powerDot(MicroWatt)
  def mcW(dot: Dot): A = powerDot(MicroWatt)
  def mW(dot: Dot): A = powerDot(MilliWatt)
  def cW(dot: Dot): A = powerDot(CentiWatt)
  def dW(dot: Dot): A = powerDot(DeciWatt)
  def W(dot: Dot): A = powerDot(Watt)
  def daW(dot: Dot): A = powerDot(DecaWatt)
  def hW(dot: Dot): A = powerDot(HectoWatt)
  def kW(dot: Dot): A = powerDot(KiloWatt)
  def MW(dot: Dot): A = powerDot(MegaWatt)
  def GW(dot: Dot): A = powerDot(GigaWatt)
  def TW(dot: Dot): A = powerDot(TeraWatt)
  def PW(dot: Dot): A = powerDot(PetaWatt)
  def EW(dot: Dot): A = powerDot(ExaWatt)
  def ZW(dot: Dot): A = powerDot(ZettaWatt)
  def YW(dot: Dot): A = powerDot(YottaWatt)
  def PS(dot: Dot): A = powerDot(Horsepower_metric)
  def p(dot: Dot): A = powerDot(Poncelet)
  def lusec(dot: Dot): A = powerDot(Lusec)
  def sq_ft_EDR(dot: Dot): A = powerDot(SquareFootEquivalentDirectRadiation)
}

trait PowerPer[A]{
  import PowerUnit._

  protected def powerPer(unit: PowerUnit): A

  def yW(per: Per): A = powerPer(YoctoWatt)
  def zW(per: Per): A = powerPer(ZeptoWatt)
  def aW(per: Per): A = powerPer(AttoWatt)
  def fW(per: Per): A = powerPer(FemtoWatt)
  def pW(per: Per): A = powerPer(PicoWatt)
  def nW(per: Per): A = powerPer(NanoWatt)
  def μW(per: Per): A = powerPer(MicroWatt)
  def mcW(per: Per): A = powerPer(MicroWatt)
  def mW(per: Per): A = powerPer(MilliWatt)
  def cW(per: Per): A = powerPer(CentiWatt)
  def dW(per: Per): A = powerPer(DeciWatt)
  def W(per: Per): A = powerPer(Watt)
  def daW(per: Per): A = powerPer(DecaWatt)
  def hW(per: Per): A = powerPer(HectoWatt)
  def kW(per: Per): A = powerPer(KiloWatt)
  def MW(per: Per): A = powerPer(MegaWatt)
  def GW(per: Per): A = powerPer(GigaWatt)
  def TW(per: Per): A = powerPer(TeraWatt)
  def PW(per: Per): A = powerPer(PetaWatt)
  def EW(per: Per): A = powerPer(ExaWatt)
  def ZW(per: Per): A = powerPer(ZettaWatt)
  def YW(per: Per): A = powerPer(YottaWatt)
  def PS(per: Per): A = powerPer(Horsepower_metric)
  def p(per: Per): A = powerPer(Poncelet)
  def lusec(per: Per): A = powerPer(Lusec)
  def sq_ft_EDR(per: Per): A = powerPer(SquareFootEquivalentDirectRadiation)
}

trait PredefinedPowerUnit extends PowerPostfixOps[PowerUnit]{
  override protected def powerPostfixOps(unit: PowerUnit) = unit
  
}

object PredefinedPowerUnit extends PredefinedPowerUnit
