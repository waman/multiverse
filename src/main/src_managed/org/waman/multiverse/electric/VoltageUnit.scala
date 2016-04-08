package org.waman.multiverse.electric

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.time._
import org.waman.multiverse.energy._
import org.waman.multiverse.magnetic._
import org.waman.multiverse.mechanics.VelocityUnit.SpeedOfLight

sealed trait VoltageUnit extends PhysicalUnit[VoltageUnit]
  with MultiplicativeByTimeUnit[FluxUnit]
  with DivisibleByCurrentUnit[ResistanceUnit]{

  override def getSIUnit = org.waman.multiverse.electric.VoltageUnit.Volt

  override def *(unit: TimeUnit) = FluxUnit(this, unit)

  override def /(unit: CurrentUnit) = ResistanceUnit(this, unit)
}

object VoltageUnit extends ConstantsDefined[VoltageUnit]{

  // intrinsic
  private[VoltageUnit]
  class IntrinsicVoltageUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends VoltageUnit{

    def this(name: String, symbols: Seq[String], unit: VoltageUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: VoltageUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoVolt extends IntrinsicVoltageUnit("YoctoVolt", Seq("yV"), r"1e-24")
  case object ZeptoVolt extends IntrinsicVoltageUnit("ZeptoVolt", Seq("zV"), r"1e-21")
  case object AttoVolt extends IntrinsicVoltageUnit("AttoVolt", Seq("aV"), r"1e-18")
  case object FemtoVolt extends IntrinsicVoltageUnit("FemtoVolt", Seq("fV"), r"1e-15")
  case object PicoVolt extends IntrinsicVoltageUnit("PicoVolt", Seq("pV"), r"1e-12")
  case object NanoVolt extends IntrinsicVoltageUnit("NanoVolt", Seq("nV"), r"1e-9")
  case object MicroVolt extends IntrinsicVoltageUnit("MicroVolt", Seq("μV", "mcV"), r"1e-6")
  case object MilliVolt extends IntrinsicVoltageUnit("MilliVolt", Seq("mV"), r"1e-3")
  case object CentiVolt extends IntrinsicVoltageUnit("CentiVolt", Seq("cV"), r"1e-2")
  case object DeciVolt extends IntrinsicVoltageUnit("DeciVolt", Seq("dV"), r"1e-1")
  case object Volt extends IntrinsicVoltageUnit("Volt", Seq("V"), r"1")
  case object DecaVolt extends IntrinsicVoltageUnit("DecaVolt", Seq("daV"), r"1e1")
  case object HectoVolt extends IntrinsicVoltageUnit("HectoVolt", Seq("hV"), r"1e2")
  case object KiloVolt extends IntrinsicVoltageUnit("KiloVolt", Seq("kV"), r"1e3")
  case object MegaVolt extends IntrinsicVoltageUnit("MegaVolt", Seq("MV"), r"1e6")
  case object GigaVolt extends IntrinsicVoltageUnit("GigaVolt", Seq("GV"), r"1e9")
  case object TeraVolt extends IntrinsicVoltageUnit("TeraVolt", Seq("TV"), r"1e12")
  case object PetaVolt extends IntrinsicVoltageUnit("PetaVolt", Seq("PV"), r"1e15")
  case object ExaVolt extends IntrinsicVoltageUnit("ExaVolt", Seq("EV"), r"1e18")
  case object ZettaVolt extends IntrinsicVoltageUnit("ZettaVolt", Seq("ZV"), r"1e21")
  case object YottaVolt extends IntrinsicVoltageUnit("YottaVolt", Seq("YV"), r"1e24")
  case object statVolt extends IntrinsicVoltageUnit("statVolt", Seq("statV"), SpeedOfLight.unitValueInSIUnit / r"1e6")
  case object Abvolt extends IntrinsicVoltageUnit("Abvolt", Seq("abV"), r"1e-8")

  override lazy val values = Seq(YoctoVolt, ZeptoVolt, AttoVolt, FemtoVolt, PicoVolt, NanoVolt, MicroVolt, MilliVolt, CentiVolt, DeciVolt, Volt, DecaVolt, HectoVolt, KiloVolt, MegaVolt, GigaVolt, TeraVolt, PetaVolt, ExaVolt, ZettaVolt, YottaVolt, statVolt, Abvolt)

  // PowerUnit / CurrentUnit -> Voltage
  private[VoltageUnit]
  class QuotientPowerPerCurrentUnit(val numeratorUnit: PowerUnit, val denominatorUnit: CurrentUnit)
      extends VoltageUnit with QuotientUnit[VoltageUnit, PowerUnit, CurrentUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: PowerUnit, dUnit: CurrentUnit): VoltageUnit =
    new QuotientPowerPerCurrentUnit(nUnit, dUnit)
}

trait MultiplicativeByVoltageUnit[R]{
  def *(unit: VoltageUnit): R
}

trait DivisibleByVoltageUnit[R]{
  def /(unit: VoltageUnit): R
}

trait VoltagePostfixOps[A]{
  import VoltageUnit._

  protected def voltagePostfixOps(unit: VoltageUnit): A


  def yV : A = voltagePostfixOps(YoctoVolt)
  def zV : A = voltagePostfixOps(ZeptoVolt)
  def aV : A = voltagePostfixOps(AttoVolt)
  def fV : A = voltagePostfixOps(FemtoVolt)
  def pV : A = voltagePostfixOps(PicoVolt)
  def nV : A = voltagePostfixOps(NanoVolt)
  def μV : A = voltagePostfixOps(MicroVolt)
  def mcV : A = voltagePostfixOps(MicroVolt)
  def mV : A = voltagePostfixOps(MilliVolt)
  def cV : A = voltagePostfixOps(CentiVolt)
  def dV : A = voltagePostfixOps(DeciVolt)
  def V : A = voltagePostfixOps(Volt)
  def daV : A = voltagePostfixOps(DecaVolt)
  def hV : A = voltagePostfixOps(HectoVolt)
  def kV : A = voltagePostfixOps(KiloVolt)
  def MV : A = voltagePostfixOps(MegaVolt)
  def GV : A = voltagePostfixOps(GigaVolt)
  def TV : A = voltagePostfixOps(TeraVolt)
  def PV : A = voltagePostfixOps(PetaVolt)
  def EV : A = voltagePostfixOps(ExaVolt)
  def ZV : A = voltagePostfixOps(ZettaVolt)
  def YV : A = voltagePostfixOps(YottaVolt)
  def statV : A = voltagePostfixOps(statVolt)
  def abV : A = voltagePostfixOps(Abvolt)
}

trait VoltageDot[A]{
  import VoltageUnit._

  protected def voltageDot(unit: VoltageUnit): A

  def yV(dot: Dot): A = voltageDot(YoctoVolt)
  def zV(dot: Dot): A = voltageDot(ZeptoVolt)
  def aV(dot: Dot): A = voltageDot(AttoVolt)
  def fV(dot: Dot): A = voltageDot(FemtoVolt)
  def pV(dot: Dot): A = voltageDot(PicoVolt)
  def nV(dot: Dot): A = voltageDot(NanoVolt)
  def μV(dot: Dot): A = voltageDot(MicroVolt)
  def mcV(dot: Dot): A = voltageDot(MicroVolt)
  def mV(dot: Dot): A = voltageDot(MilliVolt)
  def cV(dot: Dot): A = voltageDot(CentiVolt)
  def dV(dot: Dot): A = voltageDot(DeciVolt)
  def V(dot: Dot): A = voltageDot(Volt)
  def daV(dot: Dot): A = voltageDot(DecaVolt)
  def hV(dot: Dot): A = voltageDot(HectoVolt)
  def kV(dot: Dot): A = voltageDot(KiloVolt)
  def MV(dot: Dot): A = voltageDot(MegaVolt)
  def GV(dot: Dot): A = voltageDot(GigaVolt)
  def TV(dot: Dot): A = voltageDot(TeraVolt)
  def PV(dot: Dot): A = voltageDot(PetaVolt)
  def EV(dot: Dot): A = voltageDot(ExaVolt)
  def ZV(dot: Dot): A = voltageDot(ZettaVolt)
  def YV(dot: Dot): A = voltageDot(YottaVolt)
  def statV(dot: Dot): A = voltageDot(statVolt)
  def abV(dot: Dot): A = voltageDot(Abvolt)
}

trait VoltagePer[A]{
  import VoltageUnit._

  protected def voltagePer(unit: VoltageUnit): A

  def yV(per: Per): A = voltagePer(YoctoVolt)
  def zV(per: Per): A = voltagePer(ZeptoVolt)
  def aV(per: Per): A = voltagePer(AttoVolt)
  def fV(per: Per): A = voltagePer(FemtoVolt)
  def pV(per: Per): A = voltagePer(PicoVolt)
  def nV(per: Per): A = voltagePer(NanoVolt)
  def μV(per: Per): A = voltagePer(MicroVolt)
  def mcV(per: Per): A = voltagePer(MicroVolt)
  def mV(per: Per): A = voltagePer(MilliVolt)
  def cV(per: Per): A = voltagePer(CentiVolt)
  def dV(per: Per): A = voltagePer(DeciVolt)
  def V(per: Per): A = voltagePer(Volt)
  def daV(per: Per): A = voltagePer(DecaVolt)
  def hV(per: Per): A = voltagePer(HectoVolt)
  def kV(per: Per): A = voltagePer(KiloVolt)
  def MV(per: Per): A = voltagePer(MegaVolt)
  def GV(per: Per): A = voltagePer(GigaVolt)
  def TV(per: Per): A = voltagePer(TeraVolt)
  def PV(per: Per): A = voltagePer(PetaVolt)
  def EV(per: Per): A = voltagePer(ExaVolt)
  def ZV(per: Per): A = voltagePer(ZettaVolt)
  def YV(per: Per): A = voltagePer(YottaVolt)
  def statV(per: Per): A = voltagePer(statVolt)
  def abV(per: Per): A = voltagePer(Abvolt)
}

trait PredefinedVoltageUnit extends VoltagePostfixOps[VoltageUnit]{
  override protected def voltagePostfixOps(unit: VoltageUnit) = unit
  
}

object PredefinedVoltageUnit extends PredefinedVoltageUnit
