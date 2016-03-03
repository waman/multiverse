package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.magnetic.{Flux, FluxUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait VoltagePostfixOps[A]{

  import VoltageUnit._

  protected def voltagePostfixOps(voltageUnit: VoltageUnit): A

  def yV: A = voltagePostfixOps(YoctoVoltage)
  def zV: A = voltagePostfixOps(ZeptoVoltage)
  def aV: A = voltagePostfixOps(AttoVoltage)
  def fV: A = voltagePostfixOps(FemtoVoltage)
  def pV: A = voltagePostfixOps(PicoVoltage)
  def nV: A = voltagePostfixOps(NanoVoltage)
  def microVolt: A = voltagePostfixOps(MicroVoltage)
  def microV   : A = microVolt
  def μV: A = microVolt
  def mV: A = voltagePostfixOps(MilliVoltage)
  def cV: A = voltagePostfixOps(CentiVoltage)
  def dV: A = voltagePostfixOps(DeciVoltage)
  def V : A = voltagePostfixOps(Voltage)
  def daV: A = voltagePostfixOps(DecaVoltage)
  def hV: A = voltagePostfixOps(HectoVoltage)
  def kV: A = voltagePostfixOps(KiloVoltage)
  def MV: A = voltagePostfixOps(MegaVoltage)
  def GV: A = voltagePostfixOps(GigaVoltage)
  def TV: A = voltagePostfixOps(TeraVoltage)
  def PV: A = voltagePostfixOps(PetaVoltage)
  def EV: A = voltagePostfixOps(ExaVoltage)
  def ZV: A = voltagePostfixOps(ZettaVoltage)
  def YV: A = voltagePostfixOps(YottaVoltage)
}

trait VoltageDot[A]{

  import VoltageUnit._

  protected def voltageDot(voltageUnit: VoltageUnit): A

  def yV(dot: Dot): A = voltageDot(YoctoVoltage)
  def zV(dot: Dot): A = voltageDot(ZeptoVoltage)
  def aV(dot: Dot): A = voltageDot(AttoVoltage)
  def fV(dot: Dot): A = voltageDot(FemtoVoltage)
  def pV(dot: Dot): A = voltageDot(PicoVoltage)
  def nV(dot: Dot): A = voltageDot(NanoVoltage)
  def microVolt(dot: Dot): A = voltageDot(MicroVoltage)
  def microV   (dot: Dot): A = microVolt(dot)
  def μV(dot: Dot): A = microVolt(dot)
  def mV(dot: Dot): A = voltageDot(MilliVoltage)
  def cV(dot: Dot): A = voltageDot(CentiVoltage)
  def dV(dot: Dot): A = voltageDot(DeciVoltage)
  def V (dot: Dot): A = voltageDot(Voltage)
  def daV(dot: Dot): A = voltageDot(DecaVoltage)
  def hV(dot: Dot): A = voltageDot(HectoVoltage)
  def kV(dot: Dot): A = voltageDot(KiloVoltage)
  def MV(dot: Dot): A = voltageDot(MegaVoltage)
  def GV(dot: Dot): A = voltageDot(GigaVoltage)
  def TV(dot: Dot): A = voltageDot(TeraVoltage)
  def PV(dot: Dot): A = voltageDot(PetaVoltage)
  def EV(dot: Dot): A = voltageDot(ExaVoltage)
  def ZV(dot: Dot): A = voltageDot(ZettaVoltage)
  def YV(dot: Dot): A = voltageDot(YottaVoltage)
}

trait VoltagePer[A]{

  import VoltageUnit._

  protected def voltagePer(voltageUnit: VoltageUnit): A

  def yV(per: Per): A = voltagePer(YoctoVoltage)
  def zV(per: Per): A = voltagePer(ZeptoVoltage)
  def aV(per: Per): A = voltagePer(AttoVoltage)
  def fV(per: Per): A = voltagePer(FemtoVoltage)
  def pV(per: Per): A = voltagePer(PicoVoltage)
  def nV(per: Per): A = voltagePer(NanoVoltage)
  def microVolt(per: Per): A = voltagePer(MicroVoltage)
  def microV   (per: Per): A = microVolt(per)
  def μV(per: Per): A = microVolt(per)
  def mV(per: Per): A = voltagePer(MilliVoltage)
  def cV(per: Per): A = voltagePer(CentiVoltage)
  def dV(per: Per): A = voltagePer(DeciVoltage)
  def V (per: Per): A = voltagePer(Voltage)
  def daV(per: Per): A = voltagePer(DecaVoltage)
  def hV(per: Per): A = voltagePer(HectoVoltage)
  def kV(per: Per): A = voltagePer(KiloVoltage)
  def MV(per: Per): A = voltagePer(MegaVoltage)
  def GV(per: Per): A = voltagePer(GigaVoltage)
  def TV(per: Per): A = voltagePer(TeraVoltage)
  def PV(per: Per): A = voltagePer(PetaVoltage)
  def EV(per: Per): A = voltagePer(ExaVoltage)
  def ZV(per: Per): A = voltagePer(ZettaVoltage)
  def YV(per: Per): A = voltagePer(YottaVoltage)
}

class Voltage[A: Fractional](val value: A, val unit: VoltageUnit)
  extends Quantity[A, VoltageUnit]
    with VoltagePostfixOps[A]
    with MultiplicativeByTimeUnit[Flux[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VoltageUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInVolt) / real(evalUnit.unitInVolt)

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = apply(voltageUnit)

  override def *(timeUnit: TimeUnit) = new Flux(value, unit * timeUnit)
}

sealed abstract class VoltageUnit(val symbols: Seq[String], val unitInVolt: Real)
    extends PhysicalUnit[VoltageUnit]
    with MultiplicativeByTimeUnit[FluxUnit]{

  override def baseUnit = VoltageUnit.Voltage
  override def valueInBaseUnit = unitInVolt

  override def *(timeUnit: TimeUnit) = FluxUnit(this, timeUnit)
}

object VoltageUnit extends ConstantsDefined[VoltageUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)
  
  // intrinsic
  case object YoctoVoltage extends VoltageUnit("yV", r"1e-24")
  case object ZeptoVoltage extends VoltageUnit("zV", r"1e-21")
  case object AttoVoltage  extends VoltageUnit("aV", r"1e-18")
  case object FemtoVoltage extends VoltageUnit("fV", r"1e-15")
  case object PicoVoltage  extends VoltageUnit("pV", r"1e-12")
  case object NanoVoltage  extends VoltageUnit("nV", r"1e-9")
  case object MicroVoltage extends VoltageUnit(Seq("μV", "microVolt", "microV"), r"1e-6")
  case object MilliVoltage extends VoltageUnit("mV", r"1e-3")
  case object CentiVoltage extends VoltageUnit("cV", r"1e-2")
  case object DeciVoltage  extends VoltageUnit("dV", r"1e-1")
  case object Voltage      extends VoltageUnit("V" , 1)
  case object DecaVoltage  extends VoltageUnit("daV", r"1e1")
  case object HectoVoltage extends VoltageUnit("hV", r"1e2")
  case object KiloVoltage  extends VoltageUnit("kV", r"1e3")
  case object MegaVoltage  extends VoltageUnit("MV", r"1e6")
  case object GigaVoltage  extends VoltageUnit("GV", r"1e9")
  case object TeraVoltage  extends VoltageUnit("TV", r"1e12")
  case object PetaVoltage  extends VoltageUnit("PV", r"1e15")
  case object ExaVoltage   extends VoltageUnit("EV", r"1e18")
  case object ZettaVoltage extends VoltageUnit("ZV", r"1e21")
  case object YottaVoltage extends VoltageUnit("YV", r"1e24")

  override lazy val values = Seq(
    YoctoVoltage,
    ZeptoVoltage,
    AttoVoltage,
    FemtoVoltage,
    PicoVoltage,
    NanoVoltage,
    MicroVoltage,
    MilliVoltage,
    CentiVoltage,
    DeciVoltage,
    Voltage,
    DecaVoltage,
    HectoVoltage,
    KiloVoltage,
    MegaVoltage,
    GigaVoltage,
    TeraVoltage,
    PetaVoltage,
    ExaVoltage,
    ZettaVoltage,
    YottaVoltage
  )
}

trait PredefinedVoltageUnit extends VoltagePostfixOps[VoltageUnit]{

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = voltageUnit
}

object PredefinedVoltageUnit extends PredefinedVoltageUnit

trait VoltageFactory[A]
    extends VoltagePostfixOps[Voltage[A]]
    with VoltageDot[TimePostfixOps[Flux[A]]]{

  def apply(unit: VoltageUnit): Voltage[A]

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = apply(voltageUnit)

  // Volt * Time -> Flux
  def apply(unit: FluxUnit): Flux[A]

  override protected def voltageDot(voltageUnit: VoltageUnit) = new TimePostfixOps[Flux[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(voltageUnit * timeUnit)
  }
}