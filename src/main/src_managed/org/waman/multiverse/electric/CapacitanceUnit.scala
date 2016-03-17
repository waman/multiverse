package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.Real


sealed trait CapacitanceUnit extends PhysicalUnit[CapacitanceUnit]{

  def unitInFarad: Real

  override def baseUnit = org.waman.multiverse.electric.CapacitanceUnit.Farad
  override def valueInBaseUnit = unitInFarad
}

object CapacitanceUnit extends ConstantsDefined[CapacitanceUnit]{

  // intrinsic
  private[CapacitanceUnit]
  class IntrinsicCapacitanceUnit(name: String, val symbols: Seq[String], val unitInFarad: Real)
      extends CapacitanceUnit{

    def this(name: String, symbols: Seq[String], unit: CapacitanceUnit) =
      this(name, symbols, unit.unitInFarad)

    def this(name: String, symbols: Seq[String], factor: Real, unit: CapacitanceUnit) =
      this(name, symbols, factor * unit.unitInFarad)
  }


  case object YoctoFarad extends IntrinsicCapacitanceUnit("YoctoFarad", Seq("yF"), r"1e-24")
  case object ZeptoFarad extends IntrinsicCapacitanceUnit("ZeptoFarad", Seq("zF"), r"1e-21")
  case object AttoFarad extends IntrinsicCapacitanceUnit("AttoFarad", Seq("aF"), r"1e-18")
  case object FemtoFarad extends IntrinsicCapacitanceUnit("FemtoFarad", Seq("fF"), r"1e-15")
  case object PicoFarad extends IntrinsicCapacitanceUnit("PicoFarad", Seq("pF"), r"1e-12")
  case object NanoFarad extends IntrinsicCapacitanceUnit("NanoFarad", Seq("nF"), r"1e-9")
  case object MicroFarad extends IntrinsicCapacitanceUnit("MicroFarad", Seq("microFarad", "microF", "μF"), r"1e-6")
  case object MilliFarad extends IntrinsicCapacitanceUnit("MilliFarad", Seq("mF"), r"1e-3")
  case object CentiFarad extends IntrinsicCapacitanceUnit("CentiFarad", Seq("cF"), r"1e-2")
  case object DeciFarad extends IntrinsicCapacitanceUnit("DeciFarad", Seq("dF"), r"1e-1")
  case object Farad extends IntrinsicCapacitanceUnit("Farad", Seq("F"), r"1")
  case object DecaFarad extends IntrinsicCapacitanceUnit("DecaFarad", Seq("daF"), r"1e1")
  case object HectoFarad extends IntrinsicCapacitanceUnit("HectoFarad", Seq("hF"), r"1e2")
  case object KiloFarad extends IntrinsicCapacitanceUnit("KiloFarad", Seq("kF"), r"1e3")
  case object MegaFarad extends IntrinsicCapacitanceUnit("MegaFarad", Seq("MF"), r"1e6")
  case object GigaFarad extends IntrinsicCapacitanceUnit("GigaFarad", Seq("GF"), r"1e9")
  case object TeraFarad extends IntrinsicCapacitanceUnit("TeraFarad", Seq("TF"), r"1e12")
  case object PetaFarad extends IntrinsicCapacitanceUnit("PetaFarad", Seq("PF"), r"1e15")
  case object ExaFarad extends IntrinsicCapacitanceUnit("ExaFarad", Seq("EF"), r"1e18")
  case object ZettaFarad extends IntrinsicCapacitanceUnit("ZettaFarad", Seq("ZF"), r"1e21")
  case object YottaFarad extends IntrinsicCapacitanceUnit("YottaFarad", Seq("YF"), r"1e24")

  override lazy val values = Seq(YoctoFarad, ZeptoFarad, AttoFarad, FemtoFarad, PicoFarad, NanoFarad, MicroFarad, MilliFarad, CentiFarad, DeciFarad, Farad, DecaFarad, HectoFarad, KiloFarad, MegaFarad, GigaFarad, TeraFarad, PetaFarad, ExaFarad, ZettaFarad, YottaFarad)

  // ChargeUnit / VoltageUnit -> Capacitance
  private[CapacitanceUnit]
  class QuotientChargePerVoltageUnit(val numeratorUnit: ChargeUnit, val denominatorUnit: VoltageUnit)
      extends CapacitanceUnit with QuotientUnit[CapacitanceUnit, ChargeUnit, VoltageUnit]{

    override lazy val unitInFarad: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: ChargeUnit, dUnit: VoltageUnit): CapacitanceUnit =
    new QuotientChargePerVoltageUnit(nUnit, dUnit)
}

trait MultiplicativeByCapacitanceUnit[R]{
  def *(unit: CapacitanceUnit): R
}

trait DivisibleByCapacitanceUnit[R]{
  def /(unit: CapacitanceUnit): R
}

trait CapacitancePostfixOps[A]{
  import CapacitanceUnit._

  protected def capacitancePostfixOps(unit: CapacitanceUnit): A

  def yF : A = capacitancePostfixOps(YoctoFarad)
  def zF : A = capacitancePostfixOps(ZeptoFarad)
  def aF : A = capacitancePostfixOps(AttoFarad)
  def fF : A = capacitancePostfixOps(FemtoFarad)
  def pF : A = capacitancePostfixOps(PicoFarad)
  def nF : A = capacitancePostfixOps(NanoFarad)
  def microFarad : A = capacitancePostfixOps(MicroFarad)
  def microF : A = capacitancePostfixOps(MicroFarad)
  def μF : A = capacitancePostfixOps(MicroFarad)
  def mF : A = capacitancePostfixOps(MilliFarad)
  def cF : A = capacitancePostfixOps(CentiFarad)
  def dF : A = capacitancePostfixOps(DeciFarad)
  def F : A = capacitancePostfixOps(Farad)
  def daF : A = capacitancePostfixOps(DecaFarad)
  def hF : A = capacitancePostfixOps(HectoFarad)
  def kF : A = capacitancePostfixOps(KiloFarad)
  def MF : A = capacitancePostfixOps(MegaFarad)
  def GF : A = capacitancePostfixOps(GigaFarad)
  def TF : A = capacitancePostfixOps(TeraFarad)
  def PF : A = capacitancePostfixOps(PetaFarad)
  def EF : A = capacitancePostfixOps(ExaFarad)
  def ZF : A = capacitancePostfixOps(ZettaFarad)
  def YF : A = capacitancePostfixOps(YottaFarad)
}

trait CapacitanceDot[A]{
  import CapacitanceUnit._

  protected def capacitanceDot(unit: CapacitanceUnit): A

  def yF(dot: Dot): A = capacitanceDot(YoctoFarad)
  def zF(dot: Dot): A = capacitanceDot(ZeptoFarad)
  def aF(dot: Dot): A = capacitanceDot(AttoFarad)
  def fF(dot: Dot): A = capacitanceDot(FemtoFarad)
  def pF(dot: Dot): A = capacitanceDot(PicoFarad)
  def nF(dot: Dot): A = capacitanceDot(NanoFarad)
  def microFarad(dot: Dot): A = capacitanceDot(MicroFarad)
  def microF(dot: Dot): A = capacitanceDot(MicroFarad)
  def μF(dot: Dot): A = capacitanceDot(MicroFarad)
  def mF(dot: Dot): A = capacitanceDot(MilliFarad)
  def cF(dot: Dot): A = capacitanceDot(CentiFarad)
  def dF(dot: Dot): A = capacitanceDot(DeciFarad)
  def F(dot: Dot): A = capacitanceDot(Farad)
  def daF(dot: Dot): A = capacitanceDot(DecaFarad)
  def hF(dot: Dot): A = capacitanceDot(HectoFarad)
  def kF(dot: Dot): A = capacitanceDot(KiloFarad)
  def MF(dot: Dot): A = capacitanceDot(MegaFarad)
  def GF(dot: Dot): A = capacitanceDot(GigaFarad)
  def TF(dot: Dot): A = capacitanceDot(TeraFarad)
  def PF(dot: Dot): A = capacitanceDot(PetaFarad)
  def EF(dot: Dot): A = capacitanceDot(ExaFarad)
  def ZF(dot: Dot): A = capacitanceDot(ZettaFarad)
  def YF(dot: Dot): A = capacitanceDot(YottaFarad)
}

trait CapacitancePer[A]{
  import CapacitanceUnit._

  protected def capacitancePer(unit: CapacitanceUnit): A

  def yF(per: Per): A = capacitancePer(YoctoFarad)
  def zF(per: Per): A = capacitancePer(ZeptoFarad)
  def aF(per: Per): A = capacitancePer(AttoFarad)
  def fF(per: Per): A = capacitancePer(FemtoFarad)
  def pF(per: Per): A = capacitancePer(PicoFarad)
  def nF(per: Per): A = capacitancePer(NanoFarad)
  def microFarad(per: Per): A = capacitancePer(MicroFarad)
  def microF(per: Per): A = capacitancePer(MicroFarad)
  def μF(per: Per): A = capacitancePer(MicroFarad)
  def mF(per: Per): A = capacitancePer(MilliFarad)
  def cF(per: Per): A = capacitancePer(CentiFarad)
  def dF(per: Per): A = capacitancePer(DeciFarad)
  def F(per: Per): A = capacitancePer(Farad)
  def daF(per: Per): A = capacitancePer(DecaFarad)
  def hF(per: Per): A = capacitancePer(HectoFarad)
  def kF(per: Per): A = capacitancePer(KiloFarad)
  def MF(per: Per): A = capacitancePer(MegaFarad)
  def GF(per: Per): A = capacitancePer(GigaFarad)
  def TF(per: Per): A = capacitancePer(TeraFarad)
  def PF(per: Per): A = capacitancePer(PetaFarad)
  def EF(per: Per): A = capacitancePer(ExaFarad)
  def ZF(per: Per): A = capacitancePer(ZettaFarad)
  def YF(per: Per): A = capacitancePer(YottaFarad)
}

trait PredefinedCapacitanceUnit extends CapacitancePostfixOps[CapacitanceUnit]{
  override protected def capacitancePostfixOps(unit: CapacitanceUnit) = unit
  
}

object PredefinedCapacitanceUnit extends PredefinedCapacitanceUnit
