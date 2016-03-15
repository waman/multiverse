package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.time.TimeUnit
import spire.implicits._
import spire.math.Real

sealed trait PressureUnit extends PhysicalUnit[PressureUnit]
  with MultiplicativeByTimeUnit[DynamicViscosityUnit]{

  def unitInPascal: Real

  override def baseUnit = org.waman.multiverse.fluid.PressureUnit.Pascal
  override def valueInBaseUnit = unitInPascal

  override def *(unit: TimeUnit) = DynamicViscosityUnit(this, unit)
}

object PressureUnit extends ConstantsDefined[PressureUnit]{

  // intrinsic
  private[PressureUnit]
  class IntrinsicPressureUnit(name: String, val symbols: Seq[String], val unitInPascal: Real)
      extends PressureUnit{

    def this(name: String, symbols: Seq[String], unit: PressureUnit) =
      this(name, symbols, unit.unitInPascal)

    def this(name: String, symbols: Seq[String], factor: Real, unit: PressureUnit) =
      this(name, symbols, factor * unit.unitInPascal)
  }


  case object YoctoPascal extends IntrinsicPressureUnit("YoctoPascal", Seq("yPa"), r"1e-24")
  case object ZeptoPascal extends IntrinsicPressureUnit("ZeptoPascal", Seq("zPa"), r"1e-21")
  case object AttoPascal extends IntrinsicPressureUnit("AttoPascal", Seq("aPa"), r"1e-18")
  case object FemtoPascal extends IntrinsicPressureUnit("FemtoPascal", Seq("fPa"), r"1e-15")
  case object PicoPascal extends IntrinsicPressureUnit("PicoPascal", Seq("pPa"), r"1e-12")
  case object NanoPascal extends IntrinsicPressureUnit("NanoPascal", Seq("nPa"), r"1e-9")
  case object MicroPascal extends IntrinsicPressureUnit("MicroPascal", Seq("microPascal", "microPa", "μPa"), r"1e-6")
  case object MilliPascal extends IntrinsicPressureUnit("MilliPascal", Seq("mPa"), r"1e-3")
  case object CentiPascal extends IntrinsicPressureUnit("CentiPascal", Seq("cPa"), r"1e-2")
  case object DeciPascal extends IntrinsicPressureUnit("DeciPascal", Seq("dPa"), r"1e-1")
  case object Pascal extends IntrinsicPressureUnit("Pascal", Seq("Pa"), r"1")
  case object DecaPascal extends IntrinsicPressureUnit("DecaPascal", Seq("daPa"), r"1e1")
  case object HectoPascal extends IntrinsicPressureUnit("HectoPascal", Seq("hPa"), r"1e2")
  case object KiloPascal extends IntrinsicPressureUnit("KiloPascal", Seq("kPa"), r"1e3")
  case object MegaPascal extends IntrinsicPressureUnit("MegaPascal", Seq("MPa"), r"1e6")
  case object GigaPascal extends IntrinsicPressureUnit("GigaPascal", Seq("GPa"), r"1e9")
  case object TeraPascal extends IntrinsicPressureUnit("TeraPascal", Seq("TPa"), r"1e12")
  case object PetaPascal extends IntrinsicPressureUnit("PetaPascal", Seq("PPa"), r"1e15")
  case object ExaPascal extends IntrinsicPressureUnit("ExaPascal", Seq("EPa"), r"1e18")
  case object ZettaPascal extends IntrinsicPressureUnit("ZettaPascal", Seq("ZPa"), r"1e21")
  case object YottaPascal extends IntrinsicPressureUnit("YottaPascal", Seq("YPa"), r"1e24")

  override lazy val values = Seq(YoctoPascal, ZeptoPascal, AttoPascal, FemtoPascal, PicoPascal, NanoPascal, MicroPascal, MilliPascal, CentiPascal, DeciPascal, Pascal, DecaPascal, HectoPascal, KiloPascal, MegaPascal, GigaPascal, TeraPascal, PetaPascal, ExaPascal, ZettaPascal, YottaPascal)
}

trait PressurePostfixOps[A]{
  import PressureUnit._

  protected def pressurePostfixOps(unit: PressureUnit): A

  def yPa : A = pressurePostfixOps(YoctoPascal)
  def zPa : A = pressurePostfixOps(ZeptoPascal)
  def aPa : A = pressurePostfixOps(AttoPascal)
  def fPa : A = pressurePostfixOps(FemtoPascal)
  def pPa : A = pressurePostfixOps(PicoPascal)
  def nPa : A = pressurePostfixOps(NanoPascal)
  def microPascal : A = pressurePostfixOps(MicroPascal)
  def microPa : A = pressurePostfixOps(MicroPascal)
  def μPa : A = pressurePostfixOps(MicroPascal)
  def mPa : A = pressurePostfixOps(MilliPascal)
  def cPa : A = pressurePostfixOps(CentiPascal)
  def dPa : A = pressurePostfixOps(DeciPascal)
  def Pa : A = pressurePostfixOps(Pascal)
  def daPa : A = pressurePostfixOps(DecaPascal)
  def hPa : A = pressurePostfixOps(HectoPascal)
  def kPa : A = pressurePostfixOps(KiloPascal)
  def MPa : A = pressurePostfixOps(MegaPascal)
  def GPa : A = pressurePostfixOps(GigaPascal)
  def TPa : A = pressurePostfixOps(TeraPascal)
  def PPa : A = pressurePostfixOps(PetaPascal)
  def EPa : A = pressurePostfixOps(ExaPascal)
  def ZPa : A = pressurePostfixOps(ZettaPascal)
  def YPa : A = pressurePostfixOps(YottaPascal)
}

trait PressureDot[A]{
  import PressureUnit._

  protected def pressureDot(unit: PressureUnit): A

  def yPa(dot: Dot): A = pressureDot(YoctoPascal)
  def zPa(dot: Dot): A = pressureDot(ZeptoPascal)
  def aPa(dot: Dot): A = pressureDot(AttoPascal)
  def fPa(dot: Dot): A = pressureDot(FemtoPascal)
  def pPa(dot: Dot): A = pressureDot(PicoPascal)
  def nPa(dot: Dot): A = pressureDot(NanoPascal)
  def microPascal(dot: Dot): A = pressureDot(MicroPascal)
  def microPa(dot: Dot): A = pressureDot(MicroPascal)
  def μPa(dot: Dot): A = pressureDot(MicroPascal)
  def mPa(dot: Dot): A = pressureDot(MilliPascal)
  def cPa(dot: Dot): A = pressureDot(CentiPascal)
  def dPa(dot: Dot): A = pressureDot(DeciPascal)
  def Pa(dot: Dot): A = pressureDot(Pascal)
  def daPa(dot: Dot): A = pressureDot(DecaPascal)
  def hPa(dot: Dot): A = pressureDot(HectoPascal)
  def kPa(dot: Dot): A = pressureDot(KiloPascal)
  def MPa(dot: Dot): A = pressureDot(MegaPascal)
  def GPa(dot: Dot): A = pressureDot(GigaPascal)
  def TPa(dot: Dot): A = pressureDot(TeraPascal)
  def PPa(dot: Dot): A = pressureDot(PetaPascal)
  def EPa(dot: Dot): A = pressureDot(ExaPascal)
  def ZPa(dot: Dot): A = pressureDot(ZettaPascal)
  def YPa(dot: Dot): A = pressureDot(YottaPascal)
}

trait PressurePer[A]{
  import PressureUnit._

  protected def pressurePer(unit: PressureUnit): A

  def yPa(per: Per): A = pressurePer(YoctoPascal)
  def zPa(per: Per): A = pressurePer(ZeptoPascal)
  def aPa(per: Per): A = pressurePer(AttoPascal)
  def fPa(per: Per): A = pressurePer(FemtoPascal)
  def pPa(per: Per): A = pressurePer(PicoPascal)
  def nPa(per: Per): A = pressurePer(NanoPascal)
  def microPascal(per: Per): A = pressurePer(MicroPascal)
  def microPa(per: Per): A = pressurePer(MicroPascal)
  def μPa(per: Per): A = pressurePer(MicroPascal)
  def mPa(per: Per): A = pressurePer(MilliPascal)
  def cPa(per: Per): A = pressurePer(CentiPascal)
  def dPa(per: Per): A = pressurePer(DeciPascal)
  def Pa(per: Per): A = pressurePer(Pascal)
  def daPa(per: Per): A = pressurePer(DecaPascal)
  def hPa(per: Per): A = pressurePer(HectoPascal)
  def kPa(per: Per): A = pressurePer(KiloPascal)
  def MPa(per: Per): A = pressurePer(MegaPascal)
  def GPa(per: Per): A = pressurePer(GigaPascal)
  def TPa(per: Per): A = pressurePer(TeraPascal)
  def PPa(per: Per): A = pressurePer(PetaPascal)
  def EPa(per: Per): A = pressurePer(ExaPascal)
  def ZPa(per: Per): A = pressurePer(ZettaPascal)
  def YPa(per: Per): A = pressurePer(YottaPascal)
}

trait PredefinedPressureUnit extends PressurePostfixOps[PressureUnit]{
  override protected def pressurePostfixOps(unit: PressureUnit) = unit
  
}

object PredefinedPressureUnit extends PredefinedPressureUnit
