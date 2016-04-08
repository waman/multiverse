package org.waman.multiverse.luminous

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.luminous.LuminousFluxUnit._
import org.waman.multiverse.metric.AreaUnit._

sealed trait IlluminanceUnit extends PhysicalUnit[IlluminanceUnit]{

  override def getSIUnit = org.waman.multiverse.luminous.IlluminanceUnit.Lux
}

object IlluminanceUnit extends ConstantsDefined[IlluminanceUnit]{

  // intrinsic
  private[IlluminanceUnit]
  class IntrinsicIlluminanceUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends IlluminanceUnit{

    def this(name: String, symbols: Seq[String], unit: IlluminanceUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: IlluminanceUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoLux extends IntrinsicIlluminanceUnit("YoctoLux", Seq("ylx"), r"1e-24")
  case object ZeptoLux extends IntrinsicIlluminanceUnit("ZeptoLux", Seq("zlx"), r"1e-21")
  case object AttoLux extends IntrinsicIlluminanceUnit("AttoLux", Seq("alx"), r"1e-18")
  case object FemtoLux extends IntrinsicIlluminanceUnit("FemtoLux", Seq("flx"), r"1e-15")
  case object PicoLux extends IntrinsicIlluminanceUnit("PicoLux", Seq("plx"), r"1e-12")
  case object NanoLux extends IntrinsicIlluminanceUnit("NanoLux", Seq("nlx"), r"1e-9")
  case object MicroLux extends IntrinsicIlluminanceUnit("MicroLux", Seq("μlx", "mclx"), r"1e-6")
  case object MilliLux extends IntrinsicIlluminanceUnit("MilliLux", Seq("mlx"), r"1e-3")
  case object CentiLux extends IntrinsicIlluminanceUnit("CentiLux", Seq("clx"), r"1e-2")
  case object DeciLux extends IntrinsicIlluminanceUnit("DeciLux", Seq("dlx"), r"1e-1")
  case object Lux extends IntrinsicIlluminanceUnit("Lux", Seq("lx"), r"1")
  case object DecaLux extends IntrinsicIlluminanceUnit("DecaLux", Seq("dalx"), r"1e1")
  case object HectoLux extends IntrinsicIlluminanceUnit("HectoLux", Seq("hlx"), r"1e2")
  case object KiloLux extends IntrinsicIlluminanceUnit("KiloLux", Seq("klx"), r"1e3")
  case object MegaLux extends IntrinsicIlluminanceUnit("MegaLux", Seq("Mlx"), r"1e6")
  case object GigaLux extends IntrinsicIlluminanceUnit("GigaLux", Seq("Glx"), r"1e9")
  case object TeraLux extends IntrinsicIlluminanceUnit("TeraLux", Seq("Tlx"), r"1e12")
  case object PetaLux extends IntrinsicIlluminanceUnit("PetaLux", Seq("Plx"), r"1e15")
  case object ExaLux extends IntrinsicIlluminanceUnit("ExaLux", Seq("Elx"), r"1e18")
  case object ZettaLux extends IntrinsicIlluminanceUnit("ZettaLux", Seq("Zlx"), r"1e21")
  case object YottaLux extends IntrinsicIlluminanceUnit("YottaLux", Seq("Ylx"), r"1e24")
  case object Phot extends IntrinsicIlluminanceUnit("Phot", Seq("ph"), r"1e4")
  case object FootCandle extends IntrinsicIlluminanceUnit("FootCandle", Seq("fc"), Lumen / SquareFoot)

  override lazy val values = Seq(YoctoLux, ZeptoLux, AttoLux, FemtoLux, PicoLux, NanoLux, MicroLux, MilliLux, CentiLux, DeciLux, Lux, DecaLux, HectoLux, KiloLux, MegaLux, GigaLux, TeraLux, PetaLux, ExaLux, ZettaLux, YottaLux, Phot, FootCandle)

  // LuminousFluxUnit / AreaUnit -> Illuminance
  private[IlluminanceUnit]
  class QuotientLuminousFluxPerAreaUnit(val numeratorUnit: LuminousFluxUnit, val denominatorUnit: AreaUnit)
      extends IlluminanceUnit with QuotientUnit[IlluminanceUnit, LuminousFluxUnit, AreaUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: LuminousFluxUnit, dUnit: AreaUnit): IlluminanceUnit =
    new QuotientLuminousFluxPerAreaUnit(nUnit, dUnit)
}

trait MultiplicativeByIlluminanceUnit[R]{
  def *(unit: IlluminanceUnit): R
}

trait DivisibleByIlluminanceUnit[R]{
  def /(unit: IlluminanceUnit): R
}

trait IlluminancePostfixOps[A]{
  import IlluminanceUnit._

  protected def illuminancePostfixOps(unit: IlluminanceUnit): A


  def ylx : A = illuminancePostfixOps(YoctoLux)
  def zlx : A = illuminancePostfixOps(ZeptoLux)
  def alx : A = illuminancePostfixOps(AttoLux)
  def flx : A = illuminancePostfixOps(FemtoLux)
  def plx : A = illuminancePostfixOps(PicoLux)
  def nlx : A = illuminancePostfixOps(NanoLux)
  def μlx : A = illuminancePostfixOps(MicroLux)
  def mclx : A = illuminancePostfixOps(MicroLux)
  def mlx : A = illuminancePostfixOps(MilliLux)
  def clx : A = illuminancePostfixOps(CentiLux)
  def dlx : A = illuminancePostfixOps(DeciLux)
  def lx : A = illuminancePostfixOps(Lux)
  def dalx : A = illuminancePostfixOps(DecaLux)
  def hlx : A = illuminancePostfixOps(HectoLux)
  def klx : A = illuminancePostfixOps(KiloLux)
  def Mlx : A = illuminancePostfixOps(MegaLux)
  def Glx : A = illuminancePostfixOps(GigaLux)
  def Tlx : A = illuminancePostfixOps(TeraLux)
  def Plx : A = illuminancePostfixOps(PetaLux)
  def Elx : A = illuminancePostfixOps(ExaLux)
  def Zlx : A = illuminancePostfixOps(ZettaLux)
  def Ylx : A = illuminancePostfixOps(YottaLux)
  def ph : A = illuminancePostfixOps(Phot)
  def fc : A = illuminancePostfixOps(FootCandle)
}

trait IlluminanceDot[A]{
  import IlluminanceUnit._

  protected def illuminanceDot(unit: IlluminanceUnit): A

  def ylx(dot: Dot): A = illuminanceDot(YoctoLux)
  def zlx(dot: Dot): A = illuminanceDot(ZeptoLux)
  def alx(dot: Dot): A = illuminanceDot(AttoLux)
  def flx(dot: Dot): A = illuminanceDot(FemtoLux)
  def plx(dot: Dot): A = illuminanceDot(PicoLux)
  def nlx(dot: Dot): A = illuminanceDot(NanoLux)
  def μlx(dot: Dot): A = illuminanceDot(MicroLux)
  def mclx(dot: Dot): A = illuminanceDot(MicroLux)
  def mlx(dot: Dot): A = illuminanceDot(MilliLux)
  def clx(dot: Dot): A = illuminanceDot(CentiLux)
  def dlx(dot: Dot): A = illuminanceDot(DeciLux)
  def lx(dot: Dot): A = illuminanceDot(Lux)
  def dalx(dot: Dot): A = illuminanceDot(DecaLux)
  def hlx(dot: Dot): A = illuminanceDot(HectoLux)
  def klx(dot: Dot): A = illuminanceDot(KiloLux)
  def Mlx(dot: Dot): A = illuminanceDot(MegaLux)
  def Glx(dot: Dot): A = illuminanceDot(GigaLux)
  def Tlx(dot: Dot): A = illuminanceDot(TeraLux)
  def Plx(dot: Dot): A = illuminanceDot(PetaLux)
  def Elx(dot: Dot): A = illuminanceDot(ExaLux)
  def Zlx(dot: Dot): A = illuminanceDot(ZettaLux)
  def Ylx(dot: Dot): A = illuminanceDot(YottaLux)
  def ph(dot: Dot): A = illuminanceDot(Phot)
  def fc(dot: Dot): A = illuminanceDot(FootCandle)
}

trait IlluminancePer[A]{
  import IlluminanceUnit._

  protected def illuminancePer(unit: IlluminanceUnit): A

  def ylx(per: Per): A = illuminancePer(YoctoLux)
  def zlx(per: Per): A = illuminancePer(ZeptoLux)
  def alx(per: Per): A = illuminancePer(AttoLux)
  def flx(per: Per): A = illuminancePer(FemtoLux)
  def plx(per: Per): A = illuminancePer(PicoLux)
  def nlx(per: Per): A = illuminancePer(NanoLux)
  def μlx(per: Per): A = illuminancePer(MicroLux)
  def mclx(per: Per): A = illuminancePer(MicroLux)
  def mlx(per: Per): A = illuminancePer(MilliLux)
  def clx(per: Per): A = illuminancePer(CentiLux)
  def dlx(per: Per): A = illuminancePer(DeciLux)
  def lx(per: Per): A = illuminancePer(Lux)
  def dalx(per: Per): A = illuminancePer(DecaLux)
  def hlx(per: Per): A = illuminancePer(HectoLux)
  def klx(per: Per): A = illuminancePer(KiloLux)
  def Mlx(per: Per): A = illuminancePer(MegaLux)
  def Glx(per: Per): A = illuminancePer(GigaLux)
  def Tlx(per: Per): A = illuminancePer(TeraLux)
  def Plx(per: Per): A = illuminancePer(PetaLux)
  def Elx(per: Per): A = illuminancePer(ExaLux)
  def Zlx(per: Per): A = illuminancePer(ZettaLux)
  def Ylx(per: Per): A = illuminancePer(YottaLux)
  def ph(per: Per): A = illuminancePer(Phot)
  def fc(per: Per): A = illuminancePer(FootCandle)
}

trait PredefinedIlluminanceUnit extends IlluminancePostfixOps[IlluminanceUnit]{
  override protected def illuminancePostfixOps(unit: IlluminanceUnit) = unit
  
}

object PredefinedIlluminanceUnit extends PredefinedIlluminanceUnit
