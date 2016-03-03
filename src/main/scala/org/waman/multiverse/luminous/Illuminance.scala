package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait IlluminancePostfixOps[A]{

  import IlluminanceUnit._

  protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit): A

  def ylx: A = illuminancePostfixOps(YoctoLux)
  def zlx: A = illuminancePostfixOps(ZeptoLux)
  def alx: A = illuminancePostfixOps(AttoLux)
  def flx: A = illuminancePostfixOps(FemtoLux)
  def plx: A = illuminancePostfixOps(PicoLux)
  def nlx: A = illuminancePostfixOps(NanoLux)
  def microLux: A = illuminancePostfixOps(MicroLux)
  def microLx : A = microLux
  def μlx: A = microLux
  def mlx: A = illuminancePostfixOps(MilliLux)
  def clx: A = illuminancePostfixOps(CentiLux)
  def dlx: A = illuminancePostfixOps(DeciLux)
  def lx : A = illuminancePostfixOps(Lux)
  def dalx: A = illuminancePostfixOps(DecaLux)
  def hlx: A = illuminancePostfixOps(HectoLux)
  def klx: A = illuminancePostfixOps(KiloLux)
  def Mlx: A = illuminancePostfixOps(MegaLux)
  def Glx: A = illuminancePostfixOps(GigaLux)
  def Tlx: A = illuminancePostfixOps(TeraLux)
  def Plx: A = illuminancePostfixOps(PetaLux)
  def Elx: A = illuminancePostfixOps(ExaLux)
  def Zlx: A = illuminancePostfixOps(ZettaLux)
  def Ylx: A = illuminancePostfixOps(YottaLux)

  def ph: A = illuminancePostfixOps(Phot)
  def fc: A = illuminancePostfixOps(FootCandle)
}

class Illuminance[A: Fractional](val value: A, val unit: IlluminanceUnit)
  extends Quantity[A, IlluminanceUnit]
    with IlluminancePostfixOps[A]
    with LuminousFluxPostfixOps[DivisibleByAreaUnit[A]]
    with LuminousFluxPer[AreaPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: IlluminanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInLux) / real(evalUnit.unitInLux)

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) = apply(illuminanceUnit)

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) = new DivisibleByAreaUnit[A]{
    override def /(areaUnit: AreaUnit) = apply(luminousFluxUnit / areaUnit)
  }

  override protected def luminousFluxPer(luminousFluxUnit: LuminousFluxUnit) = new AreaPostfixOps[A]{
    override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousFluxUnit / areaUnit)
  }
}

sealed trait IlluminanceUnit extends PhysicalUnit[IlluminanceUnit]{
  
  def unitInLux: Real

  override def baseUnit = IlluminanceUnit.Lux
  override def valueInBaseUnit = unitInLux
}

object IlluminanceUnit extends ConstantsDefined[IlluminanceUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)

  // intrinsic
  private[IlluminanceUnit]
  class IntrinsicIlluminanceUnit(val symbols: Seq[String], val unitInLux: Real)
      extends IlluminanceUnit{

    def this(symbols: Seq[String], unit: IlluminanceUnit) = this(symbols, unit.unitInLux)
  }

  case object YoctoLux extends IntrinsicIlluminanceUnit("ylx", r"1e-24")
  case object ZeptoLux extends IntrinsicIlluminanceUnit("zlx", r"1e-21")
  case object AttoLux  extends IntrinsicIlluminanceUnit("alx", r"1e-18")
  case object FemtoLux extends IntrinsicIlluminanceUnit("flx", r"1e-15")
  case object PicoLux  extends IntrinsicIlluminanceUnit("plx", r"1e-12")
  case object NanoLux  extends IntrinsicIlluminanceUnit("nlx", r"1e-9")
  case object MicroLux extends IntrinsicIlluminanceUnit(Seq("μlx", "microLux", "microLx"), r"1e-6")
  case object MilliLux extends IntrinsicIlluminanceUnit("mlx", r"1e-3")
  case object CentiLux extends IntrinsicIlluminanceUnit("clx", r"1e-2")
  case object DeciLux  extends IntrinsicIlluminanceUnit("dlx", r"1e-1")
  case object Lux      extends IntrinsicIlluminanceUnit("lx" , 1)
  case object DecaLux  extends IntrinsicIlluminanceUnit("dalx", r"1e1")
  case object HectoLux extends IntrinsicIlluminanceUnit("hlx", r"1e2")
  case object KiloLux  extends IntrinsicIlluminanceUnit("klx", r"1e3")
  case object MegaLux  extends IntrinsicIlluminanceUnit("Mlx", r"1e6")
  case object GigaLux  extends IntrinsicIlluminanceUnit("Glx", r"1e9")
  case object TeraLux  extends IntrinsicIlluminanceUnit("Tlx", r"1e12")
  case object PetaLux  extends IntrinsicIlluminanceUnit("Plx", r"1e15")
  case object ExaLux   extends IntrinsicIlluminanceUnit("Elx", r"1e18")
  case object ZettaLux extends IntrinsicIlluminanceUnit("Zlx", r"1e21")
  case object YottaLux extends IntrinsicIlluminanceUnit("Ylx", r"1e24")
  
  case object Phot extends IntrinsicIlluminanceUnit("ph", r"1e4")
  case object FootCandle extends IntrinsicIlluminanceUnit("fc", LuminousFluxUnit.Lumen / AreaUnit.SquareFoot)

  override lazy val values = Seq(
    YoctoLux,
    ZeptoLux,
    AttoLux,
    FemtoLux,
    PicoLux,
    NanoLux,
    MicroLux,
    MilliLux,
    CentiLux,
    DeciLux,
    Lux,
    DecaLux,
    HectoLux,
    KiloLux,
    MegaLux,
    GigaLux,
    TeraLux,
    PetaLux,
    ExaLux,
    ZettaLux,
    YottaLux,
    
    Phot,
    FootCandle
  )
  
  // LuminousFlux / Area -> Illuminance
  private[IlluminanceUnit]
  class QuotientIlluminanceUnit(val numeratorUnit: LuminousFluxUnit, val denominatorUnit: AreaUnit)
    extends IlluminanceUnit with QuotientUnit[IlluminanceUnit, LuminousFluxUnit, AreaUnit]{

    override lazy val unitInLux: Real = numeratorUnit.unitInLumen / denominatorUnit.unitInSquareMetre
  }

  def apply(lfUnit: LuminousFluxUnit, aUnit: AreaUnit): IlluminanceUnit =
    new QuotientIlluminanceUnit(lfUnit, aUnit)
}

trait PredefinedIlluminanceUnit extends IlluminancePostfixOps[IlluminanceUnit]{

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) = illuminanceUnit
}

object PredefinedIlluminanceUnit extends PredefinedIlluminanceUnit

trait IlluminanceFactory[A]
    extends IlluminancePostfixOps[Illuminance[A]]{

  def apply(unit: IlluminanceUnit): Illuminance[A]

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) =
    apply(illuminanceUnit)
}