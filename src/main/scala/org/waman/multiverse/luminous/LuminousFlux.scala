package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait LuminousFluxPostfixOps[A]{

  import LuminousFluxUnit._

  protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit): A

  def ylm: A = luminousFluxPostfixOps(YoctoLumen)
  def zlm: A = luminousFluxPostfixOps(ZeptoLumen)
  def alm: A = luminousFluxPostfixOps(AttoLumen)
  def flm: A = luminousFluxPostfixOps(FemtoLumen)
  def plm: A = luminousFluxPostfixOps(PicoLumen)
  def nlm: A = luminousFluxPostfixOps(NanoLumen)
  def μlm: A = luminousFluxPostfixOps(MicroLumen)
  def mlm: A = luminousFluxPostfixOps(MilliLumen)
  def clm: A = luminousFluxPostfixOps(CentiLumen)
  def dlm: A = luminousFluxPostfixOps(DeciLumen)
  def lm : A = luminousFluxPostfixOps(Lumen)
  def dalm: A = luminousFluxPostfixOps(DecaLumen)
  def hlm: A = luminousFluxPostfixOps(HectoLumen)
  def klm: A = luminousFluxPostfixOps(KiloLumen)
  def Mlm: A = luminousFluxPostfixOps(MegaLumen)
  def Glm: A = luminousFluxPostfixOps(GigaLumen)
  def Tlm: A = luminousFluxPostfixOps(TeraLumen)
  def Plm: A = luminousFluxPostfixOps(PetaLumen)
  def Elm: A = luminousFluxPostfixOps(ExaLumen)
  def Zlm: A = luminousFluxPostfixOps(ZettaLumen)
  def Ylm: A = luminousFluxPostfixOps(YottaLumen)
}

trait LuminousFluxPer[A]{

  import LuminousFluxUnit._

  protected def luminousFluxPer(luminousFluxUnit: LuminousFluxUnit): A

  def ylm(per: Per): A = luminousFluxPer(YoctoLumen)
  def zlm(per: Per): A = luminousFluxPer(ZeptoLumen)
  def alm(per: Per): A = luminousFluxPer(AttoLumen)
  def flm(per: Per): A = luminousFluxPer(FemtoLumen)
  def plm(per: Per): A = luminousFluxPer(PicoLumen)
  def nlm(per: Per): A = luminousFluxPer(NanoLumen)
  def μlm(per: Per): A = luminousFluxPer(MicroLumen)
  def mlm(per: Per): A = luminousFluxPer(MilliLumen)
  def clm(per: Per): A = luminousFluxPer(CentiLumen)
  def dlm(per: Per): A = luminousFluxPer(DeciLumen)
  def lm (per: Per): A = luminousFluxPer(Lumen)
  def dalm(per: Per): A = luminousFluxPer(DecaLumen)
  def hlm(per: Per): A = luminousFluxPer(HectoLumen)
  def klm(per: Per): A = luminousFluxPer(KiloLumen)
  def Mlm(per: Per): A = luminousFluxPer(MegaLumen)
  def Glm(per: Per): A = luminousFluxPer(GigaLumen)
  def Tlm(per: Per): A = luminousFluxPer(TeraLumen)
  def Plm(per: Per): A = luminousFluxPer(PetaLumen)
  def Elm(per: Per): A = luminousFluxPer(ExaLumen)
  def Zlm(per: Per): A = luminousFluxPer(ZettaLumen)
  def Ylm(per: Per): A = luminousFluxPer(YottaLumen)
}

class LuminousFlux[A: Fractional](val value: A, val unit: LuminousFluxUnit)
  extends Quantity[A, LuminousFluxUnit]
    with LuminousFluxPostfixOps[A]
    with DivisibleByAreaUnit[Illuminance[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminousFluxUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInLumen) / real(evalUnit.unitInLumen)

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) = apply(luminousFluxUnit)

  override def /(areaUnit: AreaUnit) = new Illuminance[A](value, unit / areaUnit)
}

sealed abstract class LuminousFluxUnit(val symbol: String, val unitInLumen: Real)
    extends PhysicalUnit[LuminousFluxUnit]
    with DivisibleByAreaUnit[IlluminanceUnit]{

  override def baseUnit = LuminousFluxUnit.Lumen
  override def valueInBaseUnit = unitInLumen

  override def /(areaUnit: AreaUnit) = IlluminanceUnit(this, areaUnit)
}

object LuminousFluxUnit extends ConstantsDefined[LuminousFluxUnit]{

  // intrinsic
  case object YoctoLumen extends LuminousFluxUnit("ylm", r"1e-24")
  case object ZeptoLumen extends LuminousFluxUnit("zlm", r"1e-21")
  case object AttoLumen  extends LuminousFluxUnit("alm", r"1e-18")
  case object FemtoLumen extends LuminousFluxUnit("flm", r"1e-15")
  case object PicoLumen  extends LuminousFluxUnit("plm", r"1e-12")
  case object NanoLumen  extends LuminousFluxUnit("nlm", r"1e-9")
  case object MicroLumen extends LuminousFluxUnit("μlm", r"1e-6")
  case object MilliLumen extends LuminousFluxUnit("mlm", r"1e-3")
  case object CentiLumen extends LuminousFluxUnit("clm", r"1e-2")
  case object DeciLumen  extends LuminousFluxUnit("dlm", r"1e-1")
  case object Lumen      extends LuminousFluxUnit("lm" , 1)
  case object DecaLumen  extends LuminousFluxUnit("dalm", r"1e1")
  case object HectoLumen extends LuminousFluxUnit("hlm", r"1e2")
  case object KiloLumen  extends LuminousFluxUnit("klm", r"1e3")
  case object MegaLumen  extends LuminousFluxUnit("Mlm", r"1e6")
  case object GigaLumen  extends LuminousFluxUnit("Glm", r"1e9")
  case object TeraLumen  extends LuminousFluxUnit("Tlm", r"1e12")
  case object PetaLumen  extends LuminousFluxUnit("Plm", r"1e15")
  case object ExaLumen   extends LuminousFluxUnit("Elm", r"1e18")
  case object ZettaLumen extends LuminousFluxUnit("Zlm", r"1e21")
  case object YottaLumen extends LuminousFluxUnit("Ylm", r"1e24")

  override lazy val values = Seq(
    YoctoLumen,
    ZeptoLumen,
    AttoLumen,
    FemtoLumen,
    PicoLumen,
    NanoLumen,
    MicroLumen,
    MilliLumen,
    CentiLumen,
    DeciLumen,
    Lumen,
    DecaLumen,
    HectoLumen,
    KiloLumen,
    MegaLumen,
    GigaLumen,
    TeraLumen,
    PetaLumen,
    ExaLumen,
    ZettaLumen,
    YottaLumen
  )
}

trait PredefinedLuminousFluxUnit extends LuminousFluxPostfixOps[LuminousFluxUnit]{

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) = luminousFluxUnit
}

object PredefinedLuminousFluxUnit extends PredefinedLuminousFluxUnit

trait LuminousFluxFactory[A]
    extends LuminousFluxPostfixOps[LuminousFlux[A]]
    with LuminousFluxPer[AreaPostfixOps[Illuminance[A]]]{

  def apply(unit: LuminousFluxUnit): LuminousFlux[A]

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) =
    apply(luminousFluxUnit)

  // LuminousFlux / Area -> Illuminance
  def apply(unit: IlluminanceUnit): Illuminance[A]

  override protected def luminousFluxPer(luminousFluxUnit: LuminousFluxUnit) = new AreaPostfixOps[Illuminance[A]]{
    override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousFluxUnit / areaUnit)
  }
}