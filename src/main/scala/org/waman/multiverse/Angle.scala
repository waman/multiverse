package org.waman.multiverse

import org.waman.multiverse.MultiverseUtil.twoPi
import spire.implicits._
import spire.math.{Fractional, Real}

trait AnglePostfixOps[A]{
  import AngleUnit._

  protected def anglePostfixOps(angleUnit: AngleUnit): A

  def rad : A = anglePostfixOps(Radian)
  def drad: A = anglePostfixOps(DeciRadian)
  def crad: A = anglePostfixOps(CentiRadian)
  def mrad: A = anglePostfixOps(MilliRadian)
  def μrad: A = anglePostfixOps(MicroRadian)
  def nrad: A = anglePostfixOps(NanoRadian)
  def prad: A = anglePostfixOps(PicoRadian)
  def frad: A = anglePostfixOps(FemtoRadian)
  def arad: A = anglePostfixOps(AttoRadian)
  def zrad: A = anglePostfixOps(ZeptoRadian)
  def yrad: A = anglePostfixOps(YoctoRadian)

  def deg: A = anglePostfixOps(Degree)
  def °  : A = anglePostfixOps(Degree)
  def arcmin: A = anglePostfixOps(ArcMinute)
  def MOA: A = arcmin
  def arcsec: A = anglePostfixOps(ArcSecond)
  def mas: A = anglePostfixOps(MilliArcSecond)
  def μas: A = anglePostfixOps(MicroArcSecond)
  def nas: A = anglePostfixOps(NanoArcSecond)
  def pas: A = anglePostfixOps(PicoArcSecond)
  def fas: A = anglePostfixOps(FemtoArcSecond)
  def aas: A = anglePostfixOps(AttoArcSecond)
  def zas: A = anglePostfixOps(ZeptoArcSecond)
  def yas: A = anglePostfixOps(YoctoArcSecond)

  def gon: A = anglePostfixOps(Gradian)
  def ᵍ  : A = anglePostfixOps(Gradian)
  def tr : A = anglePostfixOps(Turn)
}

trait AnglePer[A]{
  import AngleUnit._

  protected def anglePer(angleUnit: AngleUnit): A

  def rad (per: Per): A = anglePer(Radian)
  def drad(per: Per): A = anglePer(DeciRadian)
  def crad(per: Per): A = anglePer(CentiRadian)
  def mrad(per: Per): A = anglePer(MilliRadian)
  def μrad(per: Per): A = anglePer(MicroRadian)
  def nrad(per: Per): A = anglePer(NanoRadian)
  def prad(per: Per): A = anglePer(PicoRadian)
  def frad(per: Per): A = anglePer(FemtoRadian)
  def arad(per: Per): A = anglePer(AttoRadian)
  def zrad(per: Per): A = anglePer(ZeptoRadian)
  def yrad(per: Per): A = anglePer(YoctoRadian)

  def deg(per: Per): A = anglePer(Degree)
  def °  (per: Per): A = anglePer(Degree)
  def arcmin(per: Per): A = anglePer(ArcMinute)
  def MOA(per: Per): A = arcmin(per)
  def arcsec(per: Per): A = anglePer(ArcSecond)
  def mas(per: Per): A = anglePer(MilliArcSecond)
  def μas(per: Per): A = anglePer(MicroArcSecond)
  def nas(per: Per): A = anglePer(NanoArcSecond)
  def pas(per: Per): A = anglePer(PicoArcSecond)
  def fas(per: Per): A = anglePer(FemtoArcSecond)
  def aas(per: Per): A = anglePer(AttoArcSecond)
  def zas(per: Per): A = anglePer(ZeptoArcSecond)
  def yas(per: Per): A = anglePer(YoctoArcSecond)

  def gon(per: Per): A = anglePer(Gradian)
  def ᵍ  (per: Per): A = anglePer(Gradian)
  def tr (per: Per): A = anglePer(Turn)
}

class Angle[A: Fractional](val value: A, val unit: AngleUnit)
    extends Quantity[A, AngleUnit]
    with AnglePostfixOps[A]
    with DivisibleByTime[AngularVelocity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInRadian) / real(evalUnit.unitInRadian)

  override protected def anglePostfixOps(angleUnit: AngleUnit) = apply(angleUnit)

  override def /(timeUnit: TimeUnit): AngularVelocity[A] = new AngularVelocity(value, unit / timeUnit)
}

abstract class AngleUnit(val symbol: String, val unitInRadian: Real)
    extends PhysicalUnit[AngleUnit]
    with DivisibleByTime[AngularVelocityUnit]{

  def this(symbol: String, factor: Real, angleUnit: AngleUnit) =
    this(symbol, factor * angleUnit.unitInRadian)

  override val baseUnit = AngleUnit.Radian
  override val inBaseUnitAccessor = () => unitInRadian

  override def /(timeUnit: TimeUnit): AngularVelocityUnit =
    AngularVelocityUnit(this, timeUnit)
}

object AngleUnit{

  case object Radian      extends AngleUnit("rad", 1)
  case object DeciRadian  extends AngleUnit("drad", r"1e-1")
  case object CentiRadian extends AngleUnit("crad", r"1e-2")
  case object MilliRadian extends AngleUnit("mrad", r"1e-3")
  case object MicroRadian extends AngleUnit("μrad", r"1e-6")
  case object NanoRadian  extends AngleUnit("nrad", r"1e-9")
  case object PicoRadian  extends AngleUnit("prad", r"1e-12")
  case object FemtoRadian extends AngleUnit("frad", r"1e-15")
  case object AttoRadian  extends AngleUnit("arad", r"1e-18")
  case object ZeptoRadian extends AngleUnit("zrad", r"1e-21")
  case object YoctoRadian extends AngleUnit("yrad", r"1e-24")

  case object Degree  extends AngleUnit("deg;°", twoPi / r"360")
  case object ArcMinute extends AngleUnit("arcmin;MOA", r"1/60", Degree)
  case object ArcSecond extends AngleUnit("arcsec", r"1/60", ArcMinute)
  case object MilliArcSecond extends AngleUnit("mas", r"1e-3", ArcSecond)
  case object MicroArcSecond extends AngleUnit("μas", r"1e-6", ArcSecond)
  case object NanoArcSecond  extends AngleUnit("nas", r"1e-9", ArcSecond)
  case object PicoArcSecond  extends AngleUnit("pas", r"1e-12", ArcSecond)
  case object FemtoArcSecond extends AngleUnit("fas", r"1e-15", ArcSecond)
  case object AttoArcSecond  extends AngleUnit("aas", r"1e-18", ArcSecond)
  case object ZeptoArcSecond extends AngleUnit("zas", r"1e-21", ArcSecond)
  case object YoctoArcSecond extends AngleUnit("yas", r"1e-24", ArcSecond)

  case object Gradian extends AngleUnit("gon;ᵍ", twoPi / r"400")
  case object Turn    extends AngleUnit("tr" , twoPi)
}

trait PredefinedAngleUnit extends AnglePostfixOps[AngleUnit]{
  override protected def anglePostfixOps(angleUnit: AngleUnit) = angleUnit
}

object PredefinedAngleUnit extends PredefinedAngleUnit

trait AngleUnitInterpreter[A]
    extends AnglePostfixOps[Angle[A]]
    with AnglePer[TimePostfixOps[AngularVelocity[A]]]{

  def apply(unit: AngleUnit): Angle[A]

  override protected def anglePostfixOps(angleUnit: AngleUnit) = apply(angleUnit)

  protected def newAnglePer(unit: AngleUnit): TimePostfixOps[AngularVelocity[A]] =
    new TimePostfixOps[AngularVelocity[A]] {
      override protected def timePostfixOps(timeUnit: TimeUnit) = apply(unit / timeUnit)
    }

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override protected def anglePer(angleUnit: AngleUnit) = newAnglePer(angleUnit)
}