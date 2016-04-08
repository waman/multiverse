package org.waman.multiverse.angle

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.MultiverseUtil.twoPi
import org.waman.multiverse.time._
import org.waman.multiverse.thermal._

sealed trait AngleUnit extends PhysicalUnit[AngleUnit]
  with DivisibleByTimeUnit[AngularVelocityUnit]{

  def unitInRadian: Real

  override def baseUnit = org.waman.multiverse.angle.AngleUnit.Radian
  override def valueInBaseUnit = unitInRadian

  override def /(unit: TimeUnit) = AngularVelocityUnit(this, unit)
}

object AngleUnit extends ConstantsDefined[AngleUnit]{

  // intrinsic
  private[AngleUnit]
  class IntrinsicAngleUnit(name: String, val symbols: Seq[String], val unitInRadian: Real)
      extends AngleUnit{

    def this(name: String, symbols: Seq[String], unit: AngleUnit) =
      this(name, symbols, unit.unitInRadian)

    def this(name: String, symbols: Seq[String], factor: Real, unit: AngleUnit) =
      this(name, symbols, factor * unit.unitInRadian)
  }


  case object YoctoRadian extends IntrinsicAngleUnit("YoctoRadian", Seq("yrad"), r"1e-24")
  case object ZeptoRadian extends IntrinsicAngleUnit("ZeptoRadian", Seq("zrad"), r"1e-21")
  case object AttoRadian extends IntrinsicAngleUnit("AttoRadian", Seq("arad"), r"1e-18")
  case object FemtoRadian extends IntrinsicAngleUnit("FemtoRadian", Seq("frad"), r"1e-15")
  case object PicoRadian extends IntrinsicAngleUnit("PicoRadian", Seq("prad"), r"1e-12")
  case object NanoRadian extends IntrinsicAngleUnit("NanoRadian", Seq("nrad"), r"1e-9")
  case object MicroRadian extends IntrinsicAngleUnit("MicroRadian", Seq("μrad", "mcrad"), r"1e-6")
  case object MilliRadian extends IntrinsicAngleUnit("MilliRadian", Seq("mrad"), r"1e-3")
  case object CentiRadian extends IntrinsicAngleUnit("CentiRadian", Seq("crad"), r"1e-2")
  case object DeciRadian extends IntrinsicAngleUnit("DeciRadian", Seq("drad"), r"1e-1")
  case object Radian extends IntrinsicAngleUnit("Radian", Seq("rad"), r"1")
  case object Degree extends IntrinsicAngleUnit("Degree", Seq("deg", "°"), twoPi / r"360")
    with DegreeTemperaturePostfixOps[TemperatureUnit] { override protected def degreeTemperaturePostfixOps(unit: TemperatureUnit) = unit }
  case object Arcmin extends IntrinsicAngleUnit("Arcmin", Seq("arcmin", "MOA"), r"1/60", Degree)
  case object Arcsec extends IntrinsicAngleUnit("Arcsec", Seq("arcsec"), r"1/60", Arcmin)
  case object YoctoArcsec extends IntrinsicAngleUnit("YoctoArcsec", Seq("yas"), r"1e-24", Arcsec)
  case object ZeptoArcsec extends IntrinsicAngleUnit("ZeptoArcsec", Seq("zas"), r"1e-21", Arcsec)
  case object AttoArcsec extends IntrinsicAngleUnit("AttoArcsec", Seq("aas"), r"1e-18", Arcsec)
  case object FemtoArcsec extends IntrinsicAngleUnit("FemtoArcsec", Seq("fas"), r"1e-15", Arcsec)
  case object PicoArcsec extends IntrinsicAngleUnit("PicoArcsec", Seq("pas"), r"1e-12", Arcsec)
  case object NanoArcsec extends IntrinsicAngleUnit("NanoArcsec", Seq("nas"), r"1e-9", Arcsec)
  case object MicroArcsec extends IntrinsicAngleUnit("MicroArcsec", Seq("μas", "mcas"), r"1e-6", Arcsec)
  case object MilliArcsec extends IntrinsicAngleUnit("MilliArcsec", Seq("mas"), r"1e-3", Arcsec)
  case object Gradian extends IntrinsicAngleUnit("Gradian", Seq("gon", "ᵍ"), twoPi / r"400")
  case object Turn extends IntrinsicAngleUnit("Turn", Seq("tr"), twoPi)

  override lazy val values = Seq(YoctoRadian, ZeptoRadian, AttoRadian, FemtoRadian, PicoRadian, NanoRadian, MicroRadian, MilliRadian, CentiRadian, DeciRadian, Radian, Degree, Arcmin, Arcsec, YoctoArcsec, ZeptoArcsec, AttoArcsec, FemtoArcsec, PicoArcsec, NanoArcsec, MicroArcsec, MilliArcsec, Gradian, Turn)
}

trait MultiplicativeByAngleUnit[R]{
  def *(unit: AngleUnit): R
}

trait DivisibleByAngleUnit[R]{
  def /(unit: AngleUnit): R
}

trait AnglePostfixOps[A]{
  import AngleUnit._

  protected def anglePostfixOps(unit: AngleUnit): A


  def yrad : A = anglePostfixOps(YoctoRadian)
  def zrad : A = anglePostfixOps(ZeptoRadian)
  def arad : A = anglePostfixOps(AttoRadian)
  def frad : A = anglePostfixOps(FemtoRadian)
  def prad : A = anglePostfixOps(PicoRadian)
  def nrad : A = anglePostfixOps(NanoRadian)
  def μrad : A = anglePostfixOps(MicroRadian)
  def mcrad : A = anglePostfixOps(MicroRadian)
  def mrad : A = anglePostfixOps(MilliRadian)
  def crad : A = anglePostfixOps(CentiRadian)
  def drad : A = anglePostfixOps(DeciRadian)
  def rad : A = anglePostfixOps(Radian)
  def deg : A = anglePostfixOps(Degree)
  def ° : A = anglePostfixOps(Degree)
  def arcmin : A = anglePostfixOps(Arcmin)
  def MOA : A = anglePostfixOps(Arcmin)
  def arcsec : A = anglePostfixOps(Arcsec)
  def yas : A = anglePostfixOps(YoctoArcsec)
  def zas : A = anglePostfixOps(ZeptoArcsec)
  def aas : A = anglePostfixOps(AttoArcsec)
  def fas : A = anglePostfixOps(FemtoArcsec)
  def pas : A = anglePostfixOps(PicoArcsec)
  def nas : A = anglePostfixOps(NanoArcsec)
  def μas : A = anglePostfixOps(MicroArcsec)
  def mcas : A = anglePostfixOps(MicroArcsec)
  def mas : A = anglePostfixOps(MilliArcsec)
  def gon : A = anglePostfixOps(Gradian)
  def ᵍ : A = anglePostfixOps(Gradian)
  def tr : A = anglePostfixOps(Turn)
}

trait AngleDot[A]{
  import AngleUnit._

  protected def angleDot(unit: AngleUnit): A

  def yrad(dot: Dot): A = angleDot(YoctoRadian)
  def zrad(dot: Dot): A = angleDot(ZeptoRadian)
  def arad(dot: Dot): A = angleDot(AttoRadian)
  def frad(dot: Dot): A = angleDot(FemtoRadian)
  def prad(dot: Dot): A = angleDot(PicoRadian)
  def nrad(dot: Dot): A = angleDot(NanoRadian)
  def μrad(dot: Dot): A = angleDot(MicroRadian)
  def mcrad(dot: Dot): A = angleDot(MicroRadian)
  def mrad(dot: Dot): A = angleDot(MilliRadian)
  def crad(dot: Dot): A = angleDot(CentiRadian)
  def drad(dot: Dot): A = angleDot(DeciRadian)
  def rad(dot: Dot): A = angleDot(Radian)
  def deg(dot: Dot): A = angleDot(Degree)
  def °(dot: Dot): A = angleDot(Degree)
  def arcmin(dot: Dot): A = angleDot(Arcmin)
  def MOA(dot: Dot): A = angleDot(Arcmin)
  def arcsec(dot: Dot): A = angleDot(Arcsec)
  def yas(dot: Dot): A = angleDot(YoctoArcsec)
  def zas(dot: Dot): A = angleDot(ZeptoArcsec)
  def aas(dot: Dot): A = angleDot(AttoArcsec)
  def fas(dot: Dot): A = angleDot(FemtoArcsec)
  def pas(dot: Dot): A = angleDot(PicoArcsec)
  def nas(dot: Dot): A = angleDot(NanoArcsec)
  def μas(dot: Dot): A = angleDot(MicroArcsec)
  def mcas(dot: Dot): A = angleDot(MicroArcsec)
  def mas(dot: Dot): A = angleDot(MilliArcsec)
  def gon(dot: Dot): A = angleDot(Gradian)
  def ᵍ(dot: Dot): A = angleDot(Gradian)
  def tr(dot: Dot): A = angleDot(Turn)
}

trait AnglePer[A]{
  import AngleUnit._

  protected def anglePer(unit: AngleUnit): A

  def yrad(per: Per): A = anglePer(YoctoRadian)
  def zrad(per: Per): A = anglePer(ZeptoRadian)
  def arad(per: Per): A = anglePer(AttoRadian)
  def frad(per: Per): A = anglePer(FemtoRadian)
  def prad(per: Per): A = anglePer(PicoRadian)
  def nrad(per: Per): A = anglePer(NanoRadian)
  def μrad(per: Per): A = anglePer(MicroRadian)
  def mcrad(per: Per): A = anglePer(MicroRadian)
  def mrad(per: Per): A = anglePer(MilliRadian)
  def crad(per: Per): A = anglePer(CentiRadian)
  def drad(per: Per): A = anglePer(DeciRadian)
  def rad(per: Per): A = anglePer(Radian)
  def deg(per: Per): A = anglePer(Degree)
  def °(per: Per): A = anglePer(Degree)
  def arcmin(per: Per): A = anglePer(Arcmin)
  def MOA(per: Per): A = anglePer(Arcmin)
  def arcsec(per: Per): A = anglePer(Arcsec)
  def yas(per: Per): A = anglePer(YoctoArcsec)
  def zas(per: Per): A = anglePer(ZeptoArcsec)
  def aas(per: Per): A = anglePer(AttoArcsec)
  def fas(per: Per): A = anglePer(FemtoArcsec)
  def pas(per: Per): A = anglePer(PicoArcsec)
  def nas(per: Per): A = anglePer(NanoArcsec)
  def μas(per: Per): A = anglePer(MicroArcsec)
  def mcas(per: Per): A = anglePer(MicroArcsec)
  def mas(per: Per): A = anglePer(MilliArcsec)
  def gon(per: Per): A = anglePer(Gradian)
  def ᵍ(per: Per): A = anglePer(Gradian)
  def tr(per: Per): A = anglePer(Turn)
}

trait PredefinedAngleUnit extends AnglePostfixOps[AngleUnit]{
  override protected def anglePostfixOps(unit: AngleUnit) = unit
  override def ° = AngleUnit.Degree
}

object PredefinedAngleUnit extends PredefinedAngleUnit
