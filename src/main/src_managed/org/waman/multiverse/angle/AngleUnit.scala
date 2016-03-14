package org.waman.multiverse.angle

import org.waman.multiverse.MultiverseUtil.twoPi
import org.waman.multiverse._
import org.waman.multiverse.thermal.{DegreeTemperaturePostfixOps, TemperatureUnit}
import org.waman.multiverse.time.TimeUnit
import spire.implicits._
import spire.math.Real

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
    
  case object MicroRadian extends IntrinsicAngleUnit("MicroRadian", Seq("microRadian", "microRad", "μrad"), r"1e-6")
    
  case object MilliRadian extends IntrinsicAngleUnit("MilliRadian", Seq("mrad"), r"1e-3")
    
  case object CentiRadian extends IntrinsicAngleUnit("CentiRadian", Seq("crad"), r"1e-2")
    
  case object DeciRadian extends IntrinsicAngleUnit("DeciRadian", Seq("drad"), r"1e-1")
    
  case object Radian extends IntrinsicAngleUnit("Radian", Seq("rad"), r"1")
    
  case object Degree extends IntrinsicAngleUnit("Degree", Seq("deg", "°"), twoPi / r"360")
    with DegreeTemperaturePostfixOps[TemperatureUnit] { override protected def degreeTemperaturePostfixOps(unit: TemperatureUnit) = unit }
  case object ArcMinute extends IntrinsicAngleUnit("ArcMinute", Seq("arcmin", "MOA"), r"1/60", Degree)
    
  case object ArcSecond extends IntrinsicAngleUnit("ArcSecond", Seq("arcsec"), r"1/60", ArcMinute)
    
  case object YoctoArcSecond extends IntrinsicAngleUnit("YoctoArcSecond", Seq("yas"), r"1e-24", ArcSecond)
    
  case object ZeptoArcSecond extends IntrinsicAngleUnit("ZeptoArcSecond", Seq("zas"), r"1e-21", ArcSecond)
    
  case object AttoArcSecond extends IntrinsicAngleUnit("AttoArcSecond", Seq("aas"), r"1e-18", ArcSecond)
    
  case object FemtoArcSecond extends IntrinsicAngleUnit("FemtoArcSecond", Seq("fas"), r"1e-15", ArcSecond)
    
  case object PicoArcSecond extends IntrinsicAngleUnit("PicoArcSecond", Seq("pas"), r"1e-12", ArcSecond)
    
  case object NanoArcSecond extends IntrinsicAngleUnit("NanoArcSecond", Seq("nas"), r"1e-9", ArcSecond)
    
  case object MicroArcSecond extends IntrinsicAngleUnit("MicroArcSecond", Seq("microArcSecond", "microAs", "μas"), r"1e-6", ArcSecond)
    
  case object MilliArcSecond extends IntrinsicAngleUnit("MilliArcSecond", Seq("mas"), r"1e-3", ArcSecond)
    
  case object Gradian extends IntrinsicAngleUnit("Gradian", Seq("gon", "ᵍ"), twoPi / r"400")
    
  case object Turn extends IntrinsicAngleUnit("Turn", Seq("tr"), twoPi)
    

  override lazy val values = Seq(YoctoRadian, ZeptoRadian, AttoRadian, FemtoRadian, PicoRadian, NanoRadian, MicroRadian, MilliRadian, CentiRadian, DeciRadian, Radian, Degree, ArcMinute, ArcSecond, YoctoArcSecond, ZeptoArcSecond, AttoArcSecond, FemtoArcSecond, PicoArcSecond, NanoArcSecond, MicroArcSecond, MilliArcSecond, Gradian, Turn)
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
  def microRadian : A = anglePostfixOps(MicroRadian)
  def microRad : A = anglePostfixOps(MicroRadian)
  def μrad : A = anglePostfixOps(MicroRadian)
  def mrad : A = anglePostfixOps(MilliRadian)
  def crad : A = anglePostfixOps(CentiRadian)
  def drad : A = anglePostfixOps(DeciRadian)
  def rad : A = anglePostfixOps(Radian)
  def deg : A = anglePostfixOps(Degree)
  def ° : A = anglePostfixOps(Degree)
  def arcmin : A = anglePostfixOps(ArcMinute)
  def MOA : A = anglePostfixOps(ArcMinute)
  def arcsec : A = anglePostfixOps(ArcSecond)
  def yas : A = anglePostfixOps(YoctoArcSecond)
  def zas : A = anglePostfixOps(ZeptoArcSecond)
  def aas : A = anglePostfixOps(AttoArcSecond)
  def fas : A = anglePostfixOps(FemtoArcSecond)
  def pas : A = anglePostfixOps(PicoArcSecond)
  def nas : A = anglePostfixOps(NanoArcSecond)
  def microArcSecond : A = anglePostfixOps(MicroArcSecond)
  def microAs : A = anglePostfixOps(MicroArcSecond)
  def μas : A = anglePostfixOps(MicroArcSecond)
  def mas : A = anglePostfixOps(MilliArcSecond)
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
  def microRadian(dot: Dot): A = angleDot(MicroRadian)
  def microRad(dot: Dot): A = angleDot(MicroRadian)
  def μrad(dot: Dot): A = angleDot(MicroRadian)
  def mrad(dot: Dot): A = angleDot(MilliRadian)
  def crad(dot: Dot): A = angleDot(CentiRadian)
  def drad(dot: Dot): A = angleDot(DeciRadian)
  def rad(dot: Dot): A = angleDot(Radian)
  def deg(dot: Dot): A = angleDot(Degree)
  def °(dot: Dot): A = angleDot(Degree)
  def arcmin(dot: Dot): A = angleDot(ArcMinute)
  def MOA(dot: Dot): A = angleDot(ArcMinute)
  def arcsec(dot: Dot): A = angleDot(ArcSecond)
  def yas(dot: Dot): A = angleDot(YoctoArcSecond)
  def zas(dot: Dot): A = angleDot(ZeptoArcSecond)
  def aas(dot: Dot): A = angleDot(AttoArcSecond)
  def fas(dot: Dot): A = angleDot(FemtoArcSecond)
  def pas(dot: Dot): A = angleDot(PicoArcSecond)
  def nas(dot: Dot): A = angleDot(NanoArcSecond)
  def microArcSecond(dot: Dot): A = angleDot(MicroArcSecond)
  def microAs(dot: Dot): A = angleDot(MicroArcSecond)
  def μas(dot: Dot): A = angleDot(MicroArcSecond)
  def mas(dot: Dot): A = angleDot(MilliArcSecond)
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
  def microRadian(per: Per): A = anglePer(MicroRadian)
  def microRad(per: Per): A = anglePer(MicroRadian)
  def μrad(per: Per): A = anglePer(MicroRadian)
  def mrad(per: Per): A = anglePer(MilliRadian)
  def crad(per: Per): A = anglePer(CentiRadian)
  def drad(per: Per): A = anglePer(DeciRadian)
  def rad(per: Per): A = anglePer(Radian)
  def deg(per: Per): A = anglePer(Degree)
  def °(per: Per): A = anglePer(Degree)
  def arcmin(per: Per): A = anglePer(ArcMinute)
  def MOA(per: Per): A = anglePer(ArcMinute)
  def arcsec(per: Per): A = anglePer(ArcSecond)
  def yas(per: Per): A = anglePer(YoctoArcSecond)
  def zas(per: Per): A = anglePer(ZeptoArcSecond)
  def aas(per: Per): A = anglePer(AttoArcSecond)
  def fas(per: Per): A = anglePer(FemtoArcSecond)
  def pas(per: Per): A = anglePer(PicoArcSecond)
  def nas(per: Per): A = anglePer(NanoArcSecond)
  def microArcSecond(per: Per): A = anglePer(MicroArcSecond)
  def microAs(per: Per): A = anglePer(MicroArcSecond)
  def μas(per: Per): A = anglePer(MicroArcSecond)
  def mas(per: Per): A = anglePer(MilliArcSecond)
  def gon(per: Per): A = anglePer(Gradian)
  def ᵍ(per: Per): A = anglePer(Gradian)
  def tr(per: Per): A = anglePer(Turn)
}

trait PredefinedAngleUnit extends AnglePostfixOps[AngleUnit]{
  override protected def anglePostfixOps(unit: AngleUnit) = unit
  override def ° = AngleUnit.Degree
}

object PredefinedAngleUnit extends PredefinedAngleUnit
