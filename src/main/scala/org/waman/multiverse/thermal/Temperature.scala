package org.waman.multiverse.thermal

import org.waman.multiverse._
import org.waman.multiverse.angle.DegreePostfixOps
import spire.implicits._
import spire.math.{Fractional, Real}

trait DegreeTemperaturePostfixOps[A]{
  import TemperatureUnit._
  protected def degreeTemperaturePostfixOps(unit: TemperatureUnit): A

  def C : A = degreeTemperaturePostfixOps(DegreeCelsius)
  def F : A = degreeTemperaturePostfixOps(DegreeFahrenheit)
  def De: A = degreeTemperaturePostfixOps(DegreeDelisle)
  def N : A = degreeTemperaturePostfixOps(DegreeNewton)
  def R : A = degreeTemperaturePostfixOps(DegreeRankine)
  def Re: A = degreeTemperaturePostfixOps(DegreeReaumur)
  def Ro: A = degreeTemperaturePostfixOps(DegreeRomer)
}

class Temperature[A: Fractional](val value: A, val unit: TemperatureUnit)
  extends Quantity[A, TemperatureUnit]
    with TemperaturePostfixOps[A]
    with DegreePostfixOps[DegreeTemperaturePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TemperatureUnit): A =
    if(unit == evalUnit) value
    else (value * real(unit.unitInKelvin) + real(unit.zeroInKelvin) - real(evalUnit.zeroInKelvin)) /
           real(evalUnit.unitInKelvin)

  override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) = apply(temperatureUnit)

  override def ° = new DegreeTemperaturePostfixOps[A]{
    override protected def degreeTemperaturePostfixOps(unit: TemperatureUnit) = apply(unit)
  }
}

/**
  * [Kelvin] = unitInKelvin * [degC] + zeroInKelvin
  * <ul>
  *   <li>unitInKelvin = 1.0</li>
  *   <li>zeroInKelvin = 273.15</li>
  * </ul>
  */
sealed abstract class TemperatureUnit(val symbols: Seq[String], val unitInKelvin: Real, val zeroInKelvin: Real)
    extends PhysicalUnit[TemperatureUnit]{

  override def baseUnit = TemperatureUnit.Kelvin
  override def valueInBaseUnit = unitInKelvin

  override def toDetailString: String = {
    val s = s"${name.padTo(30, ' ')} ($symbolStr)"

    if (this == TemperatureUnit.Kelvin) s
    else {
      var ds = s.padTo(50, ' ')
      ds += ("[" + symbols.head + "]").padTo(8, ' ')
      ds += " = "

      if(unitInKelvin == Real.one){
        ds += "[K]"
        if(zeroInKelvin != Real.zero) ds += s" - $zeroInKelvin"
      }else{
        ds += s"${unitInKelvin.reciprocal} * "
        if(zeroInKelvin == Real.zero) ds += "[K]"
        else ds += s"([K] - $zeroInKelvin)"
      }
      ds
    }
  }

  override def compare(that: TemperatureUnit): Int =
    this.unitInKelvin.compare(that.unitInKelvin) match {
      case 0 => this.zeroInKelvin.compare(that.zeroInKelvin)
      case i => i
    }
}

object TemperatureUnit extends ConstantsDefined[TemperatureUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)

  // intrinsic
  case object YoctoKelvin extends TemperatureUnit("yK", r"1e-24", 0)
  case object ZeptoKelvin extends TemperatureUnit("zK", r"1e-21", 0)
  case object AttoKelvin  extends TemperatureUnit("aK", r"1e-18", 0)
  case object FemtoKelvin extends TemperatureUnit("fK", r"1e-15", 0)
  case object PicoKelvin  extends TemperatureUnit("pK", r"1e-12", 0)
  case object NanoKelvin  extends TemperatureUnit("nK", r"1e-9", 0)
  case object MicroKelvin extends TemperatureUnit(Seq("μK", "microKelvin", "microK"), r"1e-6", 0)
  case object MilliKelvin extends TemperatureUnit("mK", r"1e-3", 0)
  case object CentiKelvin extends TemperatureUnit("cK", r"1e-2", 0)
  case object DeciKelvin  extends TemperatureUnit("dK", r"1e-1", 0)
  case object Kelvin      extends TemperatureUnit("K" , 1      , 0)
  case object DecaKelvin  extends TemperatureUnit("daK", r"1e1", 0)
  case object HectoKelvin extends TemperatureUnit("hK", r"1e2" , 0)
  case object KiloKelvin  extends TemperatureUnit("kK", r"1e3" , 0)
  case object MegaKelvin  extends TemperatureUnit("MK", r"1e6" , 0)
  case object GigaKelvin  extends TemperatureUnit("GK", r"1e9" , 0)
  case object TeraKelvin  extends TemperatureUnit("TK", r"1e12", 0)
  case object PetaKelvin  extends TemperatureUnit("PK", r"1e15", 0)
  case object ExaKelvin   extends TemperatureUnit("EK", r"1e18", 0)
  case object ZettaKelvin extends TemperatureUnit("ZK", r"1e21", 0)
  case object YottaKelvin extends TemperatureUnit("YK", r"1e24", 0)

  case object DegreeCelsius    extends TemperatureUnit(Seq("degC", "℃", "°C"), 1        , r"273.15")
  case object DegreeFahrenheit extends TemperatureUnit(Seq("degF", "℉", "°F") , r"5/9"   , r"273.15" - r"5/9" * 32)
  case object DegreeDelisle    extends TemperatureUnit(Seq("degDe", "°De")    , r"-2/3"  , r"373.15")
  case object DegreeNewton     extends TemperatureUnit(Seq("degN", "°N")      , r"100/33", r"273.15")
  case object DegreeRankine    extends TemperatureUnit(Seq("degR", "°R")      , r"5/9"   , 0)
  case object DegreeReaumur    extends TemperatureUnit(Seq("degRe", "°Re")    , r"5/4"   , r"273.15")
  case object DegreeRomer      extends TemperatureUnit(Seq("degRo", "°Ro")    , r"40/21" , r"273.15" - r"7.5" * r"40/21")
  case object ReguloGasMark    extends TemperatureUnit("GM"                   , r"125/9" , r"422.038")

  override lazy val values = Seq(
    YoctoKelvin,
    ZeptoKelvin,
    AttoKelvin,
    FemtoKelvin,
    PicoKelvin,
    NanoKelvin,
    MicroKelvin,
    MilliKelvin,
    CentiKelvin,
    DeciKelvin,
    Kelvin,
    DecaKelvin,
    HectoKelvin,
    KiloKelvin,
    MegaKelvin,
    GigaKelvin,
    TeraKelvin,
    PetaKelvin,
    ExaKelvin,
    ZettaKelvin,
    YottaKelvin,

    DegreeCelsius,
    DegreeFahrenheit,
    DegreeDelisle,
    DegreeNewton,
    DegreeRankine,
    DegreeReaumur,
    DegreeRomer,
    ReguloGasMark
  )
}

trait TemperatureFactory[A]
    extends TemperaturePostfixOps[Temperature[A]]{

  def apply(unit: TemperatureUnit): Temperature[A]

  override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) =
    apply(temperatureUnit)
}