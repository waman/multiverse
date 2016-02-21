package org.waman.multiverse.time

import org.waman.multiverse.angle.{AngleUnit, AngularVelocity}
import org.waman.multiverse.{PhysicalUnit, Quantity, UnitConverter}
import spire.implicits._
import spire.math.{Fractional, Real}

trait FrequencyPostfixOps[A]{
  import FrequencyUnit._

  protected def frequencyPostfixOps(frequencyUnit: FrequencyUnit): A

  def yHz: A = frequencyPostfixOps(YoctoHeltz)
  def zHz: A = frequencyPostfixOps(ZeptoHeltz)
  def aHz: A = frequencyPostfixOps(AttoHeltz)
  def fHz: A = frequencyPostfixOps(FemtoHeltz)
  def pHz: A = frequencyPostfixOps(PicoHeltz)
  def nHz: A = frequencyPostfixOps(NanoHeltz)
  def μHz: A = frequencyPostfixOps(MicroHeltz)
  def mHz: A = frequencyPostfixOps(MilliHeltz)
  def cHz: A = frequencyPostfixOps(CentiHeltz)
  def dHz: A = frequencyPostfixOps(DeciHeltz)
  def Hz : A = frequencyPostfixOps(Heltz)
  def daHz: A = frequencyPostfixOps(DecaHeltz)
  def hHz: A = frequencyPostfixOps(HectoHeltz)
  def kHz: A = frequencyPostfixOps(KiloHeltz)
  def MHz: A = frequencyPostfixOps(MegaHeltz)
  def GHz: A = frequencyPostfixOps(GigaHeltz)
  def THz: A = frequencyPostfixOps(TeraHeltz)
  def PHz: A = frequencyPostfixOps(PetaHeltz)
  def EHz: A = frequencyPostfixOps(ExaHeltz)
  def ZHz: A = frequencyPostfixOps(ZettaHeltz)
  def YHz: A = frequencyPostfixOps(YottaHeltz)
}

class Frequency[A: Fractional](val value: A, val unit: FrequencyUnit)
  extends Quantity[A, FrequencyUnit]
    with FrequencyPostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FrequencyUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInHeltz) / real(evalUnit.unitInHeltz)

  override protected def frequencyPostfixOps(frequencyUnit: FrequencyUnit) = apply(frequencyUnit)

  def toAngularVelocity: AngularVelocity[A] = new AngularVelocity(
    times(Hz, 2.0 * Real.pi),
    AngleUnit.Radian / TimeUnit.Second
  )
}

sealed abstract class FrequencyUnit(val symbol: String, val unitInHeltz: Real)
  extends PhysicalUnit[FrequencyUnit]{

  override val baseUnit = FrequencyUnit.Heltz
  override val inBaseUnitAccessor = () => unitInHeltz
}

object FrequencyUnit{

  case object YoctoHeltz extends FrequencyUnit("yHz", r"1e-24")
  case object ZeptoHeltz extends FrequencyUnit("zHz", r"1e-21")
  case object AttoHeltz  extends FrequencyUnit("aHz", r"1e-18")
  case object FemtoHeltz extends FrequencyUnit("aHz", r"1e-15")
  case object PicoHeltz  extends FrequencyUnit("pHz", r"1e-12")
  case object NanoHeltz  extends FrequencyUnit("nHz", r"1e-9")
  case object MicroHeltz extends FrequencyUnit("μHz", r"1e-6")
  case object MilliHeltz extends FrequencyUnit("mHz", r"1e-3")
  case object CentiHeltz extends FrequencyUnit("cHz", r"1e-2")
  case object DeciHeltz  extends FrequencyUnit("dHz", r"1e-1")
  case object Heltz      extends FrequencyUnit("Hz" , r"1")
  case object DecaHeltz  extends FrequencyUnit("daHz", r"10")
  case object HectoHeltz extends FrequencyUnit("hHz", r"1e2")
  case object KiloHeltz  extends FrequencyUnit("kHz", r"1e3")
  case object MegaHeltz  extends FrequencyUnit("MHz", r"1e6")
  case object GigaHeltz  extends FrequencyUnit("GHz", r"1e9")
  case object TeraHeltz  extends FrequencyUnit("THz", r"1e12")
  case object PetaHeltz  extends FrequencyUnit("PHz", r"1e15")
  case object ExaHeltz   extends FrequencyUnit("EHz", r"1e18")
  case object ZettaHeltz extends FrequencyUnit("ZHz", r"1e21")
  case object YottaHeltz extends FrequencyUnit("YHz", r"1e24")
}

trait PredefinedFrequencyUnit extends FrequencyPostfixOps[FrequencyUnit]{

  override protected def frequencyPostfixOps(frequencyUnit: FrequencyUnit) = frequencyUnit
}

object PredefinedFrequencyUnit extends PredefinedFrequencyUnit

trait FrequencyUnitInterpreter[A] extends FrequencyPostfixOps[Frequency[A]]{

  def apply(unit: FrequencyUnit): Frequency[A]

  override protected def frequencyPostfixOps(frequencyUnit: FrequencyUnit) = apply(frequencyUnit)
}