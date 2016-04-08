package org.waman.multiverse.time

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._


sealed trait FrequencyUnit extends PhysicalUnit[FrequencyUnit]{

  override def getSIUnit = org.waman.multiverse.time.FrequencyUnit.Heltz
}

object FrequencyUnit extends ConstantsDefined[FrequencyUnit]{

  // intrinsic
  private[FrequencyUnit]
  class IntrinsicFrequencyUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends FrequencyUnit{

    def this(name: String, symbols: Seq[String], unit: FrequencyUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: FrequencyUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoHeltz extends IntrinsicFrequencyUnit("YoctoHeltz", Seq("yHz"), r"1e-24")
  case object ZeptoHeltz extends IntrinsicFrequencyUnit("ZeptoHeltz", Seq("zHz"), r"1e-21")
  case object AttoHeltz extends IntrinsicFrequencyUnit("AttoHeltz", Seq("aHz"), r"1e-18")
  case object FemtoHeltz extends IntrinsicFrequencyUnit("FemtoHeltz", Seq("fHz"), r"1e-15")
  case object PicoHeltz extends IntrinsicFrequencyUnit("PicoHeltz", Seq("pHz"), r"1e-12")
  case object NanoHeltz extends IntrinsicFrequencyUnit("NanoHeltz", Seq("nHz"), r"1e-9")
  case object MicroHeltz extends IntrinsicFrequencyUnit("MicroHeltz", Seq("μHz", "mcHz"), r"1e-6")
  case object MilliHeltz extends IntrinsicFrequencyUnit("MilliHeltz", Seq("mHz"), r"1e-3")
  case object CentiHeltz extends IntrinsicFrequencyUnit("CentiHeltz", Seq("cHz"), r"1e-2")
  case object DeciHeltz extends IntrinsicFrequencyUnit("DeciHeltz", Seq("dHz"), r"1e-1")
  case object Heltz extends IntrinsicFrequencyUnit("Heltz", Seq("Hz"), r"1")
  case object DecaHeltz extends IntrinsicFrequencyUnit("DecaHeltz", Seq("daHz"), r"1e1")
  case object HectoHeltz extends IntrinsicFrequencyUnit("HectoHeltz", Seq("hHz"), r"1e2")
  case object KiloHeltz extends IntrinsicFrequencyUnit("KiloHeltz", Seq("kHz"), r"1e3")
  case object MegaHeltz extends IntrinsicFrequencyUnit("MegaHeltz", Seq("MHz"), r"1e6")
  case object GigaHeltz extends IntrinsicFrequencyUnit("GigaHeltz", Seq("GHz"), r"1e9")
  case object TeraHeltz extends IntrinsicFrequencyUnit("TeraHeltz", Seq("THz"), r"1e12")
  case object PetaHeltz extends IntrinsicFrequencyUnit("PetaHeltz", Seq("PHz"), r"1e15")
  case object ExaHeltz extends IntrinsicFrequencyUnit("ExaHeltz", Seq("EHz"), r"1e18")
  case object ZettaHeltz extends IntrinsicFrequencyUnit("ZettaHeltz", Seq("ZHz"), r"1e21")
  case object YottaHeltz extends IntrinsicFrequencyUnit("YottaHeltz", Seq("YHz"), r"1e24")

  override lazy val values = Seq(YoctoHeltz, ZeptoHeltz, AttoHeltz, FemtoHeltz, PicoHeltz, NanoHeltz, MicroHeltz, MilliHeltz, CentiHeltz, DeciHeltz, Heltz, DecaHeltz, HectoHeltz, KiloHeltz, MegaHeltz, GigaHeltz, TeraHeltz, PetaHeltz, ExaHeltz, ZettaHeltz, YottaHeltz)
}

trait MultiplicativeByFrequencyUnit[R]{
  def *(unit: FrequencyUnit): R
}

trait DivisibleByFrequencyUnit[R]{
  def /(unit: FrequencyUnit): R
}

trait FrequencyPostfixOps[A]{
  import FrequencyUnit._

  protected def frequencyPostfixOps(unit: FrequencyUnit): A


  def yHz : A = frequencyPostfixOps(YoctoHeltz)
  def zHz : A = frequencyPostfixOps(ZeptoHeltz)
  def aHz : A = frequencyPostfixOps(AttoHeltz)
  def fHz : A = frequencyPostfixOps(FemtoHeltz)
  def pHz : A = frequencyPostfixOps(PicoHeltz)
  def nHz : A = frequencyPostfixOps(NanoHeltz)
  def μHz : A = frequencyPostfixOps(MicroHeltz)
  def mcHz : A = frequencyPostfixOps(MicroHeltz)
  def mHz : A = frequencyPostfixOps(MilliHeltz)
  def cHz : A = frequencyPostfixOps(CentiHeltz)
  def dHz : A = frequencyPostfixOps(DeciHeltz)
  def Hz : A = frequencyPostfixOps(Heltz)
  def daHz : A = frequencyPostfixOps(DecaHeltz)
  def hHz : A = frequencyPostfixOps(HectoHeltz)
  def kHz : A = frequencyPostfixOps(KiloHeltz)
  def MHz : A = frequencyPostfixOps(MegaHeltz)
  def GHz : A = frequencyPostfixOps(GigaHeltz)
  def THz : A = frequencyPostfixOps(TeraHeltz)
  def PHz : A = frequencyPostfixOps(PetaHeltz)
  def EHz : A = frequencyPostfixOps(ExaHeltz)
  def ZHz : A = frequencyPostfixOps(ZettaHeltz)
  def YHz : A = frequencyPostfixOps(YottaHeltz)
}

trait FrequencyDot[A]{
  import FrequencyUnit._

  protected def frequencyDot(unit: FrequencyUnit): A

  def yHz(dot: Dot): A = frequencyDot(YoctoHeltz)
  def zHz(dot: Dot): A = frequencyDot(ZeptoHeltz)
  def aHz(dot: Dot): A = frequencyDot(AttoHeltz)
  def fHz(dot: Dot): A = frequencyDot(FemtoHeltz)
  def pHz(dot: Dot): A = frequencyDot(PicoHeltz)
  def nHz(dot: Dot): A = frequencyDot(NanoHeltz)
  def μHz(dot: Dot): A = frequencyDot(MicroHeltz)
  def mcHz(dot: Dot): A = frequencyDot(MicroHeltz)
  def mHz(dot: Dot): A = frequencyDot(MilliHeltz)
  def cHz(dot: Dot): A = frequencyDot(CentiHeltz)
  def dHz(dot: Dot): A = frequencyDot(DeciHeltz)
  def Hz(dot: Dot): A = frequencyDot(Heltz)
  def daHz(dot: Dot): A = frequencyDot(DecaHeltz)
  def hHz(dot: Dot): A = frequencyDot(HectoHeltz)
  def kHz(dot: Dot): A = frequencyDot(KiloHeltz)
  def MHz(dot: Dot): A = frequencyDot(MegaHeltz)
  def GHz(dot: Dot): A = frequencyDot(GigaHeltz)
  def THz(dot: Dot): A = frequencyDot(TeraHeltz)
  def PHz(dot: Dot): A = frequencyDot(PetaHeltz)
  def EHz(dot: Dot): A = frequencyDot(ExaHeltz)
  def ZHz(dot: Dot): A = frequencyDot(ZettaHeltz)
  def YHz(dot: Dot): A = frequencyDot(YottaHeltz)
}

trait FrequencyPer[A]{
  import FrequencyUnit._

  protected def frequencyPer(unit: FrequencyUnit): A

  def yHz(per: Per): A = frequencyPer(YoctoHeltz)
  def zHz(per: Per): A = frequencyPer(ZeptoHeltz)
  def aHz(per: Per): A = frequencyPer(AttoHeltz)
  def fHz(per: Per): A = frequencyPer(FemtoHeltz)
  def pHz(per: Per): A = frequencyPer(PicoHeltz)
  def nHz(per: Per): A = frequencyPer(NanoHeltz)
  def μHz(per: Per): A = frequencyPer(MicroHeltz)
  def mcHz(per: Per): A = frequencyPer(MicroHeltz)
  def mHz(per: Per): A = frequencyPer(MilliHeltz)
  def cHz(per: Per): A = frequencyPer(CentiHeltz)
  def dHz(per: Per): A = frequencyPer(DeciHeltz)
  def Hz(per: Per): A = frequencyPer(Heltz)
  def daHz(per: Per): A = frequencyPer(DecaHeltz)
  def hHz(per: Per): A = frequencyPer(HectoHeltz)
  def kHz(per: Per): A = frequencyPer(KiloHeltz)
  def MHz(per: Per): A = frequencyPer(MegaHeltz)
  def GHz(per: Per): A = frequencyPer(GigaHeltz)
  def THz(per: Per): A = frequencyPer(TeraHeltz)
  def PHz(per: Per): A = frequencyPer(PetaHeltz)
  def EHz(per: Per): A = frequencyPer(ExaHeltz)
  def ZHz(per: Per): A = frequencyPer(ZettaHeltz)
  def YHz(per: Per): A = frequencyPer(YottaHeltz)
}

trait PredefinedFrequencyUnit extends FrequencyPostfixOps[FrequencyUnit]{
  override protected def frequencyPostfixOps(unit: FrequencyUnit) = unit
  
}

object PredefinedFrequencyUnit extends PredefinedFrequencyUnit
