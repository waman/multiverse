package org.waman.multiverse.thermal

import org.waman.multiverse._
import org.waman.multiverse.energy.EnergyUnit
import spire.implicits._
import spire.math.Real

sealed trait EntropyUnit extends PhysicalUnit[EntropyUnit]{

  def unitInJoulePerKelvin: Real

  override def baseUnit = EnergyUnit.Joule / TemperatureUnit.Kelvin
  override def valueInBaseUnit = unitInJoulePerKelvin
}

object EntropyUnit extends ConstantsDefined[EntropyUnit]{

  // intrinsic
  private[EntropyUnit]
  class IntrinsicEntropyUnit(name: String, val symbols: Seq[String], val unitInJoulePerKelvin: Real)
      extends EntropyUnit{

    def this(name: String, symbols: Seq[String], unit: EntropyUnit) =
      this(name, symbols, unit.unitInJoulePerKelvin)

    def this(name: String, symbols: Seq[String], factor: Real, unit: EntropyUnit) =
      this(name, symbols, factor * unit.unitInJoulePerKelvin)
  }


  case object Nat extends IntrinsicEntropyUnit("Nat", Seq("nat", "k_B"), r"1.380650523e-23") with NotExact
  case object Bit extends IntrinsicEntropyUnit("Bit", Seq("bit", "Sh"), Real(2).log, Nat) with NotExact
  case object Ban extends IntrinsicEntropyUnit("Ban", Seq("ban", "Hart"), Real(10).log, Nat) with NotExact
  case object Byte extends IntrinsicEntropyUnit("Byte", Seq("B"), r"1" * 8, Bit) with NotExact
  case object DecaByte extends IntrinsicEntropyUnit("DecaByte", Seq("daB"), r"1e1" * 8, Bit) with NotExact
  case object HectoByte extends IntrinsicEntropyUnit("HectoByte", Seq("hB"), r"1e2" * 8, Bit) with NotExact
  case object KiloByte extends IntrinsicEntropyUnit("KiloByte", Seq("kB"), r"1e3" * 8, Bit) with NotExact
  case object MegaByte extends IntrinsicEntropyUnit("MegaByte", Seq("MB"), r"1e6" * 8, Bit) with NotExact
  case object GigaByte extends IntrinsicEntropyUnit("GigaByte", Seq("GB"), r"1e9" * 8, Bit) with NotExact
  case object TeraByte extends IntrinsicEntropyUnit("TeraByte", Seq("TB"), r"1e12" * 8, Bit) with NotExact
  case object PetaByte extends IntrinsicEntropyUnit("PetaByte", Seq("PB"), r"1e15" * 8, Bit) with NotExact
  case object ExaByte extends IntrinsicEntropyUnit("ExaByte", Seq("EB"), r"1e18" * 8, Bit) with NotExact
  case object ZettaByte extends IntrinsicEntropyUnit("ZettaByte", Seq("ZB"), r"1e21" * 8, Bit) with NotExact
  case object YottaByte extends IntrinsicEntropyUnit("YottaByte", Seq("YB"), r"1e24" * 8, Bit) with NotExact
  case object KibiByte extends IntrinsicEntropyUnit("KibiByte", Seq("KiB", "KB"), r"1024", Byte) with NotExact
  case object MebiByte extends IntrinsicEntropyUnit("MebiByte", Seq("MiB"), r"1024"**2, Byte) with NotExact
  case object GibiByte extends IntrinsicEntropyUnit("GibiByte", Seq("GiB"), r"1024"**3, Byte) with NotExact
  case object TebiByte extends IntrinsicEntropyUnit("TebiByte", Seq("TiB"), r"1024"**4, Byte) with NotExact
  case object PebiByte extends IntrinsicEntropyUnit("PebiByte", Seq("PiB"), r"1024"**5, Byte) with NotExact
  case object ExbiByte extends IntrinsicEntropyUnit("ExbiByte", Seq("EiB"), r"1024"**6, Byte) with NotExact
  case object ZebiByte extends IntrinsicEntropyUnit("ZebiByte", Seq("ZiB"), r"1024"**7, Byte) with NotExact
  case object YobiByte extends IntrinsicEntropyUnit("YobiByte", Seq("YiB"), r"1024"**8, Byte) with NotExact

  override lazy val values = Seq(Nat, Bit, Ban, Byte, DecaByte, HectoByte, KiloByte, MegaByte, GigaByte, TeraByte, PetaByte, ExaByte, ZettaByte, YottaByte, KibiByte, MebiByte, GibiByte, TebiByte, PebiByte, ExbiByte, ZebiByte, YobiByte)

  // EnergyUnit / TemperatureUnit -> Entropy
  private[EntropyUnit]
  class QuotientEnergyPerTemperatureUnit(val numeratorUnit: EnergyUnit, val denominatorUnit: TemperatureUnit)
      extends EntropyUnit with QuotientUnit[EntropyUnit, EnergyUnit, TemperatureUnit]{

    override lazy val unitInJoulePerKelvin: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: EnergyUnit, dUnit: TemperatureUnit): EntropyUnit =
    new QuotientEnergyPerTemperatureUnit(nUnit, dUnit)
}

trait EntropyPostfixOps[A]{
  import EntropyUnit._

  protected def entropyPostfixOps(unit: EntropyUnit): A

  def nat : A = entropyPostfixOps(Nat)
  def k_B : A = entropyPostfixOps(Nat)
  def bit : A = entropyPostfixOps(Bit)
  def Sh : A = entropyPostfixOps(Bit)
  def ban : A = entropyPostfixOps(Ban)
  def Hart : A = entropyPostfixOps(Ban)
  def B : A = entropyPostfixOps(Byte)
  def daB : A = entropyPostfixOps(DecaByte)
  def hB : A = entropyPostfixOps(HectoByte)
  def kB : A = entropyPostfixOps(KiloByte)
  def MB : A = entropyPostfixOps(MegaByte)
  def GB : A = entropyPostfixOps(GigaByte)
  def TB : A = entropyPostfixOps(TeraByte)
  def PB : A = entropyPostfixOps(PetaByte)
  def EB : A = entropyPostfixOps(ExaByte)
  def ZB : A = entropyPostfixOps(ZettaByte)
  def YB : A = entropyPostfixOps(YottaByte)
  def KiB : A = entropyPostfixOps(KibiByte)
  def KB : A = entropyPostfixOps(KibiByte)
  def MiB : A = entropyPostfixOps(MebiByte)
  def GiB : A = entropyPostfixOps(GibiByte)
  def TiB : A = entropyPostfixOps(TebiByte)
  def PiB : A = entropyPostfixOps(PebiByte)
  def EiB : A = entropyPostfixOps(ExbiByte)
  def ZiB : A = entropyPostfixOps(ZebiByte)
  def YiB : A = entropyPostfixOps(YobiByte)
}

trait EntropyDot[A]{
  import EntropyUnit._

  protected def entropyDot(unit: EntropyUnit): A

  def nat(dot: Dot): A = entropyDot(Nat)
  def k_B(dot: Dot): A = entropyDot(Nat)
  def bit(dot: Dot): A = entropyDot(Bit)
  def Sh(dot: Dot): A = entropyDot(Bit)
  def ban(dot: Dot): A = entropyDot(Ban)
  def Hart(dot: Dot): A = entropyDot(Ban)
  def B(dot: Dot): A = entropyDot(Byte)
  def daB(dot: Dot): A = entropyDot(DecaByte)
  def hB(dot: Dot): A = entropyDot(HectoByte)
  def kB(dot: Dot): A = entropyDot(KiloByte)
  def MB(dot: Dot): A = entropyDot(MegaByte)
  def GB(dot: Dot): A = entropyDot(GigaByte)
  def TB(dot: Dot): A = entropyDot(TeraByte)
  def PB(dot: Dot): A = entropyDot(PetaByte)
  def EB(dot: Dot): A = entropyDot(ExaByte)
  def ZB(dot: Dot): A = entropyDot(ZettaByte)
  def YB(dot: Dot): A = entropyDot(YottaByte)
  def KiB(dot: Dot): A = entropyDot(KibiByte)
  def KB(dot: Dot): A = entropyDot(KibiByte)
  def MiB(dot: Dot): A = entropyDot(MebiByte)
  def GiB(dot: Dot): A = entropyDot(GibiByte)
  def TiB(dot: Dot): A = entropyDot(TebiByte)
  def PiB(dot: Dot): A = entropyDot(PebiByte)
  def EiB(dot: Dot): A = entropyDot(ExbiByte)
  def ZiB(dot: Dot): A = entropyDot(ZebiByte)
  def YiB(dot: Dot): A = entropyDot(YobiByte)
}

trait EntropyPer[A]{
  import EntropyUnit._

  protected def entropyPer(unit: EntropyUnit): A

  def nat(per: Per): A = entropyPer(Nat)
  def k_B(per: Per): A = entropyPer(Nat)
  def bit(per: Per): A = entropyPer(Bit)
  def Sh(per: Per): A = entropyPer(Bit)
  def ban(per: Per): A = entropyPer(Ban)
  def Hart(per: Per): A = entropyPer(Ban)
  def B(per: Per): A = entropyPer(Byte)
  def daB(per: Per): A = entropyPer(DecaByte)
  def hB(per: Per): A = entropyPer(HectoByte)
  def kB(per: Per): A = entropyPer(KiloByte)
  def MB(per: Per): A = entropyPer(MegaByte)
  def GB(per: Per): A = entropyPer(GigaByte)
  def TB(per: Per): A = entropyPer(TeraByte)
  def PB(per: Per): A = entropyPer(PetaByte)
  def EB(per: Per): A = entropyPer(ExaByte)
  def ZB(per: Per): A = entropyPer(ZettaByte)
  def YB(per: Per): A = entropyPer(YottaByte)
  def KiB(per: Per): A = entropyPer(KibiByte)
  def KB(per: Per): A = entropyPer(KibiByte)
  def MiB(per: Per): A = entropyPer(MebiByte)
  def GiB(per: Per): A = entropyPer(GibiByte)
  def TiB(per: Per): A = entropyPer(TebiByte)
  def PiB(per: Per): A = entropyPer(PebiByte)
  def EiB(per: Per): A = entropyPer(ExbiByte)
  def ZiB(per: Per): A = entropyPer(ZebiByte)
  def YiB(per: Per): A = entropyPer(YobiByte)
}

trait PredefinedEntropyUnit extends EntropyPostfixOps[EntropyUnit]{
  override protected def entropyPostfixOps(unit: EntropyUnit) = unit
  
}

object PredefinedEntropyUnit extends PredefinedEntropyUnit
