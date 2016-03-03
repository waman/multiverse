package org.waman.multiverse.thermal

import org.waman.multiverse._
import org.waman.multiverse.energy.{EnergyPer, EnergyPostfixOps, EnergyUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait EntropyPostfixOps[A]{

  import EntropyUnit._

  protected def entropyPostfixOps(entropyUnit: EntropyUnit): A

  def nat: A = entropyPostfixOps(Nat)
  def k_B: A = nat
  def bit: A = entropyPostfixOps(Bit)
  def Sh : A = bit
  def ban: A = entropyPostfixOps(Ban)
  def Hart: A = ban

  def B : A = entropyPostfixOps(Byte)
  def kB: A = entropyPostfixOps(KiloByte)
  def MB: A = entropyPostfixOps(MegaByte)
  def GB: A = entropyPostfixOps(GigaByte)
  def TB: A = entropyPostfixOps(TeraByte)
  def PB: A = entropyPostfixOps(PetaByte)
  def EB: A = entropyPostfixOps(ExaByte)
  def ZB: A = entropyPostfixOps(ZettaByte)
  def YB: A = entropyPostfixOps(YottaByte)

  def KB: A = entropyPostfixOps(KibiByte)
  def KiB: A = KB
  def MiB: A = entropyPostfixOps(MebiByte)
  def GiB: A = entropyPostfixOps(GibiByte)
  def TiB: A = entropyPostfixOps(TebiByte)
  def PiB: A = entropyPostfixOps(PebiByte)
  def EiB: A = entropyPostfixOps(ExbiByte)
  def ZiB: A = entropyPostfixOps(ZebiByte)
  def YiB: A = entropyPostfixOps(YobiByte)
}

class Entropy[A: Fractional](val value: A, val unit: EntropyUnit)
  extends Quantity[A, EntropyUnit]
    with EntropyPostfixOps[A]
    with EnergyPostfixOps[DivisibleByTemperatureUnit[A]]
    with EnergyPer[TemperaturePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EntropyUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInJoulePerKelvin) / real(evalUnit.unitInJoulePerKelvin)

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) = apply(entropyUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = new  DivisibleByTemperatureUnit[A]{
    override def /(temperatureUnit: TemperatureUnit) = apply(energyUnit / temperatureUnit)
  }

  override protected def energyPer(energyUnit: EnergyUnit) = new TemperaturePostfixOps[A]{
    override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) =
      apply(energyUnit / temperatureUnit)
  }
}

sealed trait EntropyUnit extends PhysicalUnit[EntropyUnit]{

  def unitInJoulePerKelvin: Real

  override def baseUnit = EnergyUnit.Joule / TemperatureUnit.Kelvin
  override def valueInBaseUnit = unitInJoulePerKelvin
}

object EntropyUnit extends ConstantsDefined[EntropyUnit]{

  // Custom
  private[EntropyUnit]
  class IntrinsicEntropyUnit(val symbol: String, val unitInJoulePerKelvin: Real)
      extends EntropyUnit{
    
    def this(symbol: String, factor: Real, unit: EntropyUnit) = 
      this(symbol, factor * unit.unitInJoulePerKelvin)
  }

  case object Nat  extends IntrinsicEntropyUnit("nat;k_B", r"1.380650523e-23") with NotExact
  case object Bit  extends IntrinsicEntropyUnit("bit;Sh", Real(2).log, Nat) with NotExact
  case object Ban  extends IntrinsicEntropyUnit("ban;Hart", Real(10).log, Nat) with NotExact
  case object Byte extends IntrinsicEntropyUnit("B", 8, Bit) with NotExact

  case object KiloByte  extends IntrinsicEntropyUnit("kB", r"1e3", Byte)
  case object MegaByte  extends IntrinsicEntropyUnit("MB", r"1e6", Byte)
  case object GigaByte  extends IntrinsicEntropyUnit("GB", r"1e9", Byte)
  case object TeraByte  extends IntrinsicEntropyUnit("TB", r"1e12", Byte)
  case object PetaByte  extends IntrinsicEntropyUnit("PB", r"1e15", Byte)
  case object ExaByte   extends IntrinsicEntropyUnit("EB", r"1e18", Byte)
  case object ZettaByte extends IntrinsicEntropyUnit("ZB", r"1e21", Byte)
  case object YottaByte extends IntrinsicEntropyUnit("YB", r"1e24", Byte)

  case object KibiByte extends IntrinsicEntropyUnit("KiB;KB", r"1024", Byte)
  case object MebiByte extends IntrinsicEntropyUnit("MiB", r"1024"**2, Byte)
  case object GibiByte extends IntrinsicEntropyUnit("GiB", r"1024"**3, Byte)
  case object TebiByte extends IntrinsicEntropyUnit("TiB", r"1024"**4, Byte)
  case object PebiByte extends IntrinsicEntropyUnit("PiB", r"1024"**5, Byte)
  case object ExbiByte extends IntrinsicEntropyUnit("EiB", r"1024"**6, Byte)
  case object ZebiByte extends IntrinsicEntropyUnit("ZiB", r"1024"**7, Byte)
  case object YobiByte extends IntrinsicEntropyUnit("YiB", r"1024"**8, Byte)

  override lazy val values = Seq(
    Nat,
    Bit,
    Ban,
    Byte,
    
    KiloByte,
    MegaByte,
    GigaByte,
    TeraByte,
    PetaByte,
    ExaByte,
    ZettaByte,
    YottaByte,

    KibiByte,
    MebiByte,
    GibiByte,
    TebiByte,
    PebiByte,
    ExbiByte,
    ZebiByte,
    YobiByte
  )

  // Quotient (Energy / Temperature)
  private[EntropyUnit]
  class QuotientEntropyUnit(val numeratorUnit: EnergyUnit, val denominatorUnit: TemperatureUnit)
    extends EntropyUnit with QuotientUnit[EntropyUnit, EnergyUnit, TemperatureUnit]{

    override lazy val unitInJoulePerKelvin: Real =
      numeratorUnit.unitInJoule / denominatorUnit.unitInKelvin
  }

  def apply(eUnit: EnergyUnit, tUnit: TemperatureUnit): EntropyUnit =
    new QuotientEntropyUnit(eUnit, tUnit)
}

trait PredefinedEntropyUnit extends EntropyPostfixOps[EntropyUnit]{

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) = entropyUnit
}

object PredefinedEntropyUnit extends PredefinedEntropyUnit

trait EntropyFactory[A]
    extends EntropyPostfixOps[Entropy[A]]{

  def apply(unit: EntropyUnit): Entropy[A]

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) =
    apply(entropyUnit)
}