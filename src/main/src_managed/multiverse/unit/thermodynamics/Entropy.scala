package multiverse.unit.thermodynamics

import spire.math.Real
import spire.math.Fractional

import multiverse._


class Entropy[A: Fractional](val value: A, val unit: EntropyUnit)
    extends LinearQuantity[Entropy[A], A, EntropyUnit] {

  override protected def newQuantity(value: A, unit: EntropyUnit): Entropy[A] = new Entropy(value, unit)
}

trait EntropyUnit extends LinearUnit[EntropyUnit]{

  override def getSIUnit: EntropyUnit = EntropyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EntropyUnit.dimension
}

object EntropyUnit extends UnitInfo[EntropyUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, Θ -> -1, M -> 1, L -> 2).withDefaultValue(0)

  import multiverse.unit.mechanics.EnergyUnit
  val getSIUnit: EntropyUnit = EnergyUnit.getSIUnit / AbsoluteTemperatureUnit.getSIUnit

  import EntropyUnitObjects._
  def getUnits: Seq[EntropyUnit] =
    Seq(nat, bit, ban, byte, decabyte, hectobyte, kilobyte, megabyte, gigabyte, terabyte, petabyte, exabyte, zettabyte, yottabyte, kibibyte, mebibyte, gibibyte, tebibyte, pebibyte, exbibyte, zebibyte, yobibyte)
}

/** For no aliase or user defined units */
class SimpleEntropyUnit(val name: String, val symbol: String, val interval: Real) extends EntropyUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultEntropyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EntropyUnit

object EntropyUnitObjects{

  import spire.implicits._

  import multiverse.unit.Constants

  final case object nat extends DefaultEntropyUnit("nat", "nat", Seq("k_B"), Constants.BoltzmannConstant)
  final case object bit extends DefaultEntropyUnit("bit", "bit", Seq("Sh"), Real(2).log() * nat.interval)
  final case object ban extends DefaultEntropyUnit("ban", "ban", Seq("Hart"), Real(10).log() * nat.interval)
  final case object byte extends SimpleEntropyUnit("byte", "B", r"8" * bit.interval)
  final case object decabyte extends SimpleEntropyUnit("decabyte", "daB", r"8" * r"1e1" * bit.interval)
  final case object hectobyte extends SimpleEntropyUnit("hectobyte", "hB", r"8" * r"1e2" * bit.interval)
  final case object kilobyte extends DefaultEntropyUnit("kilobyte", "kB", Seq("KB"), r"8" * r"1e3" * bit.interval)
  final case object megabyte extends SimpleEntropyUnit("megabyte", "MB", r"8" * r"1e6" * bit.interval)
  final case object gigabyte extends SimpleEntropyUnit("gigabyte", "GB", r"8" * r"1e9" * bit.interval)
  final case object terabyte extends SimpleEntropyUnit("terabyte", "TB", r"8" * r"1e12" * bit.interval)
  final case object petabyte extends SimpleEntropyUnit("petabyte", "PB", r"8" * r"1e15" * bit.interval)
  final case object exabyte extends SimpleEntropyUnit("exabyte", "EB", r"8" * r"1e18" * bit.interval)
  final case object zettabyte extends SimpleEntropyUnit("zettabyte", "ZB", r"8" * r"1e21" * bit.interval)
  final case object yottabyte extends SimpleEntropyUnit("yottabyte", "YB", r"8" * r"1e24" * bit.interval)
  final case object kibibyte extends SimpleEntropyUnit("kibibyte", "KiB", r"1024" * byte.interval)
  final case object mebibyte extends SimpleEntropyUnit("mebibyte", "MiB", r"1024" * kibibyte.interval)
  final case object gibibyte extends SimpleEntropyUnit("gibibyte", "GiB", r"1024" * mebibyte.interval)
  final case object tebibyte extends SimpleEntropyUnit("tebibyte", "TiB", r"1024" * gibibyte.interval)
  final case object pebibyte extends SimpleEntropyUnit("pebibyte", "PiB", r"1024" * tebibyte.interval)
  final case object exbibyte extends SimpleEntropyUnit("exbibyte", "EiB", r"1024" * pebibyte.interval)
  final case object zebibyte extends SimpleEntropyUnit("zebibyte", "ZiB", r"1024" * exbibyte.interval)
  final case object yobibyte extends SimpleEntropyUnit("yobibyte", "YiB", r"1024" * zebibyte.interval)
}

object EntropyUnits{

  def nat: EntropyUnit = EntropyUnitObjects.nat
  def k_B: EntropyUnit = EntropyUnitObjects.nat
  def bit: EntropyUnit = EntropyUnitObjects.bit
  def Sh: EntropyUnit = EntropyUnitObjects.bit
  def ban: EntropyUnit = EntropyUnitObjects.ban
  def Hart: EntropyUnit = EntropyUnitObjects.ban
  def B: EntropyUnit = EntropyUnitObjects.byte
  def daB: EntropyUnit = EntropyUnitObjects.decabyte
  def hB: EntropyUnit = EntropyUnitObjects.hectobyte
  def kB: EntropyUnit = EntropyUnitObjects.kilobyte
  def KB: EntropyUnit = EntropyUnitObjects.kilobyte
  def MB: EntropyUnit = EntropyUnitObjects.megabyte
  def GB: EntropyUnit = EntropyUnitObjects.gigabyte
  def TB: EntropyUnit = EntropyUnitObjects.terabyte
  def PB: EntropyUnit = EntropyUnitObjects.petabyte
  def EB: EntropyUnit = EntropyUnitObjects.exabyte
  def ZB: EntropyUnit = EntropyUnitObjects.zettabyte
  def YB: EntropyUnit = EntropyUnitObjects.yottabyte
  def KiB: EntropyUnit = EntropyUnitObjects.kibibyte
  def MiB: EntropyUnit = EntropyUnitObjects.mebibyte
  def GiB: EntropyUnit = EntropyUnitObjects.gibibyte
  def TiB: EntropyUnit = EntropyUnitObjects.tebibyte
  def PiB: EntropyUnit = EntropyUnitObjects.pebibyte
  def EiB: EntropyUnit = EntropyUnitObjects.exbibyte
  def ZiB: EntropyUnit = EntropyUnitObjects.zebibyte
  def YiB: EntropyUnit = EntropyUnitObjects.yobibyte
}