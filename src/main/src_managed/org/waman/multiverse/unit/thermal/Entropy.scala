package org.waman.multiverse.unit.thermal

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._
class Entropy[A: Fractional](val value: A, val unit: EntropyUnit)
    extends LinearQuantity[Entropy[A], A, EntropyUnit] {

  override protected def newQuantity(value: A, unit: EntropyUnit): Entropy[A] = new Entropy(value, unit)
           }

trait EntropyUnit extends LinearUnit[EntropyUnit]{
  override def getSIUnit: EntropyUnit = EntropyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EntropyUnit.dimension

}

object EntropyUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, Θ -> -1, M -> 1, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.mechanics.EnergyUnit
  val getSIUnit: EntropyUnit = EnergyUnit.getSIUnit / AbsoluteTemperatureUnit.getSIUnit

import EntropyUnitObjects._
  def getUnits: Seq[EntropyUnit] =
    Seq(nat, bit, ban, byte, decabyte, hectobyte, kilobyte, megabyte, gigabyte, terabyte, petabyte, exabyte, zettabyte, yottabyte, kibibyte, mebibyte, gibibyte, tebibyte, pebibyte, exbibyte, zebibyte, yobibyte)
}



class DefaultEntropyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EntropyUnit

object EntropyUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object nat extends DefaultEntropyUnit("nat", "nat", Nil, Constants.BoltzmannConstant)
  final object bit extends DefaultEntropyUnit("bit", "bit", Nil, Real(2).log() * nat.interval)
  final object ban extends DefaultEntropyUnit("ban", "ban", Nil, Real(10).log() * nat.interval)
  final object byte extends DefaultEntropyUnit("byte", "B", Nil, r"8" * bit.interval)
  final object decabyte extends DefaultEntropyUnit("decabyte", "daB", Nil, r"8" * bit.interval * r"1e1")
  final object hectobyte extends DefaultEntropyUnit("hectobyte", "hB", Nil, r"8" * bit.interval * r"1e2")
  final object kilobyte extends DefaultEntropyUnit("kilobyte", "kB", Seq("KB"), r"8" * bit.interval * r"1e3")
  final object megabyte extends DefaultEntropyUnit("megabyte", "MB", Nil, r"8" * bit.interval * r"1e6")
  final object gigabyte extends DefaultEntropyUnit("gigabyte", "GB", Nil, r"8" * bit.interval * r"1e9")
  final object terabyte extends DefaultEntropyUnit("terabyte", "TB", Nil, r"8" * bit.interval * r"1e12")
  final object petabyte extends DefaultEntropyUnit("petabyte", "PB", Nil, r"8" * bit.interval * r"1e15")
  final object exabyte extends DefaultEntropyUnit("exabyte", "EB", Nil, r"8" * bit.interval * r"1e18")
  final object zettabyte extends DefaultEntropyUnit("zettabyte", "ZB", Nil, r"8" * bit.interval * r"1e21")
  final object yottabyte extends DefaultEntropyUnit("yottabyte", "YB", Nil, r"8" * bit.interval * r"1e24")
  final object kibibyte extends DefaultEntropyUnit("kibibyte", "KiB", Nil, r"1024" * byte.interval)
  final object mebibyte extends DefaultEntropyUnit("mebibyte", "MiB", Nil, r"1024" * kibibyte.interval)
  final object gibibyte extends DefaultEntropyUnit("gibibyte", "GiB", Nil, r"1024" * mebibyte.interval)
  final object tebibyte extends DefaultEntropyUnit("tebibyte", "TiB", Nil, r"1024" * gibibyte.interval)
  final object pebibyte extends DefaultEntropyUnit("pebibyte", "PiB", Nil, r"1024" * tebibyte.interval)
  final object exbibyte extends DefaultEntropyUnit("exbibyte", "EiB", Nil, r"1024" * pebibyte.interval)
  final object zebibyte extends DefaultEntropyUnit("zebibyte", "ZiB", Nil, r"1024" * exbibyte.interval)
  final object yobibyte extends DefaultEntropyUnit("yobibyte", "YiB", Nil, r"1024" * zebibyte.interval)
}

object EntropyUnits{
  def nat: EntropyUnit = EntropyUnitObjects.nat
  def bit: EntropyUnit = EntropyUnitObjects.bit
  def ban: EntropyUnit = EntropyUnitObjects.ban
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