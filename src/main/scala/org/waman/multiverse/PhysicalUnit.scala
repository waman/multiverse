package org.waman.multiverse

import spire.implicits._
import spire.math.Real

trait PhysicalUnit[U <: PhysicalUnit[U]] { this: U =>
  def name: String
  def symbol: String
  def aliases: Seq[String]
  /** Return <code>this.symbol +: this.aliases </code>*/
  def symbols: Seq[String] = symbol +: aliases
  def getSIUnit: U
  def dimension: Map[DimensionSymbol, Int]
}

sealed abstract class DimensionSymbol

object DimensionSymbol{
  final case object T extends DimensionSymbol
  final case object L extends DimensionSymbol
  final case object M extends DimensionSymbol
  final case object I extends DimensionSymbol
  final case object Θ extends DimensionSymbol
  final case object N extends DimensionSymbol
  final case object J extends DimensionSymbol

  def values: Seq[DimensionSymbol] = Seq(T, L, M, I, Θ, N, J)

  def valueOf(s: String): Option[DimensionSymbol] = s match {
    case "T" => Some(T)
    case "L" => Some(L)
    case "M" => Some(M)
    case "I" => Some(I)
    case "Θ" => Some(Θ)
    case "N" => Some(N)
    case "J" => Some(J)
    case _ => None
  }
}

// Maybe only temperature
trait HomogeneousUnit[U <: HomogeneousUnit[U]] extends PhysicalUnit[U]{ thisd : U =>
  /** zero value in SI unit */
  def zero: Real
  /** interval in SI Unit */
  def interval: Real

  /** Use <code>name</code> and <code>interval</code> properties (not <code>symbol</code>) for equality evaluation. */
  override def equals(other: Any): Boolean = other match {
    case that: HomogeneousUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        zero == that.zero &&
        interval == that.interval
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PhysicalUnit[_]]

  override def hashCode: Int =
    41 * (
      41 * (
        41 + name.hashCode
        ) + zero.hashCode
    ) + interval.hashCode

  override def toString: String =
    if(this == getSIUnit) {
      s"$name ($symbol)"
    }else{
      val symbolSI = getSIUnit.symbol
      val sZero = toReadableString(zero)
      this.interval match {
        case i if i.isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = Δ($symbolSI)]"
        case i if (-i).isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = -Δ($symbolSI)]"
        case _ =>
          val sInterval = toReadableString(interval)
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = $sInterval*Δ($symbolSI)]"
      }
  }
}

trait NotExact