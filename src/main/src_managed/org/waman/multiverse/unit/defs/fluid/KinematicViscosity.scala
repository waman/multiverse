package org.waman.multiverse.unit.defs.fluid

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class KinematicViscosity[A: Fractional](val value: A, val unit: KinematicViscosityUnit)
    extends LinearQuantity[KinematicViscosity[A], A, KinematicViscosityUnit] {

  override protected def newQuantity(value: A, unit: KinematicViscosityUnit): KinematicViscosity[A] = new KinematicViscosity(value, unit)
}

/** None */
trait KinematicViscosityUnit extends LinearUnit[KinematicViscosityUnit]{

  override def getSIUnit: KinematicViscosityUnit = KinematicViscosityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = KinematicViscosityUnit.dimension
}

object KinematicViscosityUnit extends UnitInfo[KinematicViscosityUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 2).withDefaultValue(0)

  val getSIUnit: KinematicViscosityUnit = AreaUnit.getSIUnit / TimeUnit.getSIUnit

  import KinematicViscosityUnitObjects._

  def getUnits: Seq[KinematicViscosityUnit] =
    Seq(stokes, yoctostokes, zeptostokes, attostokes, femtostokes, picostokes, nanostokes, microstokes, millistokes, centistokes, decistokes, decastokes, hectostokes, kilostokes, megastokes, gigastokes, terastokes, petastokes, exastokes, zettastokes, yottastokes)
}


/** For no alias or user defined units */
class SimpleKinematicViscosityUnit(val name: String, val symbol: String, val interval: Real) extends KinematicViscosityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultKinematicViscosityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends KinematicViscosityUnit
  
object KinematicViscosityUnitObjects{

  final case object stokes extends SimpleKinematicViscosityUnit("stokes", "St", r"1e-4")
  final case object yoctostokes extends SimpleKinematicViscosityUnit("yoctostokes", "ySt", r"1e-4" * r"1e-24")
  final case object zeptostokes extends SimpleKinematicViscosityUnit("zeptostokes", "zSt", r"1e-4" * r"1e-21")
  final case object attostokes extends SimpleKinematicViscosityUnit("attostokes", "aSt", r"1e-4" * r"1e-18")
  final case object femtostokes extends SimpleKinematicViscosityUnit("femtostokes", "fSt", r"1e-4" * r"1e-15")
  final case object picostokes extends SimpleKinematicViscosityUnit("picostokes", "pSt", r"1e-4" * r"1e-12")
  final case object nanostokes extends SimpleKinematicViscosityUnit("nanostokes", "nSt", r"1e-4" * r"1e-9")
  final case object microstokes extends DefaultKinematicViscosityUnit("microstokes", "μSt", Seq("mcSt"), r"1e-4" * r"1e-6")
  final case object millistokes extends SimpleKinematicViscosityUnit("millistokes", "mSt", r"1e-4" * r"1e-3")
  final case object centistokes extends SimpleKinematicViscosityUnit("centistokes", "cSt", r"1e-4" * r"1e-2")
  final case object decistokes extends SimpleKinematicViscosityUnit("decistokes", "dSt", r"1e-4" * r"1e-1")
  final case object decastokes extends SimpleKinematicViscosityUnit("decastokes", "daSt", r"1e-4" * r"1e1")
  final case object hectostokes extends SimpleKinematicViscosityUnit("hectostokes", "hSt", r"1e-4" * r"1e2")
  final case object kilostokes extends DefaultKinematicViscosityUnit("kilostokes", "kSt", Seq("KSt"), r"1e-4" * r"1e3")
  final case object megastokes extends SimpleKinematicViscosityUnit("megastokes", "MSt", r"1e-4" * r"1e6")
  final case object gigastokes extends SimpleKinematicViscosityUnit("gigastokes", "GSt", r"1e-4" * r"1e9")
  final case object terastokes extends SimpleKinematicViscosityUnit("terastokes", "TSt", r"1e-4" * r"1e12")
  final case object petastokes extends SimpleKinematicViscosityUnit("petastokes", "PSt", r"1e-4" * r"1e15")
  final case object exastokes extends SimpleKinematicViscosityUnit("exastokes", "ESt", r"1e-4" * r"1e18")
  final case object zettastokes extends SimpleKinematicViscosityUnit("zettastokes", "ZSt", r"1e-4" * r"1e21")
  final case object yottastokes extends SimpleKinematicViscosityUnit("yottastokes", "YSt", r"1e-4" * r"1e24")
}


object KinematicViscosityUnits{

  /** stokes */
  def St: KinematicViscosityUnit = KinematicViscosityUnitObjects.stokes
  /** yoctostokes */
  def ySt: KinematicViscosityUnit = KinematicViscosityUnitObjects.yoctostokes
  /** zeptostokes */
  def zSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.zeptostokes
  /** attostokes */
  def aSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.attostokes
  /** femtostokes */
  def fSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.femtostokes
  /** picostokes */
  def pSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.picostokes
  /** nanostokes */
  def nSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.nanostokes
  /** microstokes */
  def μSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.microstokes
  /** microstokes */
  def mcSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.microstokes
  /** millistokes */
  def mSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.millistokes
  /** centistokes */
  def cSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.centistokes
  /** decistokes */
  def dSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.decistokes
  /** decastokes */
  def daSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.decastokes
  /** hectostokes */
  def hSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.hectostokes
  /** kilostokes */
  def kSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.kilostokes
  /** kilostokes */
  def KSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.kilostokes
  /** megastokes */
  def MSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.megastokes
  /** gigastokes */
  def GSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.gigastokes
  /** terastokes */
  def TSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.terastokes
  /** petastokes */
  def PSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.petastokes
  /** exastokes */
  def ESt: KinematicViscosityUnit = KinematicViscosityUnitObjects.exastokes
  /** zettastokes */
  def ZSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.zettastokes
  /** yottastokes */
  def YSt: KinematicViscosityUnit = KinematicViscosityUnitObjects.yottastokes
}