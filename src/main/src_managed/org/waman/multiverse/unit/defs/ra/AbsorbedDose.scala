package org.waman.multiverse.unit.defs.ra

import spire.math._
import spire.implicits._

import org.waman.multiverse._

class AbsorbedDose[A: Fractional](val value: A, val unit: AbsorbedDoseUnit)
    extends LinearQuantity[AbsorbedDose[A], A, AbsorbedDoseUnit] {

  override protected def newQuantity(value: A, unit: AbsorbedDoseUnit): AbsorbedDose[A] = new AbsorbedDose(value, unit)
}

/** None */
trait AbsorbedDoseUnit extends LinearUnit[AbsorbedDoseUnit]{

  override def getSIUnit: AbsorbedDoseUnit = AbsorbedDoseUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AbsorbedDoseUnit.dimension
}

object AbsorbedDoseUnit extends UnitInfo[AbsorbedDoseUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gray

  import AbsorbedDoseUnitObjects._

  def getUnits: Seq[AbsorbedDoseUnit] =
    Seq(gray, yoctogray, zeptogray, attogray, femtogray, picogray, nanogray, microgray, milligray, centigray, decigray, decagray, hectogray, kilogray, megagray, gigagray, teragray, petagray, exagray, zettagray, yottagray, rad, yoctorad, zeptorad, attorad, femtorad, picorad, nanorad, microrad, millirad, centirad, decirad, decarad, hectorad, kilorad, megarad, gigarad, terarad, petarad, exarad, zettarad, yottarad)
}


/** For no alias or user defined units */
class SimpleAbsorbedDoseUnit(val name: String, val symbol: String, val interval: Real) extends AbsorbedDoseUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAbsorbedDoseUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AbsorbedDoseUnit
  
object AbsorbedDoseUnitObjects{

  final case object gray extends SimpleAbsorbedDoseUnit("gray", "Gy", 1)
  final case object yoctogray extends SimpleAbsorbedDoseUnit("yoctogray", "yGy", r"1e-24")
  final case object zeptogray extends SimpleAbsorbedDoseUnit("zeptogray", "zGy", r"1e-21")
  final case object attogray extends SimpleAbsorbedDoseUnit("attogray", "aGy", r"1e-18")
  final case object femtogray extends SimpleAbsorbedDoseUnit("femtogray", "fGy", r"1e-15")
  final case object picogray extends SimpleAbsorbedDoseUnit("picogray", "pGy", r"1e-12")
  final case object nanogray extends SimpleAbsorbedDoseUnit("nanogray", "nGy", r"1e-9")
  final case object microgray extends DefaultAbsorbedDoseUnit("microgray", "μGy", Seq("mcGy"), r"1e-6")
  final case object milligray extends SimpleAbsorbedDoseUnit("milligray", "mGy", r"1e-3")
  final case object centigray extends SimpleAbsorbedDoseUnit("centigray", "cGy", r"1e-2")
  final case object decigray extends SimpleAbsorbedDoseUnit("decigray", "dGy", r"1e-1")
  final case object decagray extends SimpleAbsorbedDoseUnit("decagray", "daGy", r"1e1")
  final case object hectogray extends SimpleAbsorbedDoseUnit("hectogray", "hGy", r"1e2")
  final case object kilogray extends DefaultAbsorbedDoseUnit("kilogray", "kGy", Seq("KGy"), r"1e3")
  final case object megagray extends SimpleAbsorbedDoseUnit("megagray", "MGy", r"1e6")
  final case object gigagray extends SimpleAbsorbedDoseUnit("gigagray", "GGy", r"1e9")
  final case object teragray extends SimpleAbsorbedDoseUnit("teragray", "TGy", r"1e12")
  final case object petagray extends SimpleAbsorbedDoseUnit("petagray", "PGy", r"1e15")
  final case object exagray extends SimpleAbsorbedDoseUnit("exagray", "EGy", r"1e18")
  final case object zettagray extends SimpleAbsorbedDoseUnit("zettagray", "ZGy", r"1e21")
  final case object yottagray extends SimpleAbsorbedDoseUnit("yottagray", "YGy", r"1e24")
  final case object rad extends SimpleAbsorbedDoseUnit("rad", "rad", r"1e-2")
  final case object yoctorad extends SimpleAbsorbedDoseUnit("yoctorad", "yrad", r"1e-2" * r"1e-24")
  final case object zeptorad extends SimpleAbsorbedDoseUnit("zeptorad", "zrad", r"1e-2" * r"1e-21")
  final case object attorad extends SimpleAbsorbedDoseUnit("attorad", "arad", r"1e-2" * r"1e-18")
  final case object femtorad extends SimpleAbsorbedDoseUnit("femtorad", "frad", r"1e-2" * r"1e-15")
  final case object picorad extends SimpleAbsorbedDoseUnit("picorad", "prad", r"1e-2" * r"1e-12")
  final case object nanorad extends SimpleAbsorbedDoseUnit("nanorad", "nrad", r"1e-2" * r"1e-9")
  final case object microrad extends DefaultAbsorbedDoseUnit("microrad", "μrad", Seq("mcrad"), r"1e-2" * r"1e-6")
  final case object millirad extends SimpleAbsorbedDoseUnit("millirad", "mrad", r"1e-2" * r"1e-3")
  final case object centirad extends SimpleAbsorbedDoseUnit("centirad", "crad", r"1e-2" * r"1e-2")
  final case object decirad extends SimpleAbsorbedDoseUnit("decirad", "drad", r"1e-2" * r"1e-1")
  final case object decarad extends SimpleAbsorbedDoseUnit("decarad", "darad", r"1e-2" * r"1e1")
  final case object hectorad extends SimpleAbsorbedDoseUnit("hectorad", "hrad", r"1e-2" * r"1e2")
  final case object kilorad extends DefaultAbsorbedDoseUnit("kilorad", "krad", Seq("Krad"), r"1e-2" * r"1e3")
  final case object megarad extends SimpleAbsorbedDoseUnit("megarad", "Mrad", r"1e-2" * r"1e6")
  final case object gigarad extends SimpleAbsorbedDoseUnit("gigarad", "Grad", r"1e-2" * r"1e9")
  final case object terarad extends SimpleAbsorbedDoseUnit("terarad", "Trad", r"1e-2" * r"1e12")
  final case object petarad extends SimpleAbsorbedDoseUnit("petarad", "Prad", r"1e-2" * r"1e15")
  final case object exarad extends SimpleAbsorbedDoseUnit("exarad", "Erad", r"1e-2" * r"1e18")
  final case object zettarad extends SimpleAbsorbedDoseUnit("zettarad", "Zrad", r"1e-2" * r"1e21")
  final case object yottarad extends SimpleAbsorbedDoseUnit("yottarad", "Yrad", r"1e-2" * r"1e24")
}


object AbsorbedDoseUnits{

  /** gray */
  def Gy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gray
  /** yoctogray */
  def yGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.yoctogray
  /** zeptogray */
  def zGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.zeptogray
  /** attogray */
  def aGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.attogray
  /** femtogray */
  def fGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.femtogray
  /** picogray */
  def pGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.picogray
  /** nanogray */
  def nGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.nanogray
  /** microgray */
  def μGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.microgray
  /** microgray */
  def mcGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.microgray
  /** milligray */
  def mGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.milligray
  /** centigray */
  def cGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.centigray
  /** decigray */
  def dGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.decigray
  /** decagray */
  def daGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.decagray
  /** hectogray */
  def hGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.hectogray
  /** kilogray */
  def kGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.kilogray
  /** kilogray */
  def KGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.kilogray
  /** megagray */
  def MGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.megagray
  /** gigagray */
  def GGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gigagray
  /** teragray */
  def TGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.teragray
  /** petagray */
  def PGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.petagray
  /** exagray */
  def EGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.exagray
  /** zettagray */
  def ZGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.zettagray
  /** yottagray */
  def YGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.yottagray
  /** rad */
  def rad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.rad
  /** yoctorad */
  def yrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.yoctorad
  /** zeptorad */
  def zrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.zeptorad
  /** attorad */
  def arad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.attorad
  /** femtorad */
  def frad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.femtorad
  /** picorad */
  def prad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.picorad
  /** nanorad */
  def nrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.nanorad
  /** microrad */
  def μrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.microrad
  /** microrad */
  def mcrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.microrad
  /** millirad */
  def mrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.millirad
  /** centirad */
  def crad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.centirad
  /** decirad */
  def drad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.decirad
  /** decarad */
  def darad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.decarad
  /** hectorad */
  def hrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.hectorad
  /** kilorad */
  def krad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.kilorad
  /** kilorad */
  def Krad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.kilorad
  /** megarad */
  def Mrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.megarad
  /** gigarad */
  def Grad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gigarad
  /** terarad */
  def Trad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.terarad
  /** petarad */
  def Prad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.petarad
  /** exarad */
  def Erad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.exarad
  /** zettarad */
  def Zrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.zettarad
  /** yottarad */
  def Yrad: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.yottarad
}