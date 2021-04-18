package org.waman.multiverse.unit.defs.chem

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class AmountOfSubstance[A: Fractional](val value: A, val unit: AmountOfSubstanceUnit)
    extends LinearQuantity[AmountOfSubstance[A], A, AmountOfSubstanceUnit] {

  override protected def newQuantity(value: A, unit: AmountOfSubstanceUnit): AmountOfSubstance[A] = new AmountOfSubstance(value, unit)

  def /(time: Time[A]): Catalysis[A] = new Catalysis(this.value / time.value, this.unit / time.unit)
}

/** None */
trait AmountOfSubstanceUnit extends LinearUnit[AmountOfSubstanceUnit]{

  override def getSIUnit: AmountOfSubstanceUnit = AmountOfSubstanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AmountOfSubstanceUnit.dimension

  def /(timeUnit: TimeUnit): CatalysisUnit =
    new QuotientUnit[CatalysisUnit, AmountOfSubstanceUnit, TimeUnit](AmountOfSubstanceUnit.this, timeUnit) with CatalysisUnit
}

object AmountOfSubstanceUnit extends UnitInfo[AmountOfSubstanceUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](N -> 1).withDefaultValue(0)

  def getSIUnit: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.mole

  import AmountOfSubstanceUnitObjects._

  def getUnits: Seq[AmountOfSubstanceUnit] =
    Seq(mole, yoctomole, zeptomole, attomole, femtomole, picomole, nanomole, micromole, millimole, centimole, decimole, decamole, hectomole, kilomole, megamole, gigamole, teramole, petamole, examole, zettamole, yottamole)
}


/** For no aliase or user defined units */
class SimpleAmountOfSubstanceUnit(val name: String, val symbol: String, val interval: Real) extends AmountOfSubstanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAmountOfSubstanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AmountOfSubstanceUnit
  
object AmountOfSubstanceUnitObjects{

  final case object mole extends SimpleAmountOfSubstanceUnit("mole", "mol", 1)
  final case object yoctomole extends SimpleAmountOfSubstanceUnit("yoctomole", "ymol", r"1e-24")
  final case object zeptomole extends SimpleAmountOfSubstanceUnit("zeptomole", "zmol", r"1e-21")
  final case object attomole extends SimpleAmountOfSubstanceUnit("attomole", "amol", r"1e-18")
  final case object femtomole extends SimpleAmountOfSubstanceUnit("femtomole", "fmol", r"1e-15")
  final case object picomole extends SimpleAmountOfSubstanceUnit("picomole", "pmol", r"1e-12")
  final case object nanomole extends SimpleAmountOfSubstanceUnit("nanomole", "nmol", r"1e-9")
  final case object micromole extends DefaultAmountOfSubstanceUnit("micromole", "μmol", Seq("mcmol"), r"1e-6")
  final case object millimole extends SimpleAmountOfSubstanceUnit("millimole", "mmol", r"1e-3")
  final case object centimole extends SimpleAmountOfSubstanceUnit("centimole", "cmol", r"1e-2")
  final case object decimole extends SimpleAmountOfSubstanceUnit("decimole", "dmol", r"1e-1")
  final case object decamole extends SimpleAmountOfSubstanceUnit("decamole", "damol", r"1e1")
  final case object hectomole extends SimpleAmountOfSubstanceUnit("hectomole", "hmol", r"1e2")
  final case object kilomole extends DefaultAmountOfSubstanceUnit("kilomole", "kmol", Seq("Kmol"), r"1e3")
  final case object megamole extends SimpleAmountOfSubstanceUnit("megamole", "Mmol", r"1e6")
  final case object gigamole extends SimpleAmountOfSubstanceUnit("gigamole", "Gmol", r"1e9")
  final case object teramole extends SimpleAmountOfSubstanceUnit("teramole", "Tmol", r"1e12")
  final case object petamole extends SimpleAmountOfSubstanceUnit("petamole", "Pmol", r"1e15")
  final case object examole extends SimpleAmountOfSubstanceUnit("examole", "Emol", r"1e18")
  final case object zettamole extends SimpleAmountOfSubstanceUnit("zettamole", "Zmol", r"1e21")
  final case object yottamole extends SimpleAmountOfSubstanceUnit("yottamole", "Ymol", r"1e24")
}


object AmountOfSubstanceUnits{

  /** mole */
  def mol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.mole
  /** yoctomole */
  def ymol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.yoctomole
  /** zeptomole */
  def zmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.zeptomole
  /** attomole */
  def amol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.attomole
  /** femtomole */
  def fmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.femtomole
  /** picomole */
  def pmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.picomole
  /** nanomole */
  def nmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.nanomole
  /** micromole */
  def μmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.micromole
  /** micromole */
  def mcmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.micromole
  /** millimole */
  def mmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.millimole
  /** centimole */
  def cmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.centimole
  /** decimole */
  def dmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.decimole
  /** decamole */
  def damol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.decamole
  /** hectomole */
  def hmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.hectomole
  /** kilomole */
  def kmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.kilomole
  /** kilomole */
  def Kmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.kilomole
  /** megamole */
  def Mmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.megamole
  /** gigamole */
  def Gmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.gigamole
  /** teramole */
  def Tmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.teramole
  /** petamole */
  def Pmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.petamole
  /** examole */
  def Emol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.examole
  /** zettamole */
  def Zmol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.zettamole
  /** yottamole */
  def Ymol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.yottamole
}