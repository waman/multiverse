package org.waman.multiverse.electric

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.time._
import org.waman.multiverse.mass._
import org.waman.multiverse.energy._
import org.waman.multiverse.radiation._
import org.waman.multiverse.mechanics.VelocityUnit.SpeedOfLight

sealed trait ChargeUnit extends PhysicalUnit[ChargeUnit]
  with MultiplicativeByLengthUnit[DipoleUnit]
  with MultiplicativeByVoltageUnit[EnergyUnit]
  with DivisibleByTimeUnit[CurrentUnit]
  with DivisibleByVoltageUnit[CapacitanceUnit]
  with DivisibleByMassUnit[ExposureUnit]{

  def unitInCoulomb: Real

  override def baseUnit = org.waman.multiverse.electric.ChargeUnit.Coulomb
  override def valueInBaseUnit = unitInCoulomb

  override def *(unit: LengthUnit) = DipoleUnit(this, unit)

  override def *(unit: VoltageUnit) = EnergyUnit(this, unit)

  override def /(unit: TimeUnit) = CurrentUnit(this, unit)

  override def /(unit: VoltageUnit) = CapacitanceUnit(this, unit)

  override def /(unit: MassUnit) = ExposureUnit(this, unit)
}

object ChargeUnit extends ConstantsDefined[ChargeUnit]{

  // intrinsic
  private[ChargeUnit]
  class IntrinsicChargeUnit(name: String, val symbols: Seq[String], val unitInCoulomb: Real)
      extends ChargeUnit{

    def this(name: String, symbols: Seq[String], unit: ChargeUnit) =
      this(name, symbols, unit.unitInCoulomb)

    def this(name: String, symbols: Seq[String], factor: Real, unit: ChargeUnit) =
      this(name, symbols, factor * unit.unitInCoulomb)
  }


  case object YoctoCoulomb extends IntrinsicChargeUnit("YoctoCoulomb", Seq("yC"), r"1e-24")
  case object ZeptoCoulomb extends IntrinsicChargeUnit("ZeptoCoulomb", Seq("zC"), r"1e-21")
  case object AttoCoulomb extends IntrinsicChargeUnit("AttoCoulomb", Seq("aC"), r"1e-18")
  case object FemtoCoulomb extends IntrinsicChargeUnit("FemtoCoulomb", Seq("fC"), r"1e-15")
  case object PicoCoulomb extends IntrinsicChargeUnit("PicoCoulomb", Seq("pC"), r"1e-12")
  case object NanoCoulomb extends IntrinsicChargeUnit("NanoCoulomb", Seq("nC"), r"1e-9")
  case object MicroCoulomb extends IntrinsicChargeUnit("MicroCoulomb", Seq("μC", "mcC"), r"1e-6")
  case object MilliCoulomb extends IntrinsicChargeUnit("MilliCoulomb", Seq("mC"), r"1e-3")
  case object CentiCoulomb extends IntrinsicChargeUnit("CentiCoulomb", Seq("cC"), r"1e-2")
  case object DeciCoulomb extends IntrinsicChargeUnit("DeciCoulomb", Seq("dC"), r"1e-1")
  case object Coulomb extends IntrinsicChargeUnit("Coulomb", Seq("C"), r"1")
  case object DecaCoulomb extends IntrinsicChargeUnit("DecaCoulomb", Seq("daC"), r"1e1")
  case object HectoCoulomb extends IntrinsicChargeUnit("HectoCoulomb", Seq("hC"), r"1e2")
  case object KiloCoulomb extends IntrinsicChargeUnit("KiloCoulomb", Seq("kC"), r"1e3")
  case object MegaCoulomb extends IntrinsicChargeUnit("MegaCoulomb", Seq("MC"), r"1e6")
  case object GigaCoulomb extends IntrinsicChargeUnit("GigaCoulomb", Seq("GC"), r"1e9")
  case object TeraCoulomb extends IntrinsicChargeUnit("TeraCoulomb", Seq("TC"), r"1e12")
  case object PetaCoulomb extends IntrinsicChargeUnit("PetaCoulomb", Seq("PC"), r"1e15")
  case object ExaCoulomb extends IntrinsicChargeUnit("ExaCoulomb", Seq("EC"), r"1e18")
  case object ZettaCoulomb extends IntrinsicChargeUnit("ZettaCoulomb", Seq("ZC"), r"1e21")
  case object YottaCoulomb extends IntrinsicChargeUnit("YottaCoulomb", Seq("YC"), r"1e24")
  case object Abcoulomb extends IntrinsicChargeUnit("Abcoulomb", Seq("abC"), 10)
  case object Statcoulomb extends IntrinsicChargeUnit("Statcoulomb", Seq("statC", "esu"), r"0.1" / SpeedOfLight.unitInMetrePerSecond)
  case object ElementaryCharge extends IntrinsicChargeUnit("ElementaryCharge", Seq("e"), r"1.602176620898e-19") with NotExact

  override lazy val values = Seq(YoctoCoulomb, ZeptoCoulomb, AttoCoulomb, FemtoCoulomb, PicoCoulomb, NanoCoulomb, MicroCoulomb, MilliCoulomb, CentiCoulomb, DeciCoulomb, Coulomb, DecaCoulomb, HectoCoulomb, KiloCoulomb, MegaCoulomb, GigaCoulomb, TeraCoulomb, PetaCoulomb, ExaCoulomb, ZettaCoulomb, YottaCoulomb, Abcoulomb, Statcoulomb, ElementaryCharge)

  // CurrentUnit * TimeUnit -> Charge
  private[ChargeUnit]
  class ProductCurrentDotTimeUnit(val firstUnit: CurrentUnit, val secondUnit: TimeUnit)
      extends ChargeUnit with ProductUnit[ChargeUnit, CurrentUnit, TimeUnit]{

    override lazy val unitInCoulomb: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: CurrentUnit, unit2: TimeUnit): ChargeUnit =
    new ProductCurrentDotTimeUnit(unit1, unit2)
}

trait MultiplicativeByChargeUnit[R]{
  def *(unit: ChargeUnit): R
}

trait DivisibleByChargeUnit[R]{
  def /(unit: ChargeUnit): R
}

trait ChargePostfixOps[A]{
  import ChargeUnit._

  protected def chargePostfixOps(unit: ChargeUnit): A


  def yC : A = chargePostfixOps(YoctoCoulomb)
  def zC : A = chargePostfixOps(ZeptoCoulomb)
  def aC : A = chargePostfixOps(AttoCoulomb)
  def fC : A = chargePostfixOps(FemtoCoulomb)
  def pC : A = chargePostfixOps(PicoCoulomb)
  def nC : A = chargePostfixOps(NanoCoulomb)
  def μC : A = chargePostfixOps(MicroCoulomb)
  def mcC : A = chargePostfixOps(MicroCoulomb)
  def mC : A = chargePostfixOps(MilliCoulomb)
  def cC : A = chargePostfixOps(CentiCoulomb)
  def dC : A = chargePostfixOps(DeciCoulomb)
  def C : A = chargePostfixOps(Coulomb)
  def daC : A = chargePostfixOps(DecaCoulomb)
  def hC : A = chargePostfixOps(HectoCoulomb)
  def kC : A = chargePostfixOps(KiloCoulomb)
  def MC : A = chargePostfixOps(MegaCoulomb)
  def GC : A = chargePostfixOps(GigaCoulomb)
  def TC : A = chargePostfixOps(TeraCoulomb)
  def PC : A = chargePostfixOps(PetaCoulomb)
  def EC : A = chargePostfixOps(ExaCoulomb)
  def ZC : A = chargePostfixOps(ZettaCoulomb)
  def YC : A = chargePostfixOps(YottaCoulomb)
  def abC : A = chargePostfixOps(Abcoulomb)
  def statC : A = chargePostfixOps(Statcoulomb)
  def esu : A = chargePostfixOps(Statcoulomb)
  def e : A = chargePostfixOps(ElementaryCharge)
}

trait ChargeDot[A]{
  import ChargeUnit._

  protected def chargeDot(unit: ChargeUnit): A

  def yC(dot: Dot): A = chargeDot(YoctoCoulomb)
  def zC(dot: Dot): A = chargeDot(ZeptoCoulomb)
  def aC(dot: Dot): A = chargeDot(AttoCoulomb)
  def fC(dot: Dot): A = chargeDot(FemtoCoulomb)
  def pC(dot: Dot): A = chargeDot(PicoCoulomb)
  def nC(dot: Dot): A = chargeDot(NanoCoulomb)
  def μC(dot: Dot): A = chargeDot(MicroCoulomb)
  def mcC(dot: Dot): A = chargeDot(MicroCoulomb)
  def mC(dot: Dot): A = chargeDot(MilliCoulomb)
  def cC(dot: Dot): A = chargeDot(CentiCoulomb)
  def dC(dot: Dot): A = chargeDot(DeciCoulomb)
  def C(dot: Dot): A = chargeDot(Coulomb)
  def daC(dot: Dot): A = chargeDot(DecaCoulomb)
  def hC(dot: Dot): A = chargeDot(HectoCoulomb)
  def kC(dot: Dot): A = chargeDot(KiloCoulomb)
  def MC(dot: Dot): A = chargeDot(MegaCoulomb)
  def GC(dot: Dot): A = chargeDot(GigaCoulomb)
  def TC(dot: Dot): A = chargeDot(TeraCoulomb)
  def PC(dot: Dot): A = chargeDot(PetaCoulomb)
  def EC(dot: Dot): A = chargeDot(ExaCoulomb)
  def ZC(dot: Dot): A = chargeDot(ZettaCoulomb)
  def YC(dot: Dot): A = chargeDot(YottaCoulomb)
  def abC(dot: Dot): A = chargeDot(Abcoulomb)
  def statC(dot: Dot): A = chargeDot(Statcoulomb)
  def esu(dot: Dot): A = chargeDot(Statcoulomb)
  def e(dot: Dot): A = chargeDot(ElementaryCharge)
}

trait ChargePer[A]{
  import ChargeUnit._

  protected def chargePer(unit: ChargeUnit): A

  def yC(per: Per): A = chargePer(YoctoCoulomb)
  def zC(per: Per): A = chargePer(ZeptoCoulomb)
  def aC(per: Per): A = chargePer(AttoCoulomb)
  def fC(per: Per): A = chargePer(FemtoCoulomb)
  def pC(per: Per): A = chargePer(PicoCoulomb)
  def nC(per: Per): A = chargePer(NanoCoulomb)
  def μC(per: Per): A = chargePer(MicroCoulomb)
  def mcC(per: Per): A = chargePer(MicroCoulomb)
  def mC(per: Per): A = chargePer(MilliCoulomb)
  def cC(per: Per): A = chargePer(CentiCoulomb)
  def dC(per: Per): A = chargePer(DeciCoulomb)
  def C(per: Per): A = chargePer(Coulomb)
  def daC(per: Per): A = chargePer(DecaCoulomb)
  def hC(per: Per): A = chargePer(HectoCoulomb)
  def kC(per: Per): A = chargePer(KiloCoulomb)
  def MC(per: Per): A = chargePer(MegaCoulomb)
  def GC(per: Per): A = chargePer(GigaCoulomb)
  def TC(per: Per): A = chargePer(TeraCoulomb)
  def PC(per: Per): A = chargePer(PetaCoulomb)
  def EC(per: Per): A = chargePer(ExaCoulomb)
  def ZC(per: Per): A = chargePer(ZettaCoulomb)
  def YC(per: Per): A = chargePer(YottaCoulomb)
  def abC(per: Per): A = chargePer(Abcoulomb)
  def statC(per: Per): A = chargePer(Statcoulomb)
  def esu(per: Per): A = chargePer(Statcoulomb)
  def e(per: Per): A = chargePer(ElementaryCharge)
}

trait PredefinedChargeUnit extends ChargePostfixOps[ChargeUnit]{
  override protected def chargePostfixOps(unit: ChargeUnit) = unit
  
}

object PredefinedChargeUnit extends PredefinedChargeUnit
