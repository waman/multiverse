package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.mass.MassUnit
import org.waman.multiverse.metric.LengthUnit
import org.waman.multiverse.radiation.ExposureUnit
import spire.implicits._
import spire.math.Real

sealed trait ChargeUnit extends PhysicalUnit[ChargeUnit]
  with MultiplicativeByLengthUnit[DipoleUnit]
  with DivisibleByMassUnit[ExposureUnit]{

  def unitInCoulomb: Real

  override def baseUnit = org.waman.multiverse.electric.ChargeUnit.Coulomb
  override def valueInBaseUnit = unitInCoulomb

  override def *(unit: LengthUnit) = DipoleUnit(this, unit)

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
    
  case object MicroCoulomb extends IntrinsicChargeUnit("MicroCoulomb", Seq("microCoulomb", "microC", "μC"), r"1e-6")
    
  case object MilliCoulomb extends IntrinsicChargeUnit("MilliCoulomb", Seq("mC"), r"1e-3")
    
  case object CentiCoulomb extends IntrinsicChargeUnit("CentiCoulomb", Seq("cC"), r"1e-2")
    
  case object DeciCoulomb extends IntrinsicChargeUnit("DeciCoulomb", Seq("dC"), r"1e-1")
    
  case object Coulomb extends IntrinsicChargeUnit("Coulomb", Seq("C"), r"1")
    
  case object DecaCoulomb extends IntrinsicChargeUnit("DecaCoulomb", Seq("daC"), r"1e-1")
    
  case object HectoCoulomb extends IntrinsicChargeUnit("HectoCoulomb", Seq("hC"), r"1e-2")
    
  case object KiloCoulomb extends IntrinsicChargeUnit("KiloCoulomb", Seq("kC"), r"1e-3")
    
  case object MegaCoulomb extends IntrinsicChargeUnit("MegaCoulomb", Seq("MC"), r"1e-6")
    
  case object GigaCoulomb extends IntrinsicChargeUnit("GigaCoulomb", Seq("GC"), r"1e-9")
    
  case object TeraCoulomb extends IntrinsicChargeUnit("TeraCoulomb", Seq("TC"), r"1e-12")
    
  case object PetaCoulomb extends IntrinsicChargeUnit("PetaCoulomb", Seq("PC"), r"1e-15")
    
  case object ExaCoulomb extends IntrinsicChargeUnit("ExaCoulomb", Seq("EC"), r"1e-18")
    
  case object ZettaCoulomb extends IntrinsicChargeUnit("ZettaCoulomb", Seq("ZC"), r"1e-21")
    
  case object YottaCoulomb extends IntrinsicChargeUnit("YottaCoulomb", Seq("YC"), r"1e-24")
    
  case object ElementaryCharge extends IntrinsicChargeUnit("ElementaryCharge", Seq(), 1.602176462e-19) with NotExact
    

  override lazy val values = Seq(YoctoCoulomb, ZeptoCoulomb, AttoCoulomb, FemtoCoulomb, PicoCoulomb, NanoCoulomb, MicroCoulomb, MilliCoulomb, CentiCoulomb, DeciCoulomb, Coulomb, DecaCoulomb, HectoCoulomb, KiloCoulomb, MegaCoulomb, GigaCoulomb, TeraCoulomb, PetaCoulomb, ExaCoulomb, ZettaCoulomb, YottaCoulomb, ElementaryCharge)
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
  def microCoulomb : A = chargePostfixOps(MicroCoulomb)
  def microC : A = chargePostfixOps(MicroCoulomb)
  def μC : A = chargePostfixOps(MicroCoulomb)
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
  def microCoulomb(dot: Dot): A = chargeDot(MicroCoulomb)
  def microC(dot: Dot): A = chargeDot(MicroCoulomb)
  def μC(dot: Dot): A = chargeDot(MicroCoulomb)
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
  def microCoulomb(per: Per): A = chargePer(MicroCoulomb)
  def microC(per: Per): A = chargePer(MicroCoulomb)
  def μC(per: Per): A = chargePer(MicroCoulomb)
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
}

trait PredefinedChargeUnit extends ChargePostfixOps[ChargeUnit]{
  override protected def chargePostfixOps(unit: ChargeUnit) = unit
  
}

object PredefinedChargeUnit extends PredefinedChargeUnit
