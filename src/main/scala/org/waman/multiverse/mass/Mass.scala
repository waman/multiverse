package org.waman.multiverse.mass

import org.waman.multiverse._
import org.waman.multiverse.metric.{VolumePostfixOps, VolumeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait MassPostfixOps[A]{

  import MassUnit._

  protected def massPostfixOps(massUnit: MassUnit): A

  def yg: A = massPostfixOps(YoctoGram)
  def zg: A = massPostfixOps(ZeptoGram)
  def ag: A = massPostfixOps(AttoGram)
  def fg: A = massPostfixOps(FemtoGram)
  def pg: A = massPostfixOps(PicoGram)
  def ng: A = massPostfixOps(NanoGram)
  def μg: A = massPostfixOps(MicroGram)
  def mg: A = massPostfixOps(MilliGram)
  def cg: A = massPostfixOps(CentiGram)
  def dg: A = massPostfixOps(DeciGram)
  def g : A = massPostfixOps(Gram)
  def dag: A = massPostfixOps(DecaGram)
  def hg: A = massPostfixOps(HectoGram)
  def kg: A = massPostfixOps(KiloGram)
  def Mg: A = massPostfixOps(MegaGram)
  def Gg: A = massPostfixOps(GigaGram)
  def Tg: A = massPostfixOps(TeraGram)
  def Pg: A = massPostfixOps(PetaGram)
  def Eg: A = massPostfixOps(ExaGram)
  def Zg: A = massPostfixOps(ZettaGram)
  def Yg: A = massPostfixOps(YottaGram)

  def grain: A = massPostfixOps(Grain)
}

trait MassPer[A]{
  import MassUnit._

  protected def massPer(massUnit: MassUnit): A

  def yg(per: Per): A = massPer(YoctoGram)
  def zg(per: Per): A = massPer(ZeptoGram)
  def ag(per: Per): A = massPer(AttoGram)
  def fg(per: Per): A = massPer(FemtoGram)
  def pg(per: Per): A = massPer(PicoGram)
  def ng(per: Per): A = massPer(NanoGram)
  def μg(per: Per): A = massPer(MicroGram)
  def mg(per: Per): A = massPer(MilliGram)
  def cg(per: Per): A = massPer(CentiGram)
  def dg(per: Per): A = massPer(DeciGram)
  def g (per: Per): A = massPer(Gram)
  def dag(per: Per): A = massPer(DecaGram)
  def hg(per: Per): A = massPer(HectoGram)
  def kg(per: Per): A = massPer(KiloGram)
  def Mg(per: Per): A = massPer(MegaGram)
  def Gg(per: Per): A = massPer(GigaGram)
  def Tg(per: Per): A = massPer(TeraGram)
  def Pg(per: Per): A = massPer(PetaGram)
  def Eg(per: Per): A = massPer(ExaGram)
  def Zg(per: Per): A = massPer(ZettaGram)
  def Yg(per: Per): A = massPer(YottaGram)
}

class Mass[A: Fractional](val value: A, val unit: MassUnit)
  extends Quantity[A, MassUnit]
    with MassPostfixOps[A]
    with DivisibleByVolume[Density[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: MassUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInKiloGram) / real(evalUnit.unitInKiloGram)


  override protected def massPostfixOps(massUnit: MassUnit) = apply(massUnit)

  override def /(volumeUnit: VolumeUnit): Density[A] = new Density(value, unit / volumeUnit)
}

abstract class MassUnit(val symbol: String, val unitInKiloGram: Real)
  extends PhysicalUnit[MassUnit] with DivisibleByVolume[DensityUnit]{

  def this(symbol: String, factor: Real, massUnit: MassUnit) =
    this(symbol, factor * massUnit.unitInKiloGram)

  override val baseUnit = MassUnit.KiloGram
  override val inBaseUnitAccessor = () => unitInKiloGram

  override def /(volumeUnit: VolumeUnit): DensityUnit = DensityUnit(this, volumeUnit)
}

object MassUnit{

  case object YoctoGram extends MassUnit("yg", r"1e-24", Gram)
  case object ZeptoGram extends MassUnit("zg", r"1e-21", Gram)
  case object AttoGram  extends MassUnit("ag", r"1e-18", Gram)
  case object FemtoGram extends MassUnit("ag", r"1e-15", Gram)
  case object PicoGram  extends MassUnit("pg", r"1e-12", Gram)
  case object NanoGram  extends MassUnit("ng", r"1e-9", Gram)
  case object MicroGram extends MassUnit("μg", r"1e-6", Gram)
  case object MilliGram extends MassUnit("mg", r"1e-3", Gram)
  case object CentiGram extends MassUnit("cg", r"1e-2", Gram)
  case object DeciGram  extends MassUnit("dg", r"1e-1", Gram)
  case object Gram      extends MassUnit("g" , r"1e-3")
  case object DecaGram  extends MassUnit("dag", r"1e1", Gram)
  case object HectoGram extends MassUnit("hg", r"1e2", Gram)
  case object KiloGram  extends MassUnit("kg", r"1")
  case object MegaGram  extends MassUnit("Mg", r"1e6", Gram)
  case object GigaGram  extends MassUnit("Gg", r"1e9", Gram)
  case object TeraGram  extends MassUnit("Tg", r"1e12", Gram)
  case object PetaGram  extends MassUnit("Pg", r"1e15", Gram)
  case object ExaGram   extends MassUnit("Eg", r"1e18", Gram)
  case object ZettaGram extends MassUnit("Zg", r"1e21", Gram)
  case object YottaGram extends MassUnit("Yg", r"1e24", Gram)

  case object Grain extends MassUnit("gr", r"6479891", MilliGram)
  case object Carat extends MassUnit("kt", r"3" + r"1/6", Grain)
  case object Carat_Metric extends MassUnit("ct", r"200", MilliGram)
  case object Ounce extends MassUnit("oz", r"28", Gram)
  case object Pound extends MassUnit("lb", r"0.45359237")
  case object Tonne extends MassUnit("t", r"1000")

  case object ElectronVolt extends MassUnit("eV", r"1.78266184e-36") with NotExact
}

trait PredefinedMassUnit extends MassPostfixOps[MassUnit]{

  override protected def massPostfixOps(massUnit: MassUnit) = massUnit
}

object PredefinedMassUnit extends PredefinedMassUnit

trait MassUnitInterpreter[A]
  extends MassPostfixOps[Mass[A]]
    with MassPer[VolumePostfixOps[Density[A]]]{

  def apply(unit: MassUnit): Mass[A]

  override protected def massPostfixOps(massUnit: MassUnit) = apply(massUnit)

  override protected def massPer(massUnit: MassUnit) = newMassPer(massUnit)

  protected def newMassPer(massUnit: MassUnit): VolumePostfixOps[Density[A]] =
    new VolumePostfixOps[Density[A]] {

      override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(massUnit / volumeUnit)
    }

  def apply(unit: DensityUnit): Density[A]
}