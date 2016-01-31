package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait MassPostfixOps[A]{

  protected def massPostfixOps(massUnit: MassUnit): A

  def yg: A = massPostfixOps(MassUnit.YoctoGram)
  def zg: A = massPostfixOps(MassUnit.ZeptoGram)
  def ag: A = massPostfixOps(MassUnit.AttoGram)
  def fg: A = massPostfixOps(MassUnit.FemtoGram)
  def pg: A = massPostfixOps(MassUnit.PicoGram)
  def ng: A = massPostfixOps(MassUnit.NanoGram)
  def μg: A = massPostfixOps(MassUnit.MicroGram)
  def mg: A = massPostfixOps(MassUnit.MilliGram)
  def cg: A = massPostfixOps(MassUnit.CentiGram)
  def dg: A = massPostfixOps(MassUnit.DeciGram)
  def g : A = massPostfixOps(MassUnit.Gram)
  def dag: A = massPostfixOps(MassUnit.DecaGram)
  def hg: A = massPostfixOps(MassUnit.HectoGram)
  def kg: A = massPostfixOps(MassUnit.KiloGram)
  def Mg: A = massPostfixOps(MassUnit.MegaGram)
  def Gg: A = massPostfixOps(MassUnit.GigaGram)
  def Tg: A = massPostfixOps(MassUnit.TeraGram)
  def Pg: A = massPostfixOps(MassUnit.PetaGram)
  def Eg: A = massPostfixOps(MassUnit.ExaGram)
  def Zg: A = massPostfixOps(MassUnit.ZettaGram)
  def Yg: A = massPostfixOps(MassUnit.YottaGram)
}

trait MassPer[A]{

  protected def massPer(massUnit: MassUnit): A

  def yg(per: Per): A = massPer(MassUnit.YoctoGram)
  def zg(per: Per): A = massPer(MassUnit.ZeptoGram)
  def ag(per: Per): A = massPer(MassUnit.AttoGram)
  def fg(per: Per): A = massPer(MassUnit.FemtoGram)
  def pg(per: Per): A = massPer(MassUnit.PicoGram)
  def ng(per: Per): A = massPer(MassUnit.NanoGram)
  def μg(per: Per): A = massPer(MassUnit.MicroGram)
  def mg(per: Per): A = massPer(MassUnit.MilliGram)
  def cg(per: Per): A = massPer(MassUnit.CentiGram)
  def dg(per: Per): A = massPer(MassUnit.DeciGram)
  def g (per: Per): A = massPer(MassUnit.Gram)
  def dag(per: Per): A = massPer(MassUnit.DecaGram)
  def hg(per: Per): A = massPer(MassUnit.HectoGram)
  def kg(per: Per): A = massPer(MassUnit.KiloGram)
  def Mg(per: Per): A = massPer(MassUnit.MegaGram)
  def Gg(per: Per): A = massPer(MassUnit.GigaGram)
  def Tg(per: Per): A = massPer(MassUnit.TeraGram)
  def Pg(per: Per): A = massPer(MassUnit.PetaGram)
  def Eg(per: Per): A = massPer(MassUnit.ExaGram)
  def Zg(per: Per): A = massPer(MassUnit.ZettaGram)
  def Yg(per: Per): A = massPer(MassUnit.YottaGram)
}

class Mass[A: Fractional](val value: A, val unit: MassUnit)
  extends Quantity[A, MassUnit]
    with MassPostfixOps[A]
    with DivisibleBy[VolumeUnit, Density[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: MassUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInKiloGram) / real(evalUnit.unitInKiloGram)


  override protected def massPostfixOps(massUnit: MassUnit) = apply(massUnit)

  override def /(volumeUnit: VolumeUnit): Density[A] = new Density(value, unit / volumeUnit)
}

abstract class MassUnit(val symbol: String, val unitInKiloGram: Real)
  extends PhysicalUnit
    with DivisibleBy[VolumeUnit, DensityUnit]{

  override protected def baseUnit = MassUnit.KiloGram
  override protected def inBaseUnitAccessor = () => unitInKiloGram

  override def /(volumeUnit: VolumeUnit): DensityUnit = DensityUnit(this, volumeUnit)
}

object MassUnit{

  case object YoctoGram extends MassUnit("yg", r"1e-27")
  case object ZeptoGram extends MassUnit("zg", r"1e-24")
  case object AttoGram  extends MassUnit("ag", r"1e-21")
  case object FemtoGram extends MassUnit("ag", r"1e-18")
  case object PicoGram  extends MassUnit("pg", r"1e-15")
  case object NanoGram  extends MassUnit("ng", r"1e-12")
  case object MicroGram extends MassUnit("μg", r"1e-9")
  case object MilliGram extends MassUnit("mg", r"1e-6")
  case object CentiGram extends MassUnit("cg", r"1e-5")
  case object DeciGram  extends MassUnit("dg", r"1e-4")
  case object Gram      extends MassUnit("g" , r"1e-3")
  case object DecaGram  extends MassUnit("dag", r"1e-2")
  case object HectoGram extends MassUnit("hg", r"1e-1")
  case object KiloGram  extends MassUnit("kg", r"1")
  case object MegaGram  extends MassUnit("Mg", r"1e3")
  case object GigaGram  extends MassUnit("Gg", r"1e6")
  case object TeraGram  extends MassUnit("Tg", r"1e9")
  case object PetaGram  extends MassUnit("Pg", r"1e12")
  case object ExaGram   extends MassUnit("Eg", r"1e15")
  case object ZettaGram extends MassUnit("Zg", r"1e18")
  case object YottaGram extends MassUnit("Yg", r"1e21")
}

trait PredefinedMassUnit{

  val yg = MassUnit.YoctoGram
  val zg = MassUnit.ZeptoGram
  val ag = MassUnit.AttoGram
  val fg = MassUnit.FemtoGram
  val pg = MassUnit.PicoGram
  val ng = MassUnit.NanoGram
  val μg = MassUnit.MicroGram
  val mg = MassUnit.MilliGram
  val cg = MassUnit.CentiGram
  val dg = MassUnit.DeciGram
  val g  = MassUnit.Gram
  val dag = MassUnit.DecaGram
  val hg = MassUnit.HectoGram
  val kg = MassUnit.KiloGram
  val Mg = MassUnit.MegaGram
  val Gg = MassUnit.GigaGram
  val Tg = MassUnit.TeraGram
  val Pg = MassUnit.PetaGram
  val Eg = MassUnit.ExaGram
  val Zg = MassUnit.ZettaGram
  val Yg = MassUnit.YottaGram
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