package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AreaPostfixOps[A]{
  def fm2: A
  def pm2: A
  def nm2: A
  def μm2: A
  def mm2: A
  def cm2: A
  def m2 : A
  def a  : A
  def ha : A
  def km2: A
  def Mm2: A
  def Gm2: A
  def Tm2: A

  // microscopic
  def yb: A
  def zb: A
  def ab: A
  def fb: A
  def pb: A
  def nb: A
  def μb: A
  def mb: A
  def b : A
  def kb: A
  def Mb: A

  // yard-pond
  def ac: A
}

class Area[A: Fractional](val value: A, val unit: AreaUnit)
  extends Quantity[A, AreaUnit]
    with AreaPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AreaUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.inSquareMetre) / real(evalUnit.inSquareMetre)

  override def fm2: A = apply(AreaUnit.SquareFemtoMetre)
  override def pm2: A = apply(AreaUnit.SquarePicoMetre)
  override def nm2: A = apply(AreaUnit.SquareNanoMetre)
  override def μm2: A = apply(AreaUnit.SquareMicroMetre)
  override def mm2: A = apply(AreaUnit.SquareMilliMetre)
  override def cm2: A = apply(AreaUnit.SquareCentiMetre)
  override def m2 : A = apply(AreaUnit.SquareMetre)
  override def a  : A = apply(AreaUnit.Are)
  override def ha : A = apply(AreaUnit.Hectare)
  override def km2: A = apply(AreaUnit.SquareKiloMetre)
  override def Mm2: A = apply(AreaUnit.SquareMegaMetre)
  override def Gm2: A = apply(AreaUnit.SquareGigaMetre)
  override def Tm2: A = apply(AreaUnit.SquareTeraMetre)

  override def yb: A = apply(AreaUnit.YoctoBarn)
  override def zb: A = apply(AreaUnit.ZeptoBarn)
  override def ab: A = apply(AreaUnit.AttoBarn)
  override def fb: A = apply(AreaUnit.FemtoBarn)
  override def pb: A = apply(AreaUnit.PicoBarn)
  override def nb: A = apply(AreaUnit.NanoBarn)
  override def μb: A = apply(AreaUnit.MicroBarn)
  override def mb: A = apply(AreaUnit.MilliBarn)
  override def b : A = apply(AreaUnit.Barn)
  override def kb: A = apply(AreaUnit.KiloBarn)
  override def Mb: A = apply(AreaUnit.MegaBarn)

  override def ac: A = apply(AreaUnit.Acre)
}

abstract class AreaUnit(val name: String, val symbol: String, val inSquareMetre: Real)
  extends PhysicalUnit

object AreaUnit{
  case object SquareFemtoMetre extends AreaUnit("SquareFemtoMetre" , "fm2", r"1e-30")
  case object SquarePicoMetre  extends AreaUnit("SquarePicoMetre"  , "pm2", r"1e-24")
  case object SquareNanoMetre  extends AreaUnit("SquareNanoMetre"  , "nm2", r"1e-18")
  case object SquareMicroMetre extends AreaUnit("SquareMicroMetre" , "μm2", r"1e-12")
  case object SquareMilliMetre extends AreaUnit("SquareMilliMetre" , "mm2", r"1e-6")
  case object SquareCentiMetre extends AreaUnit("SquareCentiMetre" , "cm2", r"1e-4")
  case object SquareMetre      extends AreaUnit("SquareMetre"      , "m2" , r"1")
  case object Are              extends AreaUnit("Are"              , "a"  , r"1e2")
  case object Hectare          extends AreaUnit("Hectare"          , "ha" , r"1e4")
  case object SquareKiloMetre  extends AreaUnit("SquareKiloMetre"  , "km2", r"1e6")
  case object SquareMegaMetre  extends AreaUnit("SquareMegaMetre"  , "Mm2", r"1e12")
  case object SquareGigaMetre  extends AreaUnit("SquareGigaMetre"  , "Gm2", r"1e18")
  case object SquareTeraMetre  extends AreaUnit("SquareTeraMetre"  , "Tm2", r"1e24")

  case object YoctoBarn extends AreaUnit("YoctoBarn", "yb", r"1e-52")
  case object ZeptoBarn extends AreaUnit("ZeptoBarn", "zb", r"1e-49")
  case object AttoBarn  extends AreaUnit("AttoBarn" , "ab", r"1e-46")
  case object FemtoBarn extends AreaUnit("FemtoBarn", "fb", r"1e-43")
  case object PicoBarn  extends AreaUnit("PicoBarn" , "pb", r"1e-40")
  case object NanoBarn  extends AreaUnit("NanoBarn" , "nb", r"1e-37")
  case object MicroBarn extends AreaUnit("MicroBarn", "μb", r"1e-34")
  case object MilliBarn extends AreaUnit("MilliBarn", "mb", r"1e-31")
  case object Barn      extends AreaUnit("Barn"     , "b" , r"1e-28")
  case object KiloBarn  extends AreaUnit("KiloBarn", "kb", r"1e-25")
  case object MegaBarn  extends AreaUnit("MegaBarn" , "Mb", r"1e-22")

  case object Acre extends AreaUnit("Acre", "ac", r"4046.8564224")
}

trait PredefinedAreaUnit{
  val fm2 = AreaUnit.SquareFemtoMetre
  val pm2 = AreaUnit.SquarePicoMetre
  val nm2 = AreaUnit.SquareNanoMetre
  val μm2 = AreaUnit.SquareMicroMetre
  val mm2 = AreaUnit.SquareMilliMetre
  val cm2 = AreaUnit.SquareCentiMetre
  val m2  = AreaUnit.SquareMetre
  val a   = AreaUnit.Are
  val ha  = AreaUnit.Hectare
  val km2 = AreaUnit.SquareKiloMetre
  val Mm2 = AreaUnit.SquareMegaMetre
  val Gm2 = AreaUnit.SquareGigaMetre
  val Tm2 = AreaUnit.SquareTeraMetre

  val yb = AreaUnit.YoctoBarn
  val zb = AreaUnit.ZeptoBarn
  val ab = AreaUnit.AttoBarn
  val fb = AreaUnit.FemtoBarn
  val pb = AreaUnit.PicoBarn
  val nb = AreaUnit.NanoBarn
  val μb = AreaUnit.MicroBarn
  val mb = AreaUnit.MilliBarn
  val b  = AreaUnit.Barn
  val kb = AreaUnit.KiloBarn
  val Mb = AreaUnit.MegaBarn

  val ac = AreaUnit.Acre
}

object PredefinedAreaUnit extends PredefinedAreaUnit

trait AreaUnitInterpreter[A]
  extends AreaPostfixOps[Area[A]]{

  def apply(unit: AreaUnit): Area[A]

  override def fm2: Area[A] = apply(AreaUnit.SquareFemtoMetre)
  override def pm2: Area[A] = apply(AreaUnit.SquarePicoMetre)
  override def nm2: Area[A] = apply(AreaUnit.SquareNanoMetre)
  override def μm2: Area[A] = apply(AreaUnit.SquareMicroMetre)
  override def mm2: Area[A] = apply(AreaUnit.SquareMilliMetre)
  override def cm2: Area[A] = apply(AreaUnit.SquareCentiMetre)
  override def m2 : Area[A] = apply(AreaUnit.SquareMetre)
  override def a  : Area[A] = apply(AreaUnit.Are)
  override def ha : Area[A] = apply(AreaUnit.Hectare)
  override def km2: Area[A] = apply(AreaUnit.SquareKiloMetre)
  override def Mm2: Area[A] = apply(AreaUnit.SquareMegaMetre)
  override def Gm2: Area[A] = apply(AreaUnit.SquareGigaMetre)
  override def Tm2: Area[A] = apply(AreaUnit.SquareTeraMetre)

  // microscopic
  override def yb: Area[A] = apply(AreaUnit.YoctoBarn)
  override def zb: Area[A] = apply(AreaUnit.ZeptoBarn)
  override def ab: Area[A] = apply(AreaUnit.AttoBarn)
  override def fb: Area[A] = apply(AreaUnit.FemtoBarn)
  override def pb: Area[A] = apply(AreaUnit.PicoBarn)
  override def nb: Area[A] = apply(AreaUnit.NanoBarn)
  override def μb: Area[A] = apply(AreaUnit.MicroBarn)
  override def mb: Area[A] = apply(AreaUnit.MilliBarn)
  override def b : Area[A] = apply(AreaUnit.Barn)
  override def kb: Area[A] = apply(AreaUnit.KiloBarn)
  override def Mb: Area[A] = apply(AreaUnit.MegaBarn)

  // yard-pond
  override def ac: Area[A] = apply(AreaUnit.Acre)
}