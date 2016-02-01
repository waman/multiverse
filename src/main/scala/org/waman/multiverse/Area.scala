package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AreaPostfixOps[A]{

  def ym2: A
  def zm2: A
  def am2: A
  def fm2: A
  def pm2: A
  def nm2: A
  def μm2: A
  def mm2: A
  def cm2: A
  def dm2: A
  def m2 : A
  def dam2: A
  def hm2: A
  def km2: A
  def Mm2: A
  def Gm2: A
  def Tm2: A
  def Pm2: A
  def Em2: A
  def Zm2: A
  def Ym2: A

  def a  : A
  def ha : A

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
  def Gb: A
  def Tb: A
  def Pb: A
  def Eb: A
  def Zb: A
  def Yb: A

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
    else value * real(unit.unitInSquareMetre) / real(evalUnit.unitInSquareMetre)

  override def ym2 = apply(AreaUnit.SquareYoctoMetre)
  override def zm2 = apply(AreaUnit.SquareZeptoMetre)
  override def am2 = apply(AreaUnit.SquareAttoMetre)
  override def fm2 = apply(AreaUnit.SquareFemtoMetre)
  override def pm2 = apply(AreaUnit.SquarePicoMetre)
  override def nm2 = apply(AreaUnit.SquareNanoMetre)
  override def μm2 = apply(AreaUnit.SquareMicroMetre)
  override def mm2 = apply(AreaUnit.SquareMilliMetre)
  override def cm2 = apply(AreaUnit.SquareCentiMetre)
  override def dm2 = apply(AreaUnit.SquareDeciMetre)
  override def m2  = apply(AreaUnit.SquareMetre)
  override def dam2 = apply(AreaUnit.SquareDecaMetre)
  override def hm2 = apply(AreaUnit.SquareHectoMetre)
  override def km2 = apply(AreaUnit.SquareKiloMetre)
  override def Mm2 = apply(AreaUnit.SquareMegaMetre)
  override def Gm2 = apply(AreaUnit.SquareGigaMetre)
  override def Tm2 = apply(AreaUnit.SquareTeraMetre)
  override def Pm2 = apply(AreaUnit.SquarePetaMetre)
  override def Em2 = apply(AreaUnit.SquareExaMetre)
  override def Zm2 = apply(AreaUnit.SquareZettaMetre)
  override def Ym2 = apply(AreaUnit.SquareYottaMetre)

  override def a   = apply(AreaUnit.Are)
  override def ha  = apply(AreaUnit.Hectare)

  override def yb = apply(AreaUnit.YoctoBarn)
  override def zb = apply(AreaUnit.ZeptoBarn)
  override def ab = apply(AreaUnit.AttoBarn)
  override def fb = apply(AreaUnit.FemtoBarn)
  override def pb = apply(AreaUnit.PicoBarn)
  override def nb = apply(AreaUnit.NanoBarn)
  override def μb = apply(AreaUnit.MicroBarn)
  override def mb = apply(AreaUnit.MilliBarn)
  override def b  = apply(AreaUnit.Barn)
  override def kb = apply(AreaUnit.KiloBarn)
  override def Mb = apply(AreaUnit.MegaBarn)
  override def Gb = apply(AreaUnit.GigaBarn)
  override def Tb = apply(AreaUnit.TeraBarn)
  override def Pb = apply(AreaUnit.PetaBarn)
  override def Eb = apply(AreaUnit.ExaBarn)
  override def Zb = apply(AreaUnit.ZettaBarn)
  override def Yb = apply(AreaUnit.YottaBarn)

  override def ac = apply(AreaUnit.Acre)
}

abstract class AreaUnit(val symbol: String, val unitInSquareMetre: Real)
  extends PhysicalUnit {

  override protected val baseUnit = AreaUnit.SquareMetre
  override protected val inBaseUnitAccessor = () => unitInSquareMetre
}

object AreaUnit{

  case object SquareYoctoMetre extends AreaUnit("ym2", r"1e-48")
  case object SquareZeptoMetre extends AreaUnit("zm2", r"1e-42")
  case object SquareAttoMetre  extends AreaUnit("am2", r"1e-36")
  case object SquareFemtoMetre extends AreaUnit("fm2", r"1e-30")
  case object SquarePicoMetre  extends AreaUnit("pm2", r"1e-24")
  case object SquareNanoMetre  extends AreaUnit("nm2", r"1e-18")
  case object SquareMicroMetre extends AreaUnit("μm2", r"1e-12")
  case object SquareMilliMetre extends AreaUnit("mm2", r"1e-6")
  case object SquareCentiMetre extends AreaUnit("cm2", r"1e-4")
  case object SquareDeciMetre  extends AreaUnit("dm2", r"1e-2")
  case object SquareMetre      extends AreaUnit("m2" , r"1")
  case object SquareDecaMetre  extends AreaUnit("dam2", r"1e2")
  case object SquareHectoMetre extends AreaUnit("hm2", r"1e4")
  case object SquareKiloMetre  extends AreaUnit("km2", r"1e6")
  case object SquareMegaMetre  extends AreaUnit("Mm2", r"1e12")
  case object SquareGigaMetre  extends AreaUnit("Gm2", r"1e18")
  case object SquareTeraMetre  extends AreaUnit("Tm2", r"1e24")
  case object SquarePetaMetre  extends AreaUnit("Pm2", r"1e30")
  case object SquareExaMetre   extends AreaUnit("Em2", r"1e36")
  case object SquareZettaMetre extends AreaUnit("Zm2", r"1e42")
  case object SquareYottaMetre extends AreaUnit("Ym2", r"1e48")

  case object Are              extends AreaUnit("a"  , r"1e2")
  case object Hectare          extends AreaUnit("ha" , r"1e4")

  case object YoctoBarn extends AreaUnit("yb", r"1e-52")
  case object ZeptoBarn extends AreaUnit("zb", r"1e-49")
  case object AttoBarn  extends AreaUnit("ab", r"1e-46")
  case object FemtoBarn extends AreaUnit("fb", r"1e-43")
  case object PicoBarn  extends AreaUnit("pb", r"1e-40")
  case object NanoBarn  extends AreaUnit("nb", r"1e-37")
  case object MicroBarn extends AreaUnit("μb", r"1e-34")
  case object MilliBarn extends AreaUnit("mb", r"1e-31")
  case object Barn      extends AreaUnit("b" , r"1e-28")
  case object KiloBarn  extends AreaUnit("kb", r"1e-25")
  case object MegaBarn  extends AreaUnit("Mb", r"1e-22")
  case object GigaBarn  extends AreaUnit("Mb", r"1e-19")
  case object TeraBarn  extends AreaUnit("Mb", r"1e-16")
  case object PetaBarn  extends AreaUnit("Pb", r"1e-13")
  case object ExaBarn   extends AreaUnit("Eb", r"1e-10")
  case object ZettaBarn extends AreaUnit("Zb", r"1e-7")
  case object YottaBarn extends AreaUnit("Yb", r"1e-4")

  case object Acre extends AreaUnit("ac", r"4046.8564224")
}

trait PredefinedAreaUnit{

  val ym2 = AreaUnit.SquareYoctoMetre
  val zm2 = AreaUnit.SquareZeptoMetre
  val am2 = AreaUnit.SquareAttoMetre
  val fm2 = AreaUnit.SquareFemtoMetre
  val pm2 = AreaUnit.SquarePicoMetre
  val nm2 = AreaUnit.SquareNanoMetre
  val μm2 = AreaUnit.SquareMicroMetre
  val mm2 = AreaUnit.SquareMilliMetre
  val cm2 = AreaUnit.SquareCentiMetre
  val dm2 = AreaUnit.SquareDeciMetre
  val m2  = AreaUnit.SquareMetre
  val dam2 = AreaUnit.SquareDecaMetre
  val hm2 = AreaUnit.SquareHectoMetre
  val km2 = AreaUnit.SquareKiloMetre
  val Mm2 = AreaUnit.SquareMegaMetre
  val Gm2 = AreaUnit.SquareGigaMetre
  val Tm2 = AreaUnit.SquareTeraMetre
  val Pm2 = AreaUnit.SquarePetaMetre
  val Em2 = AreaUnit.SquareExaMetre
  val Zm2 = AreaUnit.SquareZettaMetre
  val Ym2 = AreaUnit.SquareYottaMetre

  val a   = AreaUnit.Are
  val ha  = AreaUnit.Hectare

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
  val Gb = AreaUnit.GigaBarn
  val Tb = AreaUnit.TeraBarn
  val Pb = AreaUnit.PetaBarn
  val Eb = AreaUnit.ExaBarn
  val Zb = AreaUnit.ZettaBarn
  val Yb = AreaUnit.YottaBarn

  val ac = AreaUnit.Acre
}

object PredefinedAreaUnit extends PredefinedAreaUnit

trait AreaUnitInterpreter[A]
  extends AreaPostfixOps[Area[A]]{

  def apply(unit: AreaUnit): Area[A]

  override def ym2 = apply(AreaUnit.SquareYoctoMetre)
  override def zm2 = apply(AreaUnit.SquareZeptoMetre)
  override def am2 = apply(AreaUnit.SquareAttoMetre)
  override def fm2 = apply(AreaUnit.SquareFemtoMetre)
  override def pm2 = apply(AreaUnit.SquarePicoMetre)
  override def nm2 = apply(AreaUnit.SquareNanoMetre)
  override def μm2 = apply(AreaUnit.SquareMicroMetre)
  override def mm2 = apply(AreaUnit.SquareMilliMetre)
  override def cm2 = apply(AreaUnit.SquareCentiMetre)
  override def dm2 = apply(AreaUnit.SquareDeciMetre)
  override def m2  = apply(AreaUnit.SquareMetre)
  override def dam2 = apply(AreaUnit.SquareDecaMetre)
  override def hm2 = apply(AreaUnit.SquareHectoMetre)
  override def km2 = apply(AreaUnit.SquareKiloMetre)
  override def Mm2 = apply(AreaUnit.SquareMegaMetre)
  override def Gm2 = apply(AreaUnit.SquareGigaMetre)
  override def Tm2 = apply(AreaUnit.SquareTeraMetre)
  override def Pm2 = apply(AreaUnit.SquarePetaMetre)
  override def Em2 = apply(AreaUnit.SquareExaMetre)
  override def Zm2 = apply(AreaUnit.SquareZettaMetre)
  override def Ym2 = apply(AreaUnit.SquareYottaMetre)

  override def a   = apply(AreaUnit.Are)
  override def ha  = apply(AreaUnit.Hectare)

  // microscopic
  override def yb = apply(AreaUnit.YoctoBarn)
  override def zb = apply(AreaUnit.ZeptoBarn)
  override def ab = apply(AreaUnit.AttoBarn)
  override def fb = apply(AreaUnit.FemtoBarn)
  override def pb = apply(AreaUnit.PicoBarn)
  override def nb = apply(AreaUnit.NanoBarn)
  override def μb = apply(AreaUnit.MicroBarn)
  override def mb = apply(AreaUnit.MilliBarn)
  override def b  = apply(AreaUnit.Barn)
  override def kb = apply(AreaUnit.KiloBarn)
  override def Mb = apply(AreaUnit.MegaBarn)
  override def Gb = apply(AreaUnit.GigaBarn)
  override def Tb = apply(AreaUnit.TeraBarn)
  override def Pb = apply(AreaUnit.PetaBarn)
  override def Eb = apply(AreaUnit.ExaBarn)
  override def Zb = apply(AreaUnit.ZettaBarn)
  override def Yb = apply(AreaUnit.YottaBarn)

  // yard-pond
  override def ac = apply(AreaUnit.Acre)
}