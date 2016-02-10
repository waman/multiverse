package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AreaPostfixOps[A]{

  protected def areaPostfixOps(areaUnit: AreaUnit): A

  def ym2: A = areaPostfixOps(AreaUnit.SquareYoctoMetre)
  def zm2: A = areaPostfixOps(AreaUnit.SquareZeptoMetre)
  def am2: A = areaPostfixOps(AreaUnit.SquareAttoMetre)
  def fm2: A = areaPostfixOps(AreaUnit.SquareFemtoMetre)
  def pm2: A = areaPostfixOps(AreaUnit.SquarePicoMetre)
  def nm2: A = areaPostfixOps(AreaUnit.SquareNanoMetre)
  def μm2: A = areaPostfixOps(AreaUnit.SquareMicroMetre)
  def mm2: A = areaPostfixOps(AreaUnit.SquareMilliMetre)
  def cm2: A = areaPostfixOps(AreaUnit.SquareCentiMetre)
  def dm2: A = areaPostfixOps(AreaUnit.SquareDeciMetre)
  def m2 : A = areaPostfixOps(AreaUnit.SquareMetre)
  def dam2: A = areaPostfixOps(AreaUnit.SquareDecaMetre)
  def hm2: A = areaPostfixOps(AreaUnit.SquareHectoMetre)
  def km2: A = areaPostfixOps(AreaUnit.SquareKiloMetre)
  def Mm2: A = areaPostfixOps(AreaUnit.SquareMegaMetre)
  def Gm2: A = areaPostfixOps(AreaUnit.SquareGigaMetre)
  def Tm2: A = areaPostfixOps(AreaUnit.SquareTeraMetre)
  def Pm2: A = areaPostfixOps(AreaUnit.SquarePetaMetre)
  def Em2: A = areaPostfixOps(AreaUnit.SquareExaMetre)
  def Zm2: A = areaPostfixOps(AreaUnit.SquareZettaMetre)
  def Ym2: A = areaPostfixOps(AreaUnit.SquareYottaMetre)

  def a : A = areaPostfixOps(AreaUnit.Are)
  def ha: A = areaPostfixOps(AreaUnit.Hectare)

  // microscopic
  def yb: A = areaPostfixOps(AreaUnit.YoctoBarn)
  def zb: A = areaPostfixOps(AreaUnit.ZeptoBarn)
  def ab: A = areaPostfixOps(AreaUnit.AttoBarn)
  def fb: A = areaPostfixOps(AreaUnit.FemtoBarn)
  def pb: A = areaPostfixOps(AreaUnit.PicoBarn)
  def nb: A = areaPostfixOps(AreaUnit.NanoBarn)
  def μb: A = areaPostfixOps(AreaUnit.MicroBarn)
  def mb: A = areaPostfixOps(AreaUnit.MilliBarn)
  def b : A = areaPostfixOps(AreaUnit.Barn)
  def kb: A = areaPostfixOps(AreaUnit.KiloBarn)
  def Mb: A = areaPostfixOps(AreaUnit.MegaBarn)
  def Gb: A = areaPostfixOps(AreaUnit.GigaBarn)
  def Tb: A = areaPostfixOps(AreaUnit.TeraBarn)
  def Pb: A = areaPostfixOps(AreaUnit.PetaBarn)
  def Eb: A = areaPostfixOps(AreaUnit.ExaBarn)
  def Zb: A = areaPostfixOps(AreaUnit.ZettaBarn)
  def Yb: A = areaPostfixOps(AreaUnit.YottaBarn)

  // yard-pond
  def ac: A = areaPostfixOps(AreaUnit.Acre)
}

class Area[A: Fractional](val value: A, val unit: AreaUnit)
  extends Quantity[A, AreaUnit]
    with AreaPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AreaUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSquareMetre) / real(evalUnit.unitInSquareMetre)

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)
}

abstract class AreaUnit(val symbol: String, val unitInSquareMetre: Real)
  extends PhysicalUnit {

  def this(symbol: String, factor: Real, areaUnit: AreaUnit) =
    this(symbol, factor * areaUnit.unitInSquareMetre)

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

  case object Are     extends AreaUnit("a"  , r"1e2")
  case object Hectare extends AreaUnit("ha" , r"1e4")

  case object YoctoBarn extends AreaUnit("yb", r"1e-24", Barn)
  case object ZeptoBarn extends AreaUnit("zb", r"1e-21", Barn)
  case object AttoBarn  extends AreaUnit("ab", r"1e-18", Barn)
  case object FemtoBarn extends AreaUnit("fb", r"1e-15", Barn)
  case object PicoBarn  extends AreaUnit("pb", r"1e-12", Barn)
  case object NanoBarn  extends AreaUnit("nb", r"1e-9", Barn)
  case object MicroBarn extends AreaUnit("μb", r"1e-6", Barn)
  case object MilliBarn extends AreaUnit("mb", r"1e-3", Barn)
  case object Barn      extends AreaUnit("b" , r"1e-28")
  case object KiloBarn  extends AreaUnit("kb", r"1e3", Barn)
  case object MegaBarn  extends AreaUnit("Mb", r"1e6", Barn)
  case object GigaBarn  extends AreaUnit("Mb", r"1e9", Barn)
  case object TeraBarn  extends AreaUnit("Mb", r"1e12", Barn)
  case object PetaBarn  extends AreaUnit("Pb", r"1e15", Barn)
  case object ExaBarn   extends AreaUnit("Eb", r"1e18", Barn)
  case object ZettaBarn extends AreaUnit("Zb", r"1e21", Barn)
  case object YottaBarn extends AreaUnit("Yb", r"1e24", Barn)

  case object Acre extends AreaUnit("ac", r"4046.8564224")
}

trait PredefinedAreaUnit extends AreaPostfixOps[AreaUnit]{

  override protected def areaPostfixOps(areaUnit: AreaUnit) = areaUnit
}

object PredefinedAreaUnit extends PredefinedAreaUnit

trait AreaUnitInterpreter[A]
  extends AreaPostfixOps[Area[A]]{

  def apply(unit: AreaUnit): Area[A]

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)
}