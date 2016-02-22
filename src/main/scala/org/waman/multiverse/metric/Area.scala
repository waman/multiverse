package org.waman.multiverse.metric

import org.waman.multiverse.Context._
import org.waman.multiverse._
import org.waman.multiverse.fluid.{KinematicViscosity, KinematicViscosityUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait AreaPostfixOps[A]{
  import AreaPostfixOps._
  import AreaUnit._

  protected def areaPostfixOps(areaUnit: AreaUnit): A

  def ym2: A = areaPostfixOps(SquareYoctoMetre)
  def zm2: A = areaPostfixOps(SquareZeptoMetre)
  def am2: A = areaPostfixOps(SquareAttoMetre)
  def fm2: A = areaPostfixOps(SquareFemtoMetre)
  def pm2: A = areaPostfixOps(SquarePicoMetre)
  def nm2: A = areaPostfixOps(SquareNanoMetre)
  def μm2: A = areaPostfixOps(SquareMicroMetre)
  def mm2: A = areaPostfixOps(SquareMilliMetre)
  def cm2: A = areaPostfixOps(SquareCentiMetre)
  def dm2: A = areaPostfixOps(SquareDeciMetre)
  def m2 : A = areaPostfixOps(SquareMetre)
  def dam2: A = areaPostfixOps(SquareDecaMetre)
  def hm2: A = areaPostfixOps(SquareHectoMetre)
  def km2: A = areaPostfixOps(SquareKiloMetre)
  def Mm2: A = areaPostfixOps(SquareMegaMetre)
  def Gm2: A = areaPostfixOps(SquareGigaMetre)
  def Tm2: A = areaPostfixOps(SquareTeraMetre)
  def Pm2: A = areaPostfixOps(SquarePetaMetre)
  def Em2: A = areaPostfixOps(SquareExaMetre)
  def Zm2: A = areaPostfixOps(SquareZettaMetre)
  def Ym2: A = areaPostfixOps(SquareYottaMetre)

  def a : A = areaPostfixOps(Are)
  def ha: A = areaPostfixOps(Hectare)

  // microscopic
  def yb: A = areaPostfixOps(YoctoBarn)
  def zb: A = areaPostfixOps(ZeptoBarn)
  def ab: A = areaPostfixOps(AttoBarn)
  def fb: A = areaPostfixOps(FemtoBarn)
  def pb: A = areaPostfixOps(PicoBarn)
  def nb: A = areaPostfixOps(NanoBarn)
  def μb: A = areaPostfixOps(MicroBarn)
  def mb: A = areaPostfixOps(MilliBarn)
  def b : A = areaPostfixOps(Barn)
  def kb: A = areaPostfixOps(KiloBarn)
  def Mb: A = areaPostfixOps(MegaBarn)
  def Gb: A = areaPostfixOps(GigaBarn)
  def Tb: A = areaPostfixOps(TeraBarn)
  def Pb: A = areaPostfixOps(PetaBarn)
  def Eb: A = areaPostfixOps(ExaBarn)
  def Zb: A = areaPostfixOps(ZettaBarn)
  def Yb: A = areaPostfixOps(YottaBarn)

  // yard-pond
  def sq_mil: A = areaPostfixOps(SquareMil)
  def sq_in : A = areaPostfixOps(SquareInch)
  def sq_lnk: A = areaPostfixOps(SquareLink)
  def sq_ft : A = areaPostfixOps(SquareFoot)
  def sq_ch : A = areaPostfixOps(SquareChain)
  def sq_yd : A = areaPostfixOps(SquareYard)
  def sq_rd : A = areaPostfixOps(SquareRod)
  def sq_mi : A = areaPostfixOps(SquareMile)
  def ac: A = areaPostfixOps(Acre)
  def ro: A = areaPostfixOps(Rood)

  def sq_lnk(c: Context): A = areaPostfixOps(_sq_lnk(c))
  def sq_ft (c: Context): A = areaPostfixOps(_sq_ft(c))
  def sq_ch (c: Context): A = areaPostfixOps(_sq_ch(c))
  def sq_mi (c: Context): A = areaPostfixOps(_sq_mi(c))
  def ac(c: Context): A = areaPostfixOps(_ac(c))

  def circ_mil: A = areaPostfixOps(CircularMil)
  def circ_in : A = areaPostfixOps(CircularInch)

  def bd: A = areaPostfixOps(Board)
}

object AreaPostfixOps{

  lazy val _sq_lnk: PartialFunction[Context, AreaUnit] = {
    case UnitedStates => AreaUnit.SquareLink_US_Survey
  }

  lazy val _sq_ft: PartialFunction[Context, AreaUnit] = {
    case UnitedStates => AreaUnit.SquareFoot_US_Survey
  }

  lazy val _sq_ch: PartialFunction[Context, AreaUnit] = {
    case UnitedStates => AreaUnit.SquareChain_US_Survey
  }

  lazy val _sq_mi: PartialFunction[Context, AreaUnit] = {
    case UnitedStates => AreaUnit.SquareMile_US_Survey
  }

  lazy val _ac: PartialFunction[Context, AreaUnit] = {
    case UnitedStates => AreaUnit.Acre_US_Survey
  }
}

trait AreaPer[A]{
  import AreaUnit._

  protected def areaPer(areaUnit: AreaUnit): A

  def ym2(per: Per): A = areaPer(SquareYoctoMetre)
  def zm2(per: Per): A = areaPer(SquareZeptoMetre)
  def am2(per: Per): A = areaPer(SquareAttoMetre)
  def fm2(per: Per): A = areaPer(SquareFemtoMetre)
  def pm2(per: Per): A = areaPer(SquarePicoMetre)
  def nm2(per: Per): A = areaPer(SquareNanoMetre)
  def μm2(per: Per): A = areaPer(SquareMicroMetre)
  def mm2(per: Per): A = areaPer(SquareMilliMetre)
  def cm2(per: Per): A = areaPer(SquareCentiMetre)
  def dm2(per: Per): A = areaPer(SquareDeciMetre)
  def m2 (per: Per): A = areaPer(SquareMetre)
  def dam2(per: Per): A = areaPer(SquareDecaMetre)
  def hm2(per: Per): A = areaPer(SquareHectoMetre)
  def km2(per: Per): A = areaPer(SquareKiloMetre)
  def Mm2(per: Per): A = areaPer(SquareMegaMetre)
  def Gm2(per: Per): A = areaPer(SquareGigaMetre)
  def Tm2(per: Per): A = areaPer(SquareTeraMetre)
  def Pm2(per: Per): A = areaPer(SquarePetaMetre)
  def Em2(per: Per): A = areaPer(SquareExaMetre)
  def Zm2(per: Per): A = areaPer(SquareZettaMetre)
  def Ym2(per: Per): A = areaPer(SquareYottaMetre)

  def a (per: Per): A = areaPer(Are)
  def ha(per: Per): A = areaPer(Hectare)

  // microscopic
  def yb(per: Per): A = areaPer(YoctoBarn)
  def zb(per: Per): A = areaPer(ZeptoBarn)
  def ab(per: Per): A = areaPer(AttoBarn)
  def fb(per: Per): A = areaPer(FemtoBarn)
  def pb(per: Per): A = areaPer(PicoBarn)
  def nb(per: Per): A = areaPer(NanoBarn)
  def μb(per: Per): A = areaPer(MicroBarn)
  def mb(per: Per): A = areaPer(MilliBarn)
  def b (per: Per): A = areaPer(Barn)
  def kb(per: Per): A = areaPer(KiloBarn)
  def Mb(per: Per): A = areaPer(MegaBarn)
  def Gb(per: Per): A = areaPer(GigaBarn)
  def Tb(per: Per): A = areaPer(TeraBarn)
  def Pb(per: Per): A = areaPer(PetaBarn)
  def Eb(per: Per): A = areaPer(ExaBarn)
  def Zb(per: Per): A = areaPer(ZettaBarn)
  def Yb(per: Per): A = areaPer(YottaBarn)

  // yard-pond
  def sq_mil(per: Per): A = areaPer(SquareMil)
  def sq_in (per: Per): A = areaPer(SquareInch)
  def sq_lnk(per: Per): A = areaPer(SquareLink)
  def sq_ft (per: Per): A = areaPer(SquareFoot)
  def sq_ch (per: Per): A = areaPer(SquareChain)
  def sq_yd (per: Per): A = areaPer(SquareYard)
  def sq_rd (per: Per): A = areaPer(SquareRod)
  def sq_mi (per: Per): A = areaPer(SquareMile)
  def ac(per: Per): A = areaPer(Acre)
  def ro(per: Per): A = areaPer(Rood)

//  def sq_lnk(c: Context)(per: Per): A = areaPer(_sq_lnk(c))
//  def sq_ft (c: Context)(per: Per): A = areaPer(_sq_ft(c))
//  def sq_ch (c: Context)(per: Per): A = areaPer(_sq_ch(c))
//  def sq_mi (c: Context)(per: Per): A = areaPer(_sq_mi(c))
//  def ac(c: Context)(per: Per): A = areaPer(_ac(c))

  def circ_mil(per: Per): A = areaPer(CircularMil)
  def circ_in (per: Per): A = areaPer(CircularInch)

  def bd(per: Per): A = areaPer(Board)
}

class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends Quantity[A, AreaUnit]
    with AreaPostfixOps[A]
    with DivisibleByTimeUnit[KinematicViscosity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AreaUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSquareMetre) / real(evalUnit.unitInSquareMetre)

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)

  override def /(timeUnit: TimeUnit) = new KinematicViscosity[A](value, unit / timeUnit)
}

sealed abstract class AreaUnit(val symbol: String, val unitInSquareMetre: Real)
    extends PhysicalUnit[AreaUnit]
    with DivisibleByTimeUnit[KinematicViscosityUnit]{

  def this(symbol: String, factor: Real, areaUnit: AreaUnit) =
    this(symbol, factor * areaUnit.unitInSquareMetre)

  def this(symbol: String, lengthUnit: LengthUnit) =
    this(symbol, lengthUnit.unitInMetre **2)

  def this(symbol: String, factor: Real, lengthUnit: LengthUnit) =
    this(symbol, factor * (lengthUnit.unitInMetre**2))

  def this(symbol: String, lengthUnit1: LengthUnit, lengthUnit2: LengthUnit) =
    this(symbol, lengthUnit1.unitInMetre * lengthUnit2.unitInMetre)

  override val baseUnit = AreaUnit.SquareMetre
  override val inBaseUnitAccessor = () => unitInSquareMetre

  override def /(timeUnit: TimeUnit) = KinematicViscosityUnit(this, timeUnit)
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

  // microscopic
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

  // yard-pond
  case object SquareMil   extends AreaUnit("sq_mil", LengthUnit.Mil)
  case object SquareInch  extends AreaUnit("sq_in" , LengthUnit.Inch)
  case object SquareLink  extends AreaUnit("sq_lnk", LengthUnit.Link)
  case object SquareFoot  extends AreaUnit("sq_ft" , LengthUnit.Foot)
  case object SquareChain extends AreaUnit("sq_ch" , LengthUnit.Chain)
  case object SquareYard  extends AreaUnit("sq_yd" , LengthUnit.Yard)
  case object SquareRod   extends AreaUnit("sq_rd" , LengthUnit.Rod)
  case object SquareMile  extends AreaUnit("sq_mi" , LengthUnit.Mile)
  case object Acre extends AreaUnit("ac", 10, LengthUnit.Chain)
  case object Rood extends AreaUnit("ro", r"1/4", Acre)

  case object SquareLink_US_Survey  extends AreaUnit("sq_lnk(US)", LengthUnit.Link_US_Survey)
  case object SquareFoot_US_Survey  extends AreaUnit("sq_ft(US)" , LengthUnit.Foot_US_Survey)
  case object SquareChain_US_Survey extends AreaUnit("sq_ch(US)" , LengthUnit.Chain_US_Survey)
  case object SquareMile_US_Survey  extends AreaUnit("sq_mi(US)" , LengthUnit.Mile_US_Survey)
  case object Acre_US_Survey extends AreaUnit("acre(US)", 10, LengthUnit.Chain_US_Survey)

  case object CircularMil extends AreaUnit("circ_mil", Real.pi/4.0, SquareMil)
  case object CircularInch extends AreaUnit("circ_in", Real.pi/4.0, SquareInch)

  case object Board extends AreaUnit("bd", LengthUnit.Inch, LengthUnit.Foot)
}

trait PredefinedAreaUnit extends AreaPostfixOps[AreaUnit]{

  override protected def areaPostfixOps(areaUnit: AreaUnit) = areaUnit
}

object PredefinedAreaUnit extends PredefinedAreaUnit

trait AreaUnitInterpreter[A]
    extends AreaPostfixOps[Area[A]]
    with AreaPer[TimePostfixOps[KinematicViscosity[A]]]{

  def apply(unit: AreaUnit): Area[A]

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)

  // Area / Time -> KinematicViscosity
  protected def apply(unit: KinematicViscosityUnit): KinematicViscosity[A]

  override protected def areaPer(areaUnit: AreaUnit) = new TimePostfixOps[KinematicViscosity[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }
}