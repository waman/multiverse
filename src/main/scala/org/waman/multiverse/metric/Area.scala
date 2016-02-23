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

trait AreaDot[A]{
  import AreaUnit._

  protected def areaDot(areaUnit: AreaUnit): A

  def ym2(dot: Dot): A = areaDot(SquareYoctoMetre)
  def zm2(dot: Dot): A = areaDot(SquareZeptoMetre)
  def am2(dot: Dot): A = areaDot(SquareAttoMetre)
  def fm2(dot: Dot): A = areaDot(SquareFemtoMetre)
  def pm2(dot: Dot): A = areaDot(SquarePicoMetre)
  def nm2(dot: Dot): A = areaDot(SquareNanoMetre)
  def μm2(dot: Dot): A = areaDot(SquareMicroMetre)
  def mm2(dot: Dot): A = areaDot(SquareMilliMetre)
  def cm2(dot: Dot): A = areaDot(SquareCentiMetre)
  def dm2(dot: Dot): A = areaDot(SquareDeciMetre)
  def m2 (dot: Dot): A = areaDot(SquareMetre)
  def dam2(dot: Dot): A = areaDot(SquareDecaMetre)
  def hm2(dot: Dot): A = areaDot(SquareHectoMetre)
  def km2(dot: Dot): A = areaDot(SquareKiloMetre)
  def Mm2(dot: Dot): A = areaDot(SquareMegaMetre)
  def Gm2(dot: Dot): A = areaDot(SquareGigaMetre)
  def Tm2(dot: Dot): A = areaDot(SquareTeraMetre)
  def Pm2(dot: Dot): A = areaDot(SquarePetaMetre)
  def Em2(dot: Dot): A = areaDot(SquareExaMetre)
  def Zm2(dot: Dot): A = areaDot(SquareZettaMetre)
  def Ym2(dot: Dot): A = areaDot(SquareYottaMetre)

  def a (dot: Dot): A = areaDot(Are)
  def ha(dot: Dot): A = areaDot(Hectare)

  // microscopic
  def yb(dot: Dot): A = areaDot(YoctoBarn)
  def zb(dot: Dot): A = areaDot(ZeptoBarn)
  def ab(dot: Dot): A = areaDot(AttoBarn)
  def fb(dot: Dot): A = areaDot(FemtoBarn)
  def pb(dot: Dot): A = areaDot(PicoBarn)
  def nb(dot: Dot): A = areaDot(NanoBarn)
  def μb(dot: Dot): A = areaDot(MicroBarn)
  def mb(dot: Dot): A = areaDot(MilliBarn)
  def b (dot: Dot): A = areaDot(Barn)
  def kb(dot: Dot): A = areaDot(KiloBarn)
  def Mb(dot: Dot): A = areaDot(MegaBarn)
  def Gb(dot: Dot): A = areaDot(GigaBarn)
  def Tb(dot: Dot): A = areaDot(TeraBarn)
  def Pb(dot: Dot): A = areaDot(PetaBarn)
  def Eb(dot: Dot): A = areaDot(ExaBarn)
  def Zb(dot: Dot): A = areaDot(ZettaBarn)
  def Yb(dot: Dot): A = areaDot(YottaBarn)

  // yard-pond
  def sq_mil(dot: Dot): A = areaDot(SquareMil)
  def sq_in (dot: Dot): A = areaDot(SquareInch)
  def sq_lnk(dot: Dot): A = areaDot(SquareLink)
  def sq_ft (dot: Dot): A = areaDot(SquareFoot)
  def sq_ch (dot: Dot): A = areaDot(SquareChain)
  def sq_yd (dot: Dot): A = areaDot(SquareYard)
  def sq_rd (dot: Dot): A = areaDot(SquareRod)
  def sq_mi (dot: Dot): A = areaDot(SquareMile)
  def ac(dot: Dot): A = areaDot(Acre)
  def ro(dot: Dot): A = areaDot(Rood)

//  def sq_lnk(c: Context)(dot: Dot): A = areaPer(_sq_lnk(c))
//  def sq_ft (c: Context)(dot: Dot): A = areaPer(_sq_ft(c))
//  def sq_ch (c: Context)(dot: Dot): A = areaPer(_sq_ch(c))
//  def sq_mi (c: Context)(dot: Dot): A = areaPer(_sq_mi(c))
//  def ac(c: Context)(dot: Dot): A = areaPer(_ac(c))

  def circ_mil(dot: Dot): A = areaDot(CircularMil)
  def circ_in (dot: Dot): A = areaDot(CircularInch)

  def bd(dot: Dot): A = areaDot(Board)
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
    with LengthPostfixOps[MultiplicativeByLengthUnit[A]]
    with LengthDot[LengthPostfixOps[A]]
    with MultiplicativeByLengthUnit[Volume[A]]
    with DivisibleByTimeUnit[KinematicViscosity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AreaUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSquareMetre) / real(evalUnit.unitInSquareMetre)

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)

  override protected def lengthPostfixOps(lengthUnit1: LengthUnit) = new MultiplicativeByLengthUnit[A]{
    override def *(lengthUnit2: LengthUnit) = apply(lengthUnit1 * lengthUnit2)
  }

  override protected def lengthDot(lengthUnit1: LengthUnit) = new LengthPostfixOps[A]{
    override protected def lengthPostfixOps(lengthUnit2: LengthUnit) = apply(lengthUnit1 * lengthUnit2)
  }

  override def *(lengthUnit: LengthUnit) = new Volume(value, unit * lengthUnit)

  override def /(timeUnit: TimeUnit) = new KinematicViscosity[A](value, unit / timeUnit)
}

sealed trait AreaUnit extends PhysicalUnit[AreaUnit]
    with MultiplicativeByLengthUnit[VolumeUnit]
    with DivisibleByTimeUnit[KinematicViscosityUnit]{

  def unitInSquareMetre: Real

  override def baseUnit = AreaUnit.SquareMetre
  override def valueInBaseUnit = unitInSquareMetre

  override def *(lengthUnit: LengthUnit) = VolumeUnit(this, lengthUnit)

  override def /(timeUnit: TimeUnit) = KinematicViscosityUnit(this, timeUnit)
}

object AreaUnit{

  // custom
  private[AreaUnit]
  class AreaUnitImpl(val symbol: String, val unitInSquareMetre: Real)
    extends AreaUnit{

    def this(symbol: String, factor: Real, areaUnit: AreaUnit) =
      this(symbol, factor * areaUnit.unitInSquareMetre)

    def this(symbol: String, lengthUnit: LengthUnit) =
      this(symbol, lengthUnit.unitInMetre **2)

    def this(symbol: String, factor: Real, lengthUnit: LengthUnit) =
      this(symbol, factor * (lengthUnit.unitInMetre**2))

    def this(symbol: String, lengthUnit1: LengthUnit, lengthUnit2: LengthUnit) =
      this(symbol, lengthUnit1.unitInMetre * lengthUnit2.unitInMetre)
  }

  case object SquareYoctoMetre extends AreaUnitImpl("ym2", r"1e-48")
  case object SquareZeptoMetre extends AreaUnitImpl("zm2", r"1e-42")
  case object SquareAttoMetre  extends AreaUnitImpl("am2", r"1e-36")
  case object SquareFemtoMetre extends AreaUnitImpl("fm2", r"1e-30")
  case object SquarePicoMetre  extends AreaUnitImpl("pm2", r"1e-24")
  case object SquareNanoMetre  extends AreaUnitImpl("nm2", r"1e-18")
  case object SquareMicroMetre extends AreaUnitImpl("μm2", r"1e-12")
  case object SquareMilliMetre extends AreaUnitImpl("mm2", r"1e-6")
  case object SquareCentiMetre extends AreaUnitImpl("cm2", r"1e-4")
  case object SquareDeciMetre  extends AreaUnitImpl("dm2", r"1e-2")
  case object SquareMetre      extends AreaUnitImpl("m2" , r"1")
  case object SquareDecaMetre  extends AreaUnitImpl("dam2", r"1e2")
  case object SquareHectoMetre extends AreaUnitImpl("hm2", r"1e4")
  case object SquareKiloMetre  extends AreaUnitImpl("km2", r"1e6")
  case object SquareMegaMetre  extends AreaUnitImpl("Mm2", r"1e12")
  case object SquareGigaMetre  extends AreaUnitImpl("Gm2", r"1e18")
  case object SquareTeraMetre  extends AreaUnitImpl("Tm2", r"1e24")
  case object SquarePetaMetre  extends AreaUnitImpl("Pm2", r"1e30")
  case object SquareExaMetre   extends AreaUnitImpl("Em2", r"1e36")
  case object SquareZettaMetre extends AreaUnitImpl("Zm2", r"1e42")
  case object SquareYottaMetre extends AreaUnitImpl("Ym2", r"1e48")

  case object Are     extends AreaUnitImpl("a"  , r"1e2")
  case object Hectare extends AreaUnitImpl("ha" , r"1e4")

  // microscopic
  case object YoctoBarn extends AreaUnitImpl("yb", r"1e-24", Barn)
  case object ZeptoBarn extends AreaUnitImpl("zb", r"1e-21", Barn)
  case object AttoBarn  extends AreaUnitImpl("ab", r"1e-18", Barn)
  case object FemtoBarn extends AreaUnitImpl("fb", r"1e-15", Barn)
  case object PicoBarn  extends AreaUnitImpl("pb", r"1e-12", Barn)
  case object NanoBarn  extends AreaUnitImpl("nb", r"1e-9", Barn)
  case object MicroBarn extends AreaUnitImpl("μb", r"1e-6", Barn)
  case object MilliBarn extends AreaUnitImpl("mb", r"1e-3", Barn)
  case object Barn      extends AreaUnitImpl("b" , r"1e-28")
  case object KiloBarn  extends AreaUnitImpl("kb", r"1e3", Barn)
  case object MegaBarn  extends AreaUnitImpl("Mb", r"1e6", Barn)
  case object GigaBarn  extends AreaUnitImpl("Mb", r"1e9", Barn)
  case object TeraBarn  extends AreaUnitImpl("Mb", r"1e12", Barn)
  case object PetaBarn  extends AreaUnitImpl("Pb", r"1e15", Barn)
  case object ExaBarn   extends AreaUnitImpl("Eb", r"1e18", Barn)
  case object ZettaBarn extends AreaUnitImpl("Zb", r"1e21", Barn)
  case object YottaBarn extends AreaUnitImpl("Yb", r"1e24", Barn)

  // yard-pond
  case object SquareMil   extends AreaUnitImpl("sq_mil", LengthUnit.Mil)
  case object SquareInch  extends AreaUnitImpl("sq_in" , LengthUnit.Inch)
  case object SquareLink  extends AreaUnitImpl("sq_lnk", LengthUnit.Link)
  case object SquareFoot  extends AreaUnitImpl("sq_ft" , LengthUnit.Foot)
  case object SquareChain extends AreaUnitImpl("sq_ch" , LengthUnit.Chain)
  case object SquareYard  extends AreaUnitImpl("sq_yd" , LengthUnit.Yard)
  case object SquareRod   extends AreaUnitImpl("sq_rd" , LengthUnit.Rod)
  case object SquareMile  extends AreaUnitImpl("sq_mi" , LengthUnit.Mile)
  case object Acre extends AreaUnitImpl("ac", 10, LengthUnit.Chain)
  case object Rood extends AreaUnitImpl("ro", r"1/4", Acre)

  case object SquareLink_US_Survey  extends AreaUnitImpl("sq_lnk(US)", LengthUnit.Link_US_Survey)
  case object SquareFoot_US_Survey  extends AreaUnitImpl("sq_ft(US)" , LengthUnit.Foot_US_Survey)
  case object SquareChain_US_Survey extends AreaUnitImpl("sq_ch(US)" , LengthUnit.Chain_US_Survey)
  case object SquareMile_US_Survey  extends AreaUnitImpl("sq_mi(US)" , LengthUnit.Mile_US_Survey)
  case object Acre_US_Survey extends AreaUnitImpl("acre(US)", 10, LengthUnit.Chain_US_Survey)

  case object CircularMil extends AreaUnitImpl("circ_mil", Real.pi/4.0, SquareMil)
  case object CircularInch extends AreaUnitImpl("circ_in", Real.pi/4.0, SquareInch)

  case object Board extends AreaUnitImpl("bd", LengthUnit.Inch, LengthUnit.Foot)

  // Length * Length -> Area
  private[AreaUnit]
  class ProductAreaUnit(val firstUnit: LengthUnit, val secondUnit: LengthUnit)
      extends AreaUnit with ProductUnit[AreaUnit, LengthUnit, LengthUnit]{

    override val unitInSquareMetre: Real = firstUnit.unitInMetre * secondUnit.unitInMetre
  }

  def apply(lUnit1: LengthUnit, lUnit2: LengthUnit) = new ProductAreaUnit(lUnit1, lUnit2)
}

trait PredefinedAreaUnit extends AreaPostfixOps[AreaUnit]{

  override protected def areaPostfixOps(areaUnit: AreaUnit) = areaUnit
}

object PredefinedAreaUnit extends PredefinedAreaUnit

trait AreaUnitInterpreter[A]
    extends AreaPostfixOps[Area[A]]
    with AreaDot[LengthPostfixOps[Volume[A]]]
    with AreaPer[TimePostfixOps[KinematicViscosity[A]]]{

  def apply(unit: AreaUnit): Area[A]

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)

  // Area * Length -> Volume
  protected def apply(unit: VolumeUnit): Volume[A]

  override protected def areaDot(areaUnit: AreaUnit) = new LengthPostfixOps[Volume[A]]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(areaUnit * lengthUnit)
  }

  // Area / Time -> KinematicViscosity
  protected def apply(unit: KinematicViscosityUnit): KinematicViscosity[A]

  override protected def areaPer(areaUnit: AreaUnit) = new TimePostfixOps[KinematicViscosity[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }
}