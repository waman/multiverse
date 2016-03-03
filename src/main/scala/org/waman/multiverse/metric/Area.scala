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
  def microMetre2: A = areaPostfixOps(SquareMicroMetre)
  def micrometre2: A = microMetre2
  def μm2: A = microMetre2
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
  def microBarn: A = areaPostfixOps(MicroBarn)
  def μb: A = microBarn
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
  def mil2: A = areaPostfixOps(SquareMil)
  def in2 : A = areaPostfixOps(SquareInch)
  def li2 : A = areaPostfixOps(SquareLink)
  def lnk2: A = li2
  def ft2 : A = areaPostfixOps(SquareFoot)
  def ch2 : A = areaPostfixOps(SquareChain)
  def yd2 : A = areaPostfixOps(SquareYard)
  def rd2 : A = areaPostfixOps(SquareRod)
  def mi2 : A = areaPostfixOps(SquareMile)

  def sq_mil: A = mil2
  def sq_in : A = in2
  def sq_li : A = li2
  def sq_lnk: A = lnk2
  def sq_ft : A = ft2
  def sq_ch : A = ch2
  def sq_yd : A = yd2
  def sq_rd : A = rd2
  def sq_mi : A = mi2

  def ac: A = areaPostfixOps(Acre)
  def ro: A = areaPostfixOps(Rood)

  def sq_li(c: Context): A = areaPostfixOps(_sq_li(c))
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

  lazy val _sq_li: PartialFunction[Context, AreaUnit] = {
    case UnitedStates => AreaUnit.SquareLink_US_Survey
  }

  lazy val _sq_lnk: PartialFunction[Context, AreaUnit] = _sq_li

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
  def microMetre2(dot: Dot): A = areaDot(SquareMicroMetre)
  def micrometre2(dot: Dot): A = microMetre2(dot)
  def μm2(dot: Dot): A = microMetre2(dot)
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
  def microBarn(dot: Dot): A = areaDot(MicroBarn)
  def μb(dot: Dot): A = microBarn(dot)
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
  def mil2(dot: Dot): A = areaDot(SquareMil)
  def in2 (dot: Dot): A = areaDot(SquareInch)
  def li2 (dot: Dot): A = areaDot(SquareLink)
  def lnk2(dot: Dot): A = li2(dot)
  def ft2 (dot: Dot): A = areaDot(SquareFoot)
  def ch2 (dot: Dot): A = areaDot(SquareChain)
  def yd2 (dot: Dot): A = areaDot(SquareYard)
  def rd2 (dot: Dot): A = areaDot(SquareRod)
  def mi2 (dot: Dot): A = areaDot(SquareMile)

  def sq_mil(dot: Dot): A = mil2(dot)
  def sq_in (dot: Dot): A = in2(dot)
  def sq_li (dot: Dot): A = li2(dot)
  def sq_lnk(dot: Dot): A = li2(dot)
  def sq_ft (dot: Dot): A = ft2(dot)
  def sq_ch (dot: Dot): A = ch2(dot)
  def sq_yd (dot: Dot): A = yd2(dot)
  def sq_rd (dot: Dot): A = rd2(dot)
  def sq_mi (dot: Dot): A = mi2(dot)

  def ac(dot: Dot): A = areaDot(Acre)
  def ro(dot: Dot): A = areaDot(Rood)

//  def sq_lnk(c: Context)(dot: Dot): A = areaDot(_sq_lnk(c))
//  def sq_ft (c: Context)(dot: Dot): A = areaDot(_sq_ft(c))
//  def sq_ch (c: Context)(dot: Dot): A = areaDot(_sq_ch(c))
//  def sq_mi (c: Context)(dot: Dot): A = areaDot(_sq_mi(c))
//  def ac(c: Context)(dot: Dot): A = areaDot(_ac(c))

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
  def microMetre2(per: Per): A = areaPer(SquareMicroMetre)
  def micrometre2(per: Per): A = microMetre2(per)
  def μm2(per: Per): A = microMetre2(per)
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
  def microBarn(per: Per): A = areaPer(MicroBarn)
  def μb(per: Per): A = microBarn(per)
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
  def mil2(per: Per): A = areaPer(SquareMil)
  def in2 (per: Per): A = areaPer(SquareInch)
  def li2 (per: Per): A = areaPer(SquareLink)
  def lnk2(per: Per): A = li2(per)
  def ft2 (per: Per): A = areaPer(SquareFoot)
  def ch2 (per: Per): A = areaPer(SquareChain)
  def yd2 (per: Per): A = areaPer(SquareYard)
  def rd2 (per: Per): A = areaPer(SquareRod)
  def mi2 (per: Per): A = areaPer(SquareMile)

  def sq_mil(per: Per): A = mil2(per)
  def sq_in (per: Per): A = in2(per)
  def sq_li (per: Per): A = li2(per)
  def sq_lnk(per: Per): A = li2(per)
  def sq_ft (per: Per): A = ft2(per)
  def sq_ch (per: Per): A = ch2(per)
  def sq_yd (per: Per): A = yd2(per)
  def sq_rd (per: Per): A = rd2(per)
  def sq_mi (per: Per): A = mi2(per)

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

object AreaUnit extends ConstantsDefined[AreaUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)

  // intrinsic
  private[AreaUnit]
  class IntrinsicAreaUnit(val symbols: Seq[String], val unitInSquareMetre: Real)
    extends AreaUnit{

    def this(symbols: Seq[String], factor: Real, areaUnit: AreaUnit) =
      this(symbols, factor * areaUnit.unitInSquareMetre)

    def this(symbols: Seq[String], areaUnit: AreaUnit) = this(symbols, 1, areaUnit)
  }

  case object SquareYoctoMetre extends IntrinsicAreaUnit("ym2", r"1e-48")
  case object SquareZeptoMetre extends IntrinsicAreaUnit("zm2", r"1e-42")
  case object SquareAttoMetre  extends IntrinsicAreaUnit("am2", r"1e-36")
  case object SquareFemtoMetre extends IntrinsicAreaUnit("fm2", r"1e-30")
  case object SquarePicoMetre  extends IntrinsicAreaUnit("pm2", r"1e-24")
  case object SquareNanoMetre  extends IntrinsicAreaUnit("nm2", r"1e-18")
  case object SquareMicroMetre extends IntrinsicAreaUnit(Seq("μm2", "microMetre2", "micrometre2"), r"1e-12")
  case object SquareMilliMetre extends IntrinsicAreaUnit("mm2", r"1e-6")
  case object SquareCentiMetre extends IntrinsicAreaUnit("cm2", r"1e-4")
  case object SquareDeciMetre  extends IntrinsicAreaUnit("dm2", r"1e-2")
  case object SquareMetre      extends IntrinsicAreaUnit("m2" , r"1")
  case object SquareDecaMetre  extends IntrinsicAreaUnit("dam2", r"1e2")
  case object SquareHectoMetre extends IntrinsicAreaUnit("hm2", r"1e4")
  case object SquareKiloMetre  extends IntrinsicAreaUnit("km2", r"1e6")
  case object SquareMegaMetre  extends IntrinsicAreaUnit("Mm2", r"1e12")
  case object SquareGigaMetre  extends IntrinsicAreaUnit("Gm2", r"1e18")
  case object SquareTeraMetre  extends IntrinsicAreaUnit("Tm2", r"1e24")
  case object SquarePetaMetre  extends IntrinsicAreaUnit("Pm2", r"1e30")
  case object SquareExaMetre   extends IntrinsicAreaUnit("Em2", r"1e36")
  case object SquareZettaMetre extends IntrinsicAreaUnit("Zm2", r"1e42")
  case object SquareYottaMetre extends IntrinsicAreaUnit("Ym2", r"1e48")

  case object Are     extends IntrinsicAreaUnit("a"  , r"1e2")
  case object Hectare extends IntrinsicAreaUnit("ha" , r"1e4")

  // microscopic
  case object YoctoBarn extends IntrinsicAreaUnit("yb", r"1e-24", Barn)
  case object ZeptoBarn extends IntrinsicAreaUnit("zb", r"1e-21", Barn)
  case object AttoBarn  extends IntrinsicAreaUnit("ab", r"1e-18", Barn)
  case object FemtoBarn extends IntrinsicAreaUnit("fb", r"1e-15", Barn)
  case object PicoBarn  extends IntrinsicAreaUnit("pb", r"1e-12", Barn)
  case object NanoBarn  extends IntrinsicAreaUnit("nb", r"1e-9", Barn)
  case object MicroBarn extends IntrinsicAreaUnit(Seq("μb", "microBarn"), r"1e-6", Barn)
  case object MilliBarn extends IntrinsicAreaUnit("mb", r"1e-3", Barn)
  case object Barn      extends IntrinsicAreaUnit("b" , r"1e-28")
  case object KiloBarn  extends IntrinsicAreaUnit("kb", r"1e3", Barn)
  case object MegaBarn  extends IntrinsicAreaUnit("Mb", r"1e6", Barn)
  case object GigaBarn  extends IntrinsicAreaUnit("Gb", r"1e9", Barn)
  case object TeraBarn  extends IntrinsicAreaUnit("Tb", r"1e12", Barn)
  case object PetaBarn  extends IntrinsicAreaUnit("Pb", r"1e15", Barn)
  case object ExaBarn   extends IntrinsicAreaUnit("Eb", r"1e18", Barn)
  case object ZettaBarn extends IntrinsicAreaUnit("Zb", r"1e21", Barn)
  case object YottaBarn extends IntrinsicAreaUnit("Yb", r"1e24", Barn)

  // yard-pond
  case object SquareMil   extends IntrinsicAreaUnit(Seq("mil2", "sq_mil"), LengthUnit.Mil.square)
  case object SquareInch  extends IntrinsicAreaUnit(Seq("in2", "sq_in"), LengthUnit.Inch.square)
  case object SquareLink  extends IntrinsicAreaUnit(Seq("li2", "lnk2", "sq_li", "sq_lnk"), LengthUnit.Link.square)
  case object SquareFoot  extends IntrinsicAreaUnit(Seq("ft2", "sq_ft"), LengthUnit.Foot.square)
  case object SquareChain extends IntrinsicAreaUnit(Seq("ch2", "sq_ch"), LengthUnit.Chain.square)
  case object SquareYard  extends IntrinsicAreaUnit(Seq("yd2", "sq_yd"), LengthUnit.Yard.square)
  case object SquareRod   extends IntrinsicAreaUnit(Seq("rd2", "sq_rd"), LengthUnit.Rod.square)
  case object SquareMile  extends IntrinsicAreaUnit(Seq("mi2", "sq_mi"), LengthUnit.Mile.square)
  case object Acre extends IntrinsicAreaUnit("ac", 10, LengthUnit.Chain.square)
  case object Rood extends IntrinsicAreaUnit("ro", r"1/4", Acre)

  case object SquareLink_US_Survey  extends IntrinsicAreaUnit(Seq("sq_li(US)", "sq_lnk(US)"), LengthUnit.Link_US_Survey.square)
  case object SquareFoot_US_Survey  extends IntrinsicAreaUnit("sq_ft(US)", LengthUnit.Foot_US_Survey.square)
  case object SquareChain_US_Survey extends IntrinsicAreaUnit("sq_ch(US)", LengthUnit.Chain_US_Survey.square)
  case object SquareMile_US_Survey  extends IntrinsicAreaUnit("sq_mi(US)", LengthUnit.Mile_US_Survey.square)
  case object Acre_US_Survey extends IntrinsicAreaUnit("ac(US)", 10, LengthUnit.Chain_US_Survey.square)

  case object CircularMil extends IntrinsicAreaUnit("circ_mil", Real.pi/4.0, SquareMil)
  case object CircularInch extends IntrinsicAreaUnit("circ_in", Real.pi/4.0, SquareInch)

  case object Board extends IntrinsicAreaUnit("bd", LengthUnit.Inch * LengthUnit.Foot)

  override lazy val values = Seq(
    SquareYoctoMetre,
    SquareZeptoMetre,
    SquareAttoMetre,
    SquareFemtoMetre,
    SquarePicoMetre,
    SquareNanoMetre,
    SquareMicroMetre,
    SquareMilliMetre,
    SquareCentiMetre,
    SquareDeciMetre,
    SquareMetre,
    SquareDecaMetre,
    SquareHectoMetre,
    SquareKiloMetre,
    SquareMegaMetre,
    SquareGigaMetre,
    SquareTeraMetre,
    SquarePetaMetre,
    SquareExaMetre,
    SquareZettaMetre,
    SquareYottaMetre,

    Are,
    Hectare,

    YoctoBarn,
    ZeptoBarn,
    AttoBarn,
    FemtoBarn,
    PicoBarn,
    NanoBarn,
    MicroBarn,
    MilliBarn,
    Barn,
    KiloBarn,
    MegaBarn,
    GigaBarn,
    TeraBarn,
    PetaBarn,
    ExaBarn,
    ZettaBarn,
    YottaBarn,

    SquareMil,
    SquareInch,
    SquareLink,
    SquareFoot,
    SquareChain,
    SquareYard,
    SquareRod,
    SquareMile,
    Acre,
    Rood,

    SquareLink_US_Survey,
    SquareFoot_US_Survey,
    SquareChain_US_Survey,
    SquareMile_US_Survey,
    Acre_US_Survey,

    CircularMil,
    CircularInch,

    Board
  )

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

trait AreaFactory[A]
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