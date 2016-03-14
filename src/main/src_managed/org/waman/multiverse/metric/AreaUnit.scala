package org.waman.multiverse.metric

import org.waman.multiverse._
import org.waman.multiverse.fluid.KinematicViscosityUnit
import org.waman.multiverse.time.TimeUnit
import spire.implicits._
import spire.math.Real

sealed trait AreaUnit extends PhysicalUnit[AreaUnit]
  with MultiplicativeByLengthUnit[VolumeUnit]
  with DivisibleByTimeUnit[KinematicViscosityUnit]{

  def unitInSquareMetre: Real

  override def baseUnit = org.waman.multiverse.metric.AreaUnit.SquareMetre
  override def valueInBaseUnit = unitInSquareMetre

  override def *(unit: LengthUnit) = VolumeUnit(this, unit)

  override def /(unit: TimeUnit) = KinematicViscosityUnit(this, unit)
}

object AreaUnit extends ConstantsDefined[AreaUnit]{

  // intrinsic
  private[AreaUnit]
  class IntrinsicAreaUnit(name: String, val symbols: Seq[String], val unitInSquareMetre: Real)
      extends AreaUnit{

    def this(name: String, symbols: Seq[String], unit: AreaUnit) =
      this(name, symbols, unit.unitInSquareMetre)

    def this(name: String, symbols: Seq[String], factor: Real, unit: AreaUnit) =
      this(name, symbols, factor * unit.unitInSquareMetre)
  }

  case object YoctoSquareMetre extends IntrinsicAreaUnit("YoctoSquareMetre", Seq("ym2"), r"1e-24"**2)
    
  case object ZeptoSquareMetre extends IntrinsicAreaUnit("ZeptoSquareMetre", Seq("zm2"), r"1e-21"**2)
    
  case object AttoSquareMetre extends IntrinsicAreaUnit("AttoSquareMetre", Seq("am2"), r"1e-18"**2)
    
  case object FemtoSquareMetre extends IntrinsicAreaUnit("FemtoSquareMetre", Seq("fm2"), r"1e-15"**2)
    
  case object PicoSquareMetre extends IntrinsicAreaUnit("PicoSquareMetre", Seq("pm2"), r"1e-12"**2)
    
  case object NanoSquareMetre extends IntrinsicAreaUnit("NanoSquareMetre", Seq("nm2"), r"1e-9"**2)
    
  case object MicroSquareMetre extends IntrinsicAreaUnit("MicroSquareMetre", Seq("microSquareMetre", "microM2", "μm2"), r"1e-6"**2)
    
  case object MilliSquareMetre extends IntrinsicAreaUnit("MilliSquareMetre", Seq("mm2"), r"1e-3"**2)
    
  case object CentiSquareMetre extends IntrinsicAreaUnit("CentiSquareMetre", Seq("cm2"), r"1e-2"**2)
    
  case object DeciSquareMetre extends IntrinsicAreaUnit("DeciSquareMetre", Seq("dm2"), r"1e-1"**2)
    
  case object SquareMetre extends IntrinsicAreaUnit("SquareMetre", Seq("m2"), r"1"**2)
    
  case object DecaSquareMetre extends IntrinsicAreaUnit("DecaSquareMetre", Seq("dam2"), r"1e-1"**2)
    
  case object HectoSquareMetre extends IntrinsicAreaUnit("HectoSquareMetre", Seq("hm2"), r"1e-2"**2)
    
  case object KiloSquareMetre extends IntrinsicAreaUnit("KiloSquareMetre", Seq("km2"), r"1e-3"**2)
    
  case object MegaSquareMetre extends IntrinsicAreaUnit("MegaSquareMetre", Seq("Mm2"), r"1e-6"**2)
    
  case object GigaSquareMetre extends IntrinsicAreaUnit("GigaSquareMetre", Seq("Gm2"), r"1e-9"**2)
    
  case object TeraSquareMetre extends IntrinsicAreaUnit("TeraSquareMetre", Seq("Tm2"), r"1e-12"**2)
    
  case object PetaSquareMetre extends IntrinsicAreaUnit("PetaSquareMetre", Seq("Pm2"), r"1e-15"**2)
    
  case object ExaSquareMetre extends IntrinsicAreaUnit("ExaSquareMetre", Seq("Em2"), r"1e-18"**2)
    
  case object ZettaSquareMetre extends IntrinsicAreaUnit("ZettaSquareMetre", Seq("Zm2"), r"1e-21"**2)
    
  case object YottaSquareMetre extends IntrinsicAreaUnit("YottaSquareMetre", Seq("Ym2"), r"1e-24"**2)
    
  case object Are extends IntrinsicAreaUnit("Are", Seq("a"), r"1e2")
    
  case object Hectare extends IntrinsicAreaUnit("Hectare", Seq("ha"), r"1e4")
    
  case object YoctoBarn extends IntrinsicAreaUnit("YoctoBarn", Seq("yb"), r"1e-24" * r"1e-28")
    
  case object ZeptoBarn extends IntrinsicAreaUnit("ZeptoBarn", Seq("zb"), r"1e-21" * r"1e-28")
    
  case object AttoBarn extends IntrinsicAreaUnit("AttoBarn", Seq("ab"), r"1e-18" * r"1e-28")
    
  case object FemtoBarn extends IntrinsicAreaUnit("FemtoBarn", Seq("fb"), r"1e-15" * r"1e-28")
    
  case object PicoBarn extends IntrinsicAreaUnit("PicoBarn", Seq("pb"), r"1e-12" * r"1e-28")
    
  case object NanoBarn extends IntrinsicAreaUnit("NanoBarn", Seq("nb"), r"1e-9" * r"1e-28")
    
  case object MicroBarn extends IntrinsicAreaUnit("MicroBarn", Seq("microBarn", "μb"), r"1e-6" * r"1e-28")
    
  case object MilliBarn extends IntrinsicAreaUnit("MilliBarn", Seq("mb"), r"1e-3" * r"1e-28")
    
  case object Barn extends IntrinsicAreaUnit("Barn", Seq("b"), r"1" * r"1e-28")
    
  case object KiloBarn extends IntrinsicAreaUnit("KiloBarn", Seq("kb"), r"1e-3" * r"1e-28")
    
  case object MegaBarn extends IntrinsicAreaUnit("MegaBarn", Seq("Mb"), r"1e-6" * r"1e-28")
    
  case object GigaBarn extends IntrinsicAreaUnit("GigaBarn", Seq("Gb"), r"1e-9" * r"1e-28")
    
  case object TeraBarn extends IntrinsicAreaUnit("TeraBarn", Seq("Tb"), r"1e-12" * r"1e-28")
    
  case object PetaBarn extends IntrinsicAreaUnit("PetaBarn", Seq("Pb"), r"1e-15" * r"1e-28")
    
  case object ExaBarn extends IntrinsicAreaUnit("ExaBarn", Seq("Eb"), r"1e-18" * r"1e-28")
    
  case object ZettaBarn extends IntrinsicAreaUnit("ZettaBarn", Seq("Zb"), r"1e-21" * r"1e-28")
    
  case object YottaBarn extends IntrinsicAreaUnit("YottaBarn", Seq("Yb"), r"1e-24" * r"1e-28")
    
  case object SquareMil extends IntrinsicAreaUnit("SquareMil", Seq("mil2", "sq_mil"), LengthUnit.Mil.square)
    
  case object SquareInch extends IntrinsicAreaUnit("SquareInch", Seq("in2", "sq_in"), LengthUnit.Mil.square)
    
  case object SquareLink extends IntrinsicAreaUnit("SquareLink", Seq("li2", "lnk2", "sq_li", "sq_lnk"), LengthUnit.Link.square)
    
  case object SquareFoot extends IntrinsicAreaUnit("SquareFoot", Seq("ft2", "sq_ft"), LengthUnit.Foot.square)
    
  case object SquareChain extends IntrinsicAreaUnit("SquareChain", Seq("ch2", "sq_ch"), LengthUnit.Chain.square)
    
  case object SquareYard extends IntrinsicAreaUnit("SquareYard", Seq("yd2", "sq_yd"), LengthUnit.Yard.square)
    
  case object SquareRod extends IntrinsicAreaUnit("SquareRod", Seq("rd2", "sq_rd"), LengthUnit.Rod.square)
    
  case object SquareMile extends IntrinsicAreaUnit("SquareMile", Seq("mi2", "sq_mi"), LengthUnit.Mile.square)
    
  case object Acre extends IntrinsicAreaUnit("Acre", Seq("ac"), 10, LengthUnit.Chain.square)
    
  case object Rood extends IntrinsicAreaUnit("Rood", Seq("ro"), r"1/4", Acre)
    
  case object SquareLink_US_Survey extends IntrinsicAreaUnit("SquareLink_US_Survey", Seq("sq_li(US)", "sq_lnk(US)"), LengthUnit.Link_US_Survey.square)
    
  case object SquareFoot_US_Survey extends IntrinsicAreaUnit("SquareFoot_US_Survey", Seq("sq_ft(US)"), LengthUnit.Foot_US_Survey.square)
    
  case object SquareChain_US_Survey extends IntrinsicAreaUnit("SquareChain_US_Survey", Seq("sq_ch(US)"), LengthUnit.Chain_US_Survey.square)
    
  case object SquareMile_US_Survey extends IntrinsicAreaUnit("SquareMile_US_Survey", Seq("sq_mi(US)"), LengthUnit.Mile_US_Survey.square)
    
  case object Acre_US_Survey extends IntrinsicAreaUnit("Acre_US_Survey", Seq("ac(US)"), 10, LengthUnit.Chain_US_Survey.square)
    
  case object CircularMil extends IntrinsicAreaUnit("CircularMil", Seq("circ_mil"), Real.pi/4.0, SquareMil)
    
  case object CircularInch extends IntrinsicAreaUnit("CircularInch", Seq("circ_in"), Real.pi/4.0, SquareInch)
    
  case object Board extends IntrinsicAreaUnit("Board", Seq("bd"), LengthUnit.Inch * LengthUnit.Foot)
    

  override lazy val values = Seq(YoctoSquareMetre, ZeptoSquareMetre, AttoSquareMetre, FemtoSquareMetre, PicoSquareMetre, NanoSquareMetre, MicroSquareMetre, MilliSquareMetre, CentiSquareMetre, DeciSquareMetre, SquareMetre, DecaSquareMetre, HectoSquareMetre, KiloSquareMetre, MegaSquareMetre, GigaSquareMetre, TeraSquareMetre, PetaSquareMetre, ExaSquareMetre, ZettaSquareMetre, YottaSquareMetre, Are, Hectare, YoctoBarn, ZeptoBarn, AttoBarn, FemtoBarn, PicoBarn, NanoBarn, MicroBarn, MilliBarn, Barn, KiloBarn, MegaBarn, GigaBarn, TeraBarn, PetaBarn, ExaBarn, ZettaBarn, YottaBarn, SquareMil, SquareInch, SquareLink, SquareFoot, SquareChain, SquareYard, SquareRod, SquareMile, Acre, Rood, SquareLink_US_Survey, SquareFoot_US_Survey, SquareChain_US_Survey, SquareMile_US_Survey, Acre_US_Survey, CircularMil, CircularInch, Board)

  // LengthUnit * LengthUnit -> Area
  private[AreaUnit]
  class LengthDotLengthUnit(val firstUnit: LengthUnit, val secondUnit: LengthUnit)
      extends AreaUnit with ProductUnit[AreaUnit, LengthUnit, LengthUnit]{

    override lazy val unitInSquareMetre: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: LengthUnit, unit2: LengthUnit): AreaUnit =
    new LengthDotLengthUnit(unit1, unit2)
}

trait AreaPostfixOps[A]{
  import AreaUnit._

  protected def areaPostfixOps(unit: AreaUnit): A

  def ym2 : A = areaPostfixOps(YoctoSquareMetre)
  def zm2 : A = areaPostfixOps(ZeptoSquareMetre)
  def am2 : A = areaPostfixOps(AttoSquareMetre)
  def fm2 : A = areaPostfixOps(FemtoSquareMetre)
  def pm2 : A = areaPostfixOps(PicoSquareMetre)
  def nm2 : A = areaPostfixOps(NanoSquareMetre)
  def microSquareMetre : A = areaPostfixOps(MicroSquareMetre)
  def microM2 : A = areaPostfixOps(MicroSquareMetre)
  def μm2 : A = areaPostfixOps(MicroSquareMetre)
  def mm2 : A = areaPostfixOps(MilliSquareMetre)
  def cm2 : A = areaPostfixOps(CentiSquareMetre)
  def dm2 : A = areaPostfixOps(DeciSquareMetre)
  def m2 : A = areaPostfixOps(SquareMetre)
  def dam2 : A = areaPostfixOps(DecaSquareMetre)
  def hm2 : A = areaPostfixOps(HectoSquareMetre)
  def km2 : A = areaPostfixOps(KiloSquareMetre)
  def Mm2 : A = areaPostfixOps(MegaSquareMetre)
  def Gm2 : A = areaPostfixOps(GigaSquareMetre)
  def Tm2 : A = areaPostfixOps(TeraSquareMetre)
  def Pm2 : A = areaPostfixOps(PetaSquareMetre)
  def Em2 : A = areaPostfixOps(ExaSquareMetre)
  def Zm2 : A = areaPostfixOps(ZettaSquareMetre)
  def Ym2 : A = areaPostfixOps(YottaSquareMetre)
  def a : A = areaPostfixOps(Are)
  def ha : A = areaPostfixOps(Hectare)
  def yb : A = areaPostfixOps(YoctoBarn)
  def zb : A = areaPostfixOps(ZeptoBarn)
  def ab : A = areaPostfixOps(AttoBarn)
  def fb : A = areaPostfixOps(FemtoBarn)
  def pb : A = areaPostfixOps(PicoBarn)
  def nb : A = areaPostfixOps(NanoBarn)
  def microBarn : A = areaPostfixOps(MicroBarn)
  def μb : A = areaPostfixOps(MicroBarn)
  def mb : A = areaPostfixOps(MilliBarn)
  def b : A = areaPostfixOps(Barn)
  def kb : A = areaPostfixOps(KiloBarn)
  def Mb : A = areaPostfixOps(MegaBarn)
  def Gb : A = areaPostfixOps(GigaBarn)
  def Tb : A = areaPostfixOps(TeraBarn)
  def Pb : A = areaPostfixOps(PetaBarn)
  def Eb : A = areaPostfixOps(ExaBarn)
  def Zb : A = areaPostfixOps(ZettaBarn)
  def Yb : A = areaPostfixOps(YottaBarn)
  def mil2 : A = areaPostfixOps(SquareMil)
  def sq_mil : A = areaPostfixOps(SquareMil)
  def in2 : A = areaPostfixOps(SquareInch)
  def sq_in : A = areaPostfixOps(SquareInch)
  def li2 : A = areaPostfixOps(SquareLink)
  def lnk2 : A = areaPostfixOps(SquareLink)
  def sq_li : A = areaPostfixOps(SquareLink)
  def sq_lnk : A = areaPostfixOps(SquareLink)
  def ft2 : A = areaPostfixOps(SquareFoot)
  def sq_ft : A = areaPostfixOps(SquareFoot)
  def ch2 : A = areaPostfixOps(SquareChain)
  def sq_ch : A = areaPostfixOps(SquareChain)
  def yd2 : A = areaPostfixOps(SquareYard)
  def sq_yd : A = areaPostfixOps(SquareYard)
  def rd2 : A = areaPostfixOps(SquareRod)
  def sq_rd : A = areaPostfixOps(SquareRod)
  def mi2 : A = areaPostfixOps(SquareMile)
  def sq_mi : A = areaPostfixOps(SquareMile)
  def ac : A = areaPostfixOps(Acre)
  def ro : A = areaPostfixOps(Rood)
  def circ_mil : A = areaPostfixOps(CircularMil)
  def circ_in : A = areaPostfixOps(CircularInch)
  def bd : A = areaPostfixOps(Board)
  import AreaPostfixOps._

  def sq_li(c: Context): A = areaPostfixOps(_sq_li(c))
  def sq_lnk(c: Context): A = areaPostfixOps(_sq_lnk(c))
  def sq_ft(c: Context): A = areaPostfixOps(_sq_ft(c))
  def sq_ch(c: Context): A = areaPostfixOps(_sq_ch(c))
  def sq_mi(c: Context): A = areaPostfixOps(_sq_mi(c))
  def ac(c: Context): A = areaPostfixOps(_ac(c))
}

object AreaPostfixOps{
  import AreaUnit._
  import org.waman.multiverse.Context._


  lazy val _sq_lnk : PartialFunction[Context, AreaUnit] = {
    case UnitedStates => SquareLink_US_Survey
  }

  lazy val _sq_ft : PartialFunction[Context, AreaUnit] = {
    case UnitedStates => SquareFoot_US_Survey
  }

  lazy val _sq_ch : PartialFunction[Context, AreaUnit] = {
    case UnitedStates => SquareChain_US_Survey
  }

  lazy val _sq_li : PartialFunction[Context, AreaUnit] = {
    case UnitedStates => SquareLink_US_Survey
  }

  lazy val _ac : PartialFunction[Context, AreaUnit] = {
    case UnitedStates => Acre_US_Survey
  }

  lazy val _sq_mi : PartialFunction[Context, AreaUnit] = {
    case UnitedStates => SquareMile_US_Survey
  }
}

trait AreaDot[A]{
  import AreaUnit._

  protected def areaDot(unit: AreaUnit): A

  def ym2(dot: Dot): A = areaDot(YoctoSquareMetre)
  def zm2(dot: Dot): A = areaDot(ZeptoSquareMetre)
  def am2(dot: Dot): A = areaDot(AttoSquareMetre)
  def fm2(dot: Dot): A = areaDot(FemtoSquareMetre)
  def pm2(dot: Dot): A = areaDot(PicoSquareMetre)
  def nm2(dot: Dot): A = areaDot(NanoSquareMetre)
  def microSquareMetre(dot: Dot): A = areaDot(MicroSquareMetre)
  def microM2(dot: Dot): A = areaDot(MicroSquareMetre)
  def μm2(dot: Dot): A = areaDot(MicroSquareMetre)
  def mm2(dot: Dot): A = areaDot(MilliSquareMetre)
  def cm2(dot: Dot): A = areaDot(CentiSquareMetre)
  def dm2(dot: Dot): A = areaDot(DeciSquareMetre)
  def m2(dot: Dot): A = areaDot(SquareMetre)
  def dam2(dot: Dot): A = areaDot(DecaSquareMetre)
  def hm2(dot: Dot): A = areaDot(HectoSquareMetre)
  def km2(dot: Dot): A = areaDot(KiloSquareMetre)
  def Mm2(dot: Dot): A = areaDot(MegaSquareMetre)
  def Gm2(dot: Dot): A = areaDot(GigaSquareMetre)
  def Tm2(dot: Dot): A = areaDot(TeraSquareMetre)
  def Pm2(dot: Dot): A = areaDot(PetaSquareMetre)
  def Em2(dot: Dot): A = areaDot(ExaSquareMetre)
  def Zm2(dot: Dot): A = areaDot(ZettaSquareMetre)
  def Ym2(dot: Dot): A = areaDot(YottaSquareMetre)
  def a(dot: Dot): A = areaDot(Are)
  def ha(dot: Dot): A = areaDot(Hectare)
  def yb(dot: Dot): A = areaDot(YoctoBarn)
  def zb(dot: Dot): A = areaDot(ZeptoBarn)
  def ab(dot: Dot): A = areaDot(AttoBarn)
  def fb(dot: Dot): A = areaDot(FemtoBarn)
  def pb(dot: Dot): A = areaDot(PicoBarn)
  def nb(dot: Dot): A = areaDot(NanoBarn)
  def microBarn(dot: Dot): A = areaDot(MicroBarn)
  def μb(dot: Dot): A = areaDot(MicroBarn)
  def mb(dot: Dot): A = areaDot(MilliBarn)
  def b(dot: Dot): A = areaDot(Barn)
  def kb(dot: Dot): A = areaDot(KiloBarn)
  def Mb(dot: Dot): A = areaDot(MegaBarn)
  def Gb(dot: Dot): A = areaDot(GigaBarn)
  def Tb(dot: Dot): A = areaDot(TeraBarn)
  def Pb(dot: Dot): A = areaDot(PetaBarn)
  def Eb(dot: Dot): A = areaDot(ExaBarn)
  def Zb(dot: Dot): A = areaDot(ZettaBarn)
  def Yb(dot: Dot): A = areaDot(YottaBarn)
  def mil2(dot: Dot): A = areaDot(SquareMil)
  def sq_mil(dot: Dot): A = areaDot(SquareMil)
  def in2(dot: Dot): A = areaDot(SquareInch)
  def sq_in(dot: Dot): A = areaDot(SquareInch)
  def li2(dot: Dot): A = areaDot(SquareLink)
  def lnk2(dot: Dot): A = areaDot(SquareLink)
  def sq_li(dot: Dot): A = areaDot(SquareLink)
  def sq_lnk(dot: Dot): A = areaDot(SquareLink)
  def ft2(dot: Dot): A = areaDot(SquareFoot)
  def sq_ft(dot: Dot): A = areaDot(SquareFoot)
  def ch2(dot: Dot): A = areaDot(SquareChain)
  def sq_ch(dot: Dot): A = areaDot(SquareChain)
  def yd2(dot: Dot): A = areaDot(SquareYard)
  def sq_yd(dot: Dot): A = areaDot(SquareYard)
  def rd2(dot: Dot): A = areaDot(SquareRod)
  def sq_rd(dot: Dot): A = areaDot(SquareRod)
  def mi2(dot: Dot): A = areaDot(SquareMile)
  def sq_mi(dot: Dot): A = areaDot(SquareMile)
  def ac(dot: Dot): A = areaDot(Acre)
  def ro(dot: Dot): A = areaDot(Rood)
  def circ_mil(dot: Dot): A = areaDot(CircularMil)
  def circ_in(dot: Dot): A = areaDot(CircularInch)
  def bd(dot: Dot): A = areaDot(Board)
}

trait AreaPer[A]{
  import AreaUnit._

  protected def areaPer(unit: AreaUnit): A

  def ym2(per: Per): A = areaPer(YoctoSquareMetre)
  def zm2(per: Per): A = areaPer(ZeptoSquareMetre)
  def am2(per: Per): A = areaPer(AttoSquareMetre)
  def fm2(per: Per): A = areaPer(FemtoSquareMetre)
  def pm2(per: Per): A = areaPer(PicoSquareMetre)
  def nm2(per: Per): A = areaPer(NanoSquareMetre)
  def microSquareMetre(per: Per): A = areaPer(MicroSquareMetre)
  def microM2(per: Per): A = areaPer(MicroSquareMetre)
  def μm2(per: Per): A = areaPer(MicroSquareMetre)
  def mm2(per: Per): A = areaPer(MilliSquareMetre)
  def cm2(per: Per): A = areaPer(CentiSquareMetre)
  def dm2(per: Per): A = areaPer(DeciSquareMetre)
  def m2(per: Per): A = areaPer(SquareMetre)
  def dam2(per: Per): A = areaPer(DecaSquareMetre)
  def hm2(per: Per): A = areaPer(HectoSquareMetre)
  def km2(per: Per): A = areaPer(KiloSquareMetre)
  def Mm2(per: Per): A = areaPer(MegaSquareMetre)
  def Gm2(per: Per): A = areaPer(GigaSquareMetre)
  def Tm2(per: Per): A = areaPer(TeraSquareMetre)
  def Pm2(per: Per): A = areaPer(PetaSquareMetre)
  def Em2(per: Per): A = areaPer(ExaSquareMetre)
  def Zm2(per: Per): A = areaPer(ZettaSquareMetre)
  def Ym2(per: Per): A = areaPer(YottaSquareMetre)
  def a(per: Per): A = areaPer(Are)
  def ha(per: Per): A = areaPer(Hectare)
  def yb(per: Per): A = areaPer(YoctoBarn)
  def zb(per: Per): A = areaPer(ZeptoBarn)
  def ab(per: Per): A = areaPer(AttoBarn)
  def fb(per: Per): A = areaPer(FemtoBarn)
  def pb(per: Per): A = areaPer(PicoBarn)
  def nb(per: Per): A = areaPer(NanoBarn)
  def microBarn(per: Per): A = areaPer(MicroBarn)
  def μb(per: Per): A = areaPer(MicroBarn)
  def mb(per: Per): A = areaPer(MilliBarn)
  def b(per: Per): A = areaPer(Barn)
  def kb(per: Per): A = areaPer(KiloBarn)
  def Mb(per: Per): A = areaPer(MegaBarn)
  def Gb(per: Per): A = areaPer(GigaBarn)
  def Tb(per: Per): A = areaPer(TeraBarn)
  def Pb(per: Per): A = areaPer(PetaBarn)
  def Eb(per: Per): A = areaPer(ExaBarn)
  def Zb(per: Per): A = areaPer(ZettaBarn)
  def Yb(per: Per): A = areaPer(YottaBarn)
  def mil2(per: Per): A = areaPer(SquareMil)
  def sq_mil(per: Per): A = areaPer(SquareMil)
  def in2(per: Per): A = areaPer(SquareInch)
  def sq_in(per: Per): A = areaPer(SquareInch)
  def li2(per: Per): A = areaPer(SquareLink)
  def lnk2(per: Per): A = areaPer(SquareLink)
  def sq_li(per: Per): A = areaPer(SquareLink)
  def sq_lnk(per: Per): A = areaPer(SquareLink)
  def ft2(per: Per): A = areaPer(SquareFoot)
  def sq_ft(per: Per): A = areaPer(SquareFoot)
  def ch2(per: Per): A = areaPer(SquareChain)
  def sq_ch(per: Per): A = areaPer(SquareChain)
  def yd2(per: Per): A = areaPer(SquareYard)
  def sq_yd(per: Per): A = areaPer(SquareYard)
  def rd2(per: Per): A = areaPer(SquareRod)
  def sq_rd(per: Per): A = areaPer(SquareRod)
  def mi2(per: Per): A = areaPer(SquareMile)
  def sq_mi(per: Per): A = areaPer(SquareMile)
  def ac(per: Per): A = areaPer(Acre)
  def ro(per: Per): A = areaPer(Rood)
  def circ_mil(per: Per): A = areaPer(CircularMil)
  def circ_in(per: Per): A = areaPer(CircularInch)
  def bd(per: Per): A = areaPer(Board)
}

trait PredefinedAreaUnit extends AreaPostfixOps[AreaUnit]{
  override protected def areaPostfixOps(unit: AreaUnit) = unit
  
}

object PredefinedAreaUnit extends PredefinedAreaUnit
