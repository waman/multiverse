package org.waman.multiverse.fluid

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.mass.DensityUnit
import org.waman.multiverse.time._
import org.waman.multiverse.mechanics._
import org.waman.multiverse.metric.LengthUnit._
import org.waman.multiverse.metric.AreaUnit._
import org.waman.multiverse.mechanics.ForceUnit._
import org.waman.multiverse.mass.DensityUnit._
import org.waman.multiverse.mechanics.AccelerationUnit.StandardGravity

sealed trait PressureUnit extends PhysicalUnit[PressureUnit]
  with MultiplicativeByTimeUnit[DynamicViscosityUnit]{

  override def getSIUnit = org.waman.multiverse.fluid.PressureUnit.Pascal

  override def *(unit: TimeUnit) = DynamicViscosityUnit(this, unit)
}

object PressureUnit extends ConstantsDefined[PressureUnit]{

  // intrinsic
  private[PressureUnit]
  class IntrinsicPressureUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends PressureUnit{

    def this(name: String, symbols: Seq[String], unit: PressureUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: PressureUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoPascal extends IntrinsicPressureUnit("YoctoPascal", Seq("yPa"), r"1e-24")
  case object ZeptoPascal extends IntrinsicPressureUnit("ZeptoPascal", Seq("zPa"), r"1e-21")
  case object AttoPascal extends IntrinsicPressureUnit("AttoPascal", Seq("aPa"), r"1e-18")
  case object FemtoPascal extends IntrinsicPressureUnit("FemtoPascal", Seq("fPa"), r"1e-15")
  case object PicoPascal extends IntrinsicPressureUnit("PicoPascal", Seq("pPa"), r"1e-12")
  case object NanoPascal extends IntrinsicPressureUnit("NanoPascal", Seq("nPa"), r"1e-9")
  case object MicroPascal extends IntrinsicPressureUnit("MicroPascal", Seq("μPa", "mcPa"), r"1e-6")
  case object MilliPascal extends IntrinsicPressureUnit("MilliPascal", Seq("mPa"), r"1e-3")
  case object CentiPascal extends IntrinsicPressureUnit("CentiPascal", Seq("cPa"), r"1e-2")
  case object DeciPascal extends IntrinsicPressureUnit("DeciPascal", Seq("dPa"), r"1e-1")
  case object Pascal extends IntrinsicPressureUnit("Pascal", Seq("Pa"), r"1")
  case object DecaPascal extends IntrinsicPressureUnit("DecaPascal", Seq("daPa"), r"1e1")
  case object HectoPascal extends IntrinsicPressureUnit("HectoPascal", Seq("hPa"), r"1e2")
  case object KiloPascal extends IntrinsicPressureUnit("KiloPascal", Seq("kPa"), r"1e3")
  case object MegaPascal extends IntrinsicPressureUnit("MegaPascal", Seq("MPa"), r"1e6")
  case object GigaPascal extends IntrinsicPressureUnit("GigaPascal", Seq("GPa"), r"1e9")
  case object TeraPascal extends IntrinsicPressureUnit("TeraPascal", Seq("TPa"), r"1e12")
  case object PetaPascal extends IntrinsicPressureUnit("PetaPascal", Seq("PPa"), r"1e15")
  case object ExaPascal extends IntrinsicPressureUnit("ExaPascal", Seq("EPa"), r"1e18")
  case object ZettaPascal extends IntrinsicPressureUnit("ZettaPascal", Seq("ZPa"), r"1e21")
  case object YottaPascal extends IntrinsicPressureUnit("YottaPascal", Seq("YPa"), r"1e24")
  case object Barye extends IntrinsicPressureUnit("Barye", Seq("Ba"), Dyne / SquareCentiMetre)
  case object Atmosphere extends IntrinsicPressureUnit("Atmosphere", Seq("atm"), 101325)
  case object Atmosphere_technical extends IntrinsicPressureUnit("Atmosphere_technical", Seq("at"), KiloGramForce / SquareCentiMetre)
  case object Bar extends IntrinsicPressureUnit("Bar", Seq("bar"), r"1e5")
  case object Pieze extends IntrinsicPressureUnit("Pieze", Seq("pz"), 1000)
  case object Torr extends IntrinsicPressureUnit("Torr", Seq("torr"), Atmosphere.unitValueInSIUnit / 760)
  case object KipPerSquareInch extends IntrinsicPressureUnit("KipPerSquareInch", Seq("ksi"), KipForce / SquareInch)
  case object PoundPerSquareFoot extends IntrinsicPressureUnit("PoundPerSquareFoot", Seq("psf"), PoundForce / SquareFoot)
  case object PoundPerSquareInch extends IntrinsicPressureUnit("PoundPerSquareInch", Seq("psi"), PoundForce / SquareInch)
  case object MicroMetreOfMercury extends IntrinsicPressureUnit("MicroMetreOfMercury", Seq("μmHg", "microMetreHg"), Mercury.unitValueInSIUnit * MicroMetre.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object MilliMetreOfMercury extends IntrinsicPressureUnit("MilliMetreOfMercury", Seq("mmHg"), Mercury.unitValueInSIUnit * MilliMetre.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object CentiMetreOfMercury extends IntrinsicPressureUnit("CentiMetreOfMercury", Seq("cmHg"), Mercury.unitValueInSIUnit * CentiMetre.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object InchOfMercury extends IntrinsicPressureUnit("InchOfMercury", Seq("inHg"), Mercury.unitValueInSIUnit * Inch.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object FootOfMercury extends IntrinsicPressureUnit("FootOfMercury", Seq("ftHg"), Mercury.unitValueInSIUnit * Foot.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object MilliMetreOfWater extends IntrinsicPressureUnit("MilliMetreOfWater", Seq("mmH2O"), Water.unitValueInSIUnit * MilliMetre.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object CentiMetreOfWater extends IntrinsicPressureUnit("CentiMetreOfWater", Seq("cmH2O"), Water.unitValueInSIUnit * CentiMetre.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object InchOfWater extends IntrinsicPressureUnit("InchOfWater", Seq("inH2O"), Water.unitValueInSIUnit * Inch.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)
  case object FootOfWater extends IntrinsicPressureUnit("FootOfWater", Seq("ftH2O"), Water.unitValueInSIUnit * Foot.unitValueInSIUnit * StandardGravity.unitValueInSIUnit)

  override lazy val values = Seq(YoctoPascal, ZeptoPascal, AttoPascal, FemtoPascal, PicoPascal, NanoPascal, MicroPascal, MilliPascal, CentiPascal, DeciPascal, Pascal, DecaPascal, HectoPascal, KiloPascal, MegaPascal, GigaPascal, TeraPascal, PetaPascal, ExaPascal, ZettaPascal, YottaPascal, Barye, Atmosphere, Atmosphere_technical, Bar, Pieze, Torr, KipPerSquareInch, PoundPerSquareFoot, PoundPerSquareInch, MicroMetreOfMercury, MilliMetreOfMercury, CentiMetreOfMercury, InchOfMercury, FootOfMercury, MilliMetreOfWater, CentiMetreOfWater, InchOfWater, FootOfWater)

  // ForceUnit / AreaUnit -> Pressure
  private[PressureUnit]
  class QuotientForcePerAreaUnit(val numeratorUnit: ForceUnit, val denominatorUnit: AreaUnit)
      extends PressureUnit with QuotientUnit[PressureUnit, ForceUnit, AreaUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: ForceUnit, dUnit: AreaUnit): PressureUnit =
    new QuotientForcePerAreaUnit(nUnit, dUnit)
}

trait MultiplicativeByPressureUnit[R]{
  def *(unit: PressureUnit): R
}

trait DivisibleByPressureUnit[R]{
  def /(unit: PressureUnit): R
}

trait PressurePostfixOps[A]{
  import PressureUnit._

  protected def pressurePostfixOps(unit: PressureUnit): A


  def yPa : A = pressurePostfixOps(YoctoPascal)
  def zPa : A = pressurePostfixOps(ZeptoPascal)
  def aPa : A = pressurePostfixOps(AttoPascal)
  def fPa : A = pressurePostfixOps(FemtoPascal)
  def pPa : A = pressurePostfixOps(PicoPascal)
  def nPa : A = pressurePostfixOps(NanoPascal)
  def μPa : A = pressurePostfixOps(MicroPascal)
  def mcPa : A = pressurePostfixOps(MicroPascal)
  def mPa : A = pressurePostfixOps(MilliPascal)
  def cPa : A = pressurePostfixOps(CentiPascal)
  def dPa : A = pressurePostfixOps(DeciPascal)
  def Pa : A = pressurePostfixOps(Pascal)
  def daPa : A = pressurePostfixOps(DecaPascal)
  def hPa : A = pressurePostfixOps(HectoPascal)
  def kPa : A = pressurePostfixOps(KiloPascal)
  def MPa : A = pressurePostfixOps(MegaPascal)
  def GPa : A = pressurePostfixOps(GigaPascal)
  def TPa : A = pressurePostfixOps(TeraPascal)
  def PPa : A = pressurePostfixOps(PetaPascal)
  def EPa : A = pressurePostfixOps(ExaPascal)
  def ZPa : A = pressurePostfixOps(ZettaPascal)
  def YPa : A = pressurePostfixOps(YottaPascal)
  def Ba : A = pressurePostfixOps(Barye)
  def atm : A = pressurePostfixOps(Atmosphere)
  def at : A = pressurePostfixOps(Atmosphere_technical)
  def bar : A = pressurePostfixOps(Bar)
  def pz : A = pressurePostfixOps(Pieze)
  def torr : A = pressurePostfixOps(Torr)
  def ksi : A = pressurePostfixOps(KipPerSquareInch)
  def psf : A = pressurePostfixOps(PoundPerSquareFoot)
  def psi : A = pressurePostfixOps(PoundPerSquareInch)
  def μmHg : A = pressurePostfixOps(MicroMetreOfMercury)
  def microMetreHg : A = pressurePostfixOps(MicroMetreOfMercury)
  def mmHg : A = pressurePostfixOps(MilliMetreOfMercury)
  def cmHg : A = pressurePostfixOps(CentiMetreOfMercury)
  def inHg : A = pressurePostfixOps(InchOfMercury)
  def ftHg : A = pressurePostfixOps(FootOfMercury)
  def mmH2O : A = pressurePostfixOps(MilliMetreOfWater)
  def cmH2O : A = pressurePostfixOps(CentiMetreOfWater)
  def inH2O : A = pressurePostfixOps(InchOfWater)
  def ftH2O : A = pressurePostfixOps(FootOfWater)
}

trait PressureDot[A]{
  import PressureUnit._

  protected def pressureDot(unit: PressureUnit): A

  def yPa(dot: Dot): A = pressureDot(YoctoPascal)
  def zPa(dot: Dot): A = pressureDot(ZeptoPascal)
  def aPa(dot: Dot): A = pressureDot(AttoPascal)
  def fPa(dot: Dot): A = pressureDot(FemtoPascal)
  def pPa(dot: Dot): A = pressureDot(PicoPascal)
  def nPa(dot: Dot): A = pressureDot(NanoPascal)
  def μPa(dot: Dot): A = pressureDot(MicroPascal)
  def mcPa(dot: Dot): A = pressureDot(MicroPascal)
  def mPa(dot: Dot): A = pressureDot(MilliPascal)
  def cPa(dot: Dot): A = pressureDot(CentiPascal)
  def dPa(dot: Dot): A = pressureDot(DeciPascal)
  def Pa(dot: Dot): A = pressureDot(Pascal)
  def daPa(dot: Dot): A = pressureDot(DecaPascal)
  def hPa(dot: Dot): A = pressureDot(HectoPascal)
  def kPa(dot: Dot): A = pressureDot(KiloPascal)
  def MPa(dot: Dot): A = pressureDot(MegaPascal)
  def GPa(dot: Dot): A = pressureDot(GigaPascal)
  def TPa(dot: Dot): A = pressureDot(TeraPascal)
  def PPa(dot: Dot): A = pressureDot(PetaPascal)
  def EPa(dot: Dot): A = pressureDot(ExaPascal)
  def ZPa(dot: Dot): A = pressureDot(ZettaPascal)
  def YPa(dot: Dot): A = pressureDot(YottaPascal)
  def Ba(dot: Dot): A = pressureDot(Barye)
  def atm(dot: Dot): A = pressureDot(Atmosphere)
  def at(dot: Dot): A = pressureDot(Atmosphere_technical)
  def bar(dot: Dot): A = pressureDot(Bar)
  def pz(dot: Dot): A = pressureDot(Pieze)
  def torr(dot: Dot): A = pressureDot(Torr)
  def ksi(dot: Dot): A = pressureDot(KipPerSquareInch)
  def psf(dot: Dot): A = pressureDot(PoundPerSquareFoot)
  def psi(dot: Dot): A = pressureDot(PoundPerSquareInch)
  def μmHg(dot: Dot): A = pressureDot(MicroMetreOfMercury)
  def microMetreHg(dot: Dot): A = pressureDot(MicroMetreOfMercury)
  def mmHg(dot: Dot): A = pressureDot(MilliMetreOfMercury)
  def cmHg(dot: Dot): A = pressureDot(CentiMetreOfMercury)
  def inHg(dot: Dot): A = pressureDot(InchOfMercury)
  def ftHg(dot: Dot): A = pressureDot(FootOfMercury)
  def mmH2O(dot: Dot): A = pressureDot(MilliMetreOfWater)
  def cmH2O(dot: Dot): A = pressureDot(CentiMetreOfWater)
  def inH2O(dot: Dot): A = pressureDot(InchOfWater)
  def ftH2O(dot: Dot): A = pressureDot(FootOfWater)
}

trait PressurePer[A]{
  import PressureUnit._

  protected def pressurePer(unit: PressureUnit): A

  def yPa(per: Per): A = pressurePer(YoctoPascal)
  def zPa(per: Per): A = pressurePer(ZeptoPascal)
  def aPa(per: Per): A = pressurePer(AttoPascal)
  def fPa(per: Per): A = pressurePer(FemtoPascal)
  def pPa(per: Per): A = pressurePer(PicoPascal)
  def nPa(per: Per): A = pressurePer(NanoPascal)
  def μPa(per: Per): A = pressurePer(MicroPascal)
  def mcPa(per: Per): A = pressurePer(MicroPascal)
  def mPa(per: Per): A = pressurePer(MilliPascal)
  def cPa(per: Per): A = pressurePer(CentiPascal)
  def dPa(per: Per): A = pressurePer(DeciPascal)
  def Pa(per: Per): A = pressurePer(Pascal)
  def daPa(per: Per): A = pressurePer(DecaPascal)
  def hPa(per: Per): A = pressurePer(HectoPascal)
  def kPa(per: Per): A = pressurePer(KiloPascal)
  def MPa(per: Per): A = pressurePer(MegaPascal)
  def GPa(per: Per): A = pressurePer(GigaPascal)
  def TPa(per: Per): A = pressurePer(TeraPascal)
  def PPa(per: Per): A = pressurePer(PetaPascal)
  def EPa(per: Per): A = pressurePer(ExaPascal)
  def ZPa(per: Per): A = pressurePer(ZettaPascal)
  def YPa(per: Per): A = pressurePer(YottaPascal)
  def Ba(per: Per): A = pressurePer(Barye)
  def atm(per: Per): A = pressurePer(Atmosphere)
  def at(per: Per): A = pressurePer(Atmosphere_technical)
  def bar(per: Per): A = pressurePer(Bar)
  def pz(per: Per): A = pressurePer(Pieze)
  def torr(per: Per): A = pressurePer(Torr)
  def ksi(per: Per): A = pressurePer(KipPerSquareInch)
  def psf(per: Per): A = pressurePer(PoundPerSquareFoot)
  def psi(per: Per): A = pressurePer(PoundPerSquareInch)
  def μmHg(per: Per): A = pressurePer(MicroMetreOfMercury)
  def microMetreHg(per: Per): A = pressurePer(MicroMetreOfMercury)
  def mmHg(per: Per): A = pressurePer(MilliMetreOfMercury)
  def cmHg(per: Per): A = pressurePer(CentiMetreOfMercury)
  def inHg(per: Per): A = pressurePer(InchOfMercury)
  def ftHg(per: Per): A = pressurePer(FootOfMercury)
  def mmH2O(per: Per): A = pressurePer(MilliMetreOfWater)
  def cmH2O(per: Per): A = pressurePer(CentiMetreOfWater)
  def inH2O(per: Per): A = pressurePer(InchOfWater)
  def ftH2O(per: Per): A = pressurePer(FootOfWater)
}

trait PredefinedPressureUnit extends PressurePostfixOps[PressureUnit]{
  override protected def pressurePostfixOps(unit: PressureUnit) = unit
  
}

object PredefinedPressureUnit extends PredefinedPressureUnit
