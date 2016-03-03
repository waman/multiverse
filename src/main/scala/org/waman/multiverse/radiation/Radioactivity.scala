package org.waman.multiverse.radiation

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait RadioactivityPostfixOps[A]{

  import RadioactivityUnit._

  protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit): A

  def yBq: A = radioactivityPostfixOps(YoctoBecquerel)
  def zBq: A = radioactivityPostfixOps(ZeptoBecquerel)
  def aBq: A = radioactivityPostfixOps(AttoBecquerel)
  def fBq: A = radioactivityPostfixOps(FemtoBecquerel)
  def pBq: A = radioactivityPostfixOps(PicoBecquerel)
  def nBq: A = radioactivityPostfixOps(NanoBecquerel)
  def microBecquerel: A = radioactivityPostfixOps(MicroBecquerel)
  def microBq       : A = microBecquerel
  def μBq: A = microBecquerel
  def mBq: A = radioactivityPostfixOps(MilliBecquerel)
  def cBq: A = radioactivityPostfixOps(CentiBecquerel)
  def dBq: A = radioactivityPostfixOps(DeciBecquerel)
  def Bq : A = radioactivityPostfixOps(Becquerel)
  def daBq: A = radioactivityPostfixOps(DecaBecquerel)
  def hBq: A = radioactivityPostfixOps(HectoBecquerel)
  def kBq: A = radioactivityPostfixOps(KiloBecquerel)
  def MBq: A = radioactivityPostfixOps(MegaBecquerel)
  def GBq: A = radioactivityPostfixOps(GigaBecquerel)
  def TBq: A = radioactivityPostfixOps(TeraBecquerel)
  def PBq: A = radioactivityPostfixOps(PetaBecquerel)
  def EBq: A = radioactivityPostfixOps(ExaBecquerel)
  def ZBq: A = radioactivityPostfixOps(ZettaBecquerel)
  def YBq: A = radioactivityPostfixOps(YottaBecquerel)

  def yCi: A = radioactivityPostfixOps(YoctoCurie)
  def zCi: A = radioactivityPostfixOps(ZeptoCurie)
  def aCi: A = radioactivityPostfixOps(AttoCurie)
  def fCi: A = radioactivityPostfixOps(FemtoCurie)
  def pCi: A = radioactivityPostfixOps(PicoCurie)
  def nCi: A = radioactivityPostfixOps(NanoCurie)
  def microCurie: A = radioactivityPostfixOps(MicroCurie)
  def microCi   : A = microCurie
  def μCi: A = microCurie
  def mCi: A = radioactivityPostfixOps(MilliCurie)
  def cCi: A = radioactivityPostfixOps(CentiCurie)
  def dCi: A = radioactivityPostfixOps(DeciCurie)
  def Ci : A = radioactivityPostfixOps(Curie)
  def daCi: A = radioactivityPostfixOps(DecaCurie)
  def hCi: A = radioactivityPostfixOps(HectoCurie)
  def kCi: A = radioactivityPostfixOps(KiloCurie)
  def MCi: A = radioactivityPostfixOps(MegaCurie)
  def GCi: A = radioactivityPostfixOps(GigaCurie)
  def TCi: A = radioactivityPostfixOps(TeraCurie)
  def PCi: A = radioactivityPostfixOps(PetaCurie)
  def ECi: A = radioactivityPostfixOps(ExaCurie)
  def ZCi: A = radioactivityPostfixOps(ZettaCurie)
  def YCi: A = radioactivityPostfixOps(YottaCurie)
  
  def Rd: A = radioactivityPostfixOps(Rutherford)
}

class Radioactivity[A: Fractional](val value: A, val unit: RadioactivityUnit)
  extends Quantity[A, RadioactivityUnit]
    with RadioactivityPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: RadioactivityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInBecquerel) / real(evalUnit.unitInBecquerel)

  override protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit) = apply(radioactivityUnit)
}

sealed abstract class RadioactivityUnit(val symbols: Seq[String], val unitInBecquerel: Real)
    extends PhysicalUnit[RadioactivityUnit]{

  def this(symbols: Seq[String], factor: Real, unit: RadioactivityUnit) =
    this(symbols, factor * unit.unitInBecquerel)

  override def baseUnit = RadioactivityUnit.Becquerel
  override def valueInBaseUnit = unitInBecquerel
}

object RadioactivityUnit extends ConstantsDefined[RadioactivityUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)
  
  // intrinsic
  case object YoctoBecquerel extends RadioactivityUnit("yBq", r"1e-24")
  case object ZeptoBecquerel extends RadioactivityUnit("zBq", r"1e-21")
  case object AttoBecquerel  extends RadioactivityUnit("aBq", r"1e-18")
  case object FemtoBecquerel extends RadioactivityUnit("fBq", r"1e-15")
  case object PicoBecquerel  extends RadioactivityUnit("pBq", r"1e-12")
  case object NanoBecquerel  extends RadioactivityUnit("nBq", r"1e-9")
  case object MicroBecquerel extends RadioactivityUnit(Seq("μBq", "microBecquerel", "microBq"), r"1e-6")
  case object MilliBecquerel extends RadioactivityUnit("mBq", r"1e-3")
  case object CentiBecquerel extends RadioactivityUnit("cBq", r"1e-2")
  case object DeciBecquerel  extends RadioactivityUnit("dBq", r"1e-1")
  case object Becquerel      extends RadioactivityUnit("Bq" , 1)
  case object DecaBecquerel  extends RadioactivityUnit("daBq", r"1e1")
  case object HectoBecquerel extends RadioactivityUnit("hBq", r"1e2")
  case object KiloBecquerel  extends RadioactivityUnit("kBq", r"1e3")
  case object MegaBecquerel  extends RadioactivityUnit("MBq", r"1e6")
  case object GigaBecquerel  extends RadioactivityUnit("GBq", r"1e9")
  case object TeraBecquerel  extends RadioactivityUnit("TBq", r"1e12")
  case object PetaBecquerel  extends RadioactivityUnit("PBq", r"1e15")
  case object ExaBecquerel   extends RadioactivityUnit("EBq", r"1e18")
  case object ZettaBecquerel extends RadioactivityUnit("ZBq", r"1e21")
  case object YottaBecquerel extends RadioactivityUnit("YBq", r"1e24")

  case object YoctoCurie extends RadioactivityUnit("yCi", r"1e-24", Curie)
  case object ZeptoCurie extends RadioactivityUnit("zCi", r"1e-21", Curie)
  case object AttoCurie  extends RadioactivityUnit("aCi", r"1e-18", Curie)
  case object FemtoCurie extends RadioactivityUnit("fCi", r"1e-15", Curie)
  case object PicoCurie  extends RadioactivityUnit("pCi", r"1e-12", Curie)
  case object NanoCurie  extends RadioactivityUnit("nCi", r"1e-9", Curie)
  case object MicroCurie extends RadioactivityUnit(Seq("μCi", "microCurie", "microCi"), r"1e-6", Curie)
  case object MilliCurie extends RadioactivityUnit("mCi", r"1e-3", Curie)
  case object CentiCurie extends RadioactivityUnit("cCi", r"1e-2", Curie)
  case object DeciCurie  extends RadioactivityUnit("dCi", r"1e-1", Curie)
  case object Curie      extends RadioactivityUnit("Ci", r"3.7e10")
  case object DecaCurie  extends RadioactivityUnit("daCi", r"1e1", Curie)
  case object HectoCurie extends RadioactivityUnit("hCi", r"1e2", Curie)
  case object KiloCurie  extends RadioactivityUnit("kCi", r"1e3", Curie)
  case object MegaCurie  extends RadioactivityUnit("MCi", r"1e6", Curie)
  case object GigaCurie  extends RadioactivityUnit("GCi", r"1e9", Curie)
  case object TeraCurie  extends RadioactivityUnit("TCi", r"1e12", Curie)
  case object PetaCurie  extends RadioactivityUnit("PCi", r"1e15", Curie)
  case object ExaCurie   extends RadioactivityUnit("ECi", r"1e18", Curie)
  case object ZettaCurie extends RadioactivityUnit("ZCi", r"1e21", Curie)
  case object YottaCurie extends RadioactivityUnit("YCi", r"1e24", Curie)
  
  case object Rutherford extends RadioactivityUnit("Rd", 1, MegaBecquerel)

  override lazy val values = Seq(
    YoctoBecquerel,
    ZeptoBecquerel,
    AttoBecquerel,
    FemtoBecquerel,
    PicoBecquerel,
    NanoBecquerel,
    MicroBecquerel,
    MilliBecquerel,
    CentiBecquerel,
    DeciBecquerel,
    Becquerel,
    DecaBecquerel,
    HectoBecquerel,
    KiloBecquerel,
    MegaBecquerel,
    GigaBecquerel,
    TeraBecquerel,
    PetaBecquerel,
    ExaBecquerel,
    ZettaBecquerel,
    YottaBecquerel,

    YoctoCurie,
    ZeptoCurie,
    AttoCurie,
    FemtoCurie,
    PicoCurie,
    NanoCurie,
    MicroCurie,
    MilliCurie,
    CentiCurie,
    DeciCurie,
    Curie,
    DecaCurie,
    HectoCurie,
    KiloCurie,
    MegaCurie,
    GigaCurie,
    TeraCurie,
    PetaCurie,
    ExaCurie,
    ZettaCurie,
    YottaCurie,

    Rutherford
  )
}

trait PredefinedRadioactivityUnit extends RadioactivityPostfixOps[RadioactivityUnit]{

  override protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit) = radioactivityUnit
}

object PredefinedRadioactivityUnit extends PredefinedRadioactivityUnit

trait RadioactivityFactory[A]
    extends RadioactivityPostfixOps[Radioactivity[A]]{

  def apply(unit: RadioactivityUnit): Radioactivity[A]

  override protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit) =
    apply(radioactivityUnit)
}