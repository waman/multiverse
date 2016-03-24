package org.waman.multiverse.radiation

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._


sealed trait RadioactivityUnit extends PhysicalUnit[RadioactivityUnit]{

  def unitInBecquerel: Real

  override def baseUnit = org.waman.multiverse.radiation.RadioactivityUnit.Becquerel
  override def valueInBaseUnit = unitInBecquerel
}

object RadioactivityUnit extends ConstantsDefined[RadioactivityUnit]{

  // intrinsic
  private[RadioactivityUnit]
  class IntrinsicRadioactivityUnit(name: String, val symbols: Seq[String], val unitInBecquerel: Real)
      extends RadioactivityUnit{

    def this(name: String, symbols: Seq[String], unit: RadioactivityUnit) =
      this(name, symbols, unit.unitInBecquerel)

    def this(name: String, symbols: Seq[String], factor: Real, unit: RadioactivityUnit) =
      this(name, symbols, factor * unit.unitInBecquerel)
  }


  case object YoctoBecquerel extends IntrinsicRadioactivityUnit("YoctoBecquerel", Seq("yBq"), r"1e-24")
  case object ZeptoBecquerel extends IntrinsicRadioactivityUnit("ZeptoBecquerel", Seq("zBq"), r"1e-21")
  case object AttoBecquerel extends IntrinsicRadioactivityUnit("AttoBecquerel", Seq("aBq"), r"1e-18")
  case object FemtoBecquerel extends IntrinsicRadioactivityUnit("FemtoBecquerel", Seq("fBq"), r"1e-15")
  case object PicoBecquerel extends IntrinsicRadioactivityUnit("PicoBecquerel", Seq("pBq"), r"1e-12")
  case object NanoBecquerel extends IntrinsicRadioactivityUnit("NanoBecquerel", Seq("nBq"), r"1e-9")
  case object MicroBecquerel extends IntrinsicRadioactivityUnit("MicroBecquerel", Seq("microBecquerel", "microBq", "μBq"), r"1e-6")
  case object MilliBecquerel extends IntrinsicRadioactivityUnit("MilliBecquerel", Seq("mBq"), r"1e-3")
  case object CentiBecquerel extends IntrinsicRadioactivityUnit("CentiBecquerel", Seq("cBq"), r"1e-2")
  case object DeciBecquerel extends IntrinsicRadioactivityUnit("DeciBecquerel", Seq("dBq"), r"1e-1")
  case object Becquerel extends IntrinsicRadioactivityUnit("Becquerel", Seq("Bq"), r"1")
  case object DecaBecquerel extends IntrinsicRadioactivityUnit("DecaBecquerel", Seq("daBq"), r"1e1")
  case object HectoBecquerel extends IntrinsicRadioactivityUnit("HectoBecquerel", Seq("hBq"), r"1e2")
  case object KiloBecquerel extends IntrinsicRadioactivityUnit("KiloBecquerel", Seq("kBq"), r"1e3")
  case object MegaBecquerel extends IntrinsicRadioactivityUnit("MegaBecquerel", Seq("MBq"), r"1e6")
  case object GigaBecquerel extends IntrinsicRadioactivityUnit("GigaBecquerel", Seq("GBq"), r"1e9")
  case object TeraBecquerel extends IntrinsicRadioactivityUnit("TeraBecquerel", Seq("TBq"), r"1e12")
  case object PetaBecquerel extends IntrinsicRadioactivityUnit("PetaBecquerel", Seq("PBq"), r"1e15")
  case object ExaBecquerel extends IntrinsicRadioactivityUnit("ExaBecquerel", Seq("EBq"), r"1e18")
  case object ZettaBecquerel extends IntrinsicRadioactivityUnit("ZettaBecquerel", Seq("ZBq"), r"1e21")
  case object YottaBecquerel extends IntrinsicRadioactivityUnit("YottaBecquerel", Seq("YBq"), r"1e24")
  case object YoctoCurie extends IntrinsicRadioactivityUnit("YoctoCurie", Seq("yCi"), r"1e-24" * r"3.7e10")
  case object ZeptoCurie extends IntrinsicRadioactivityUnit("ZeptoCurie", Seq("zCi"), r"1e-21" * r"3.7e10")
  case object AttoCurie extends IntrinsicRadioactivityUnit("AttoCurie", Seq("aCi"), r"1e-18" * r"3.7e10")
  case object FemtoCurie extends IntrinsicRadioactivityUnit("FemtoCurie", Seq("fCi"), r"1e-15" * r"3.7e10")
  case object PicoCurie extends IntrinsicRadioactivityUnit("PicoCurie", Seq("pCi"), r"1e-12" * r"3.7e10")
  case object NanoCurie extends IntrinsicRadioactivityUnit("NanoCurie", Seq("nCi"), r"1e-9" * r"3.7e10")
  case object MicroCurie extends IntrinsicRadioactivityUnit("MicroCurie", Seq("microCurie", "microCi", "μCi"), r"1e-6" * r"3.7e10")
  case object MilliCurie extends IntrinsicRadioactivityUnit("MilliCurie", Seq("mCi"), r"1e-3" * r"3.7e10")
  case object CentiCurie extends IntrinsicRadioactivityUnit("CentiCurie", Seq("cCi"), r"1e-2" * r"3.7e10")
  case object DeciCurie extends IntrinsicRadioactivityUnit("DeciCurie", Seq("dCi"), r"1e-1" * r"3.7e10")
  case object Curie extends IntrinsicRadioactivityUnit("Curie", Seq("Ci"), r"1" * r"3.7e10")
  case object DecaCurie extends IntrinsicRadioactivityUnit("DecaCurie", Seq("daCi"), r"1e1" * r"3.7e10")
  case object HectoCurie extends IntrinsicRadioactivityUnit("HectoCurie", Seq("hCi"), r"1e2" * r"3.7e10")
  case object KiloCurie extends IntrinsicRadioactivityUnit("KiloCurie", Seq("kCi"), r"1e3" * r"3.7e10")
  case object MegaCurie extends IntrinsicRadioactivityUnit("MegaCurie", Seq("MCi"), r"1e6" * r"3.7e10")
  case object GigaCurie extends IntrinsicRadioactivityUnit("GigaCurie", Seq("GCi"), r"1e9" * r"3.7e10")
  case object TeraCurie extends IntrinsicRadioactivityUnit("TeraCurie", Seq("TCi"), r"1e12" * r"3.7e10")
  case object PetaCurie extends IntrinsicRadioactivityUnit("PetaCurie", Seq("PCi"), r"1e15" * r"3.7e10")
  case object ExaCurie extends IntrinsicRadioactivityUnit("ExaCurie", Seq("ECi"), r"1e18" * r"3.7e10")
  case object ZettaCurie extends IntrinsicRadioactivityUnit("ZettaCurie", Seq("ZCi"), r"1e21" * r"3.7e10")
  case object YottaCurie extends IntrinsicRadioactivityUnit("YottaCurie", Seq("YCi"), r"1e24" * r"3.7e10")
  case object Rutherford extends IntrinsicRadioactivityUnit("Rutherford", Seq("Rd"), 1, MegaBecquerel)

  override lazy val values = Seq(YoctoBecquerel, ZeptoBecquerel, AttoBecquerel, FemtoBecquerel, PicoBecquerel, NanoBecquerel, MicroBecquerel, MilliBecquerel, CentiBecquerel, DeciBecquerel, Becquerel, DecaBecquerel, HectoBecquerel, KiloBecquerel, MegaBecquerel, GigaBecquerel, TeraBecquerel, PetaBecquerel, ExaBecquerel, ZettaBecquerel, YottaBecquerel, YoctoCurie, ZeptoCurie, AttoCurie, FemtoCurie, PicoCurie, NanoCurie, MicroCurie, MilliCurie, CentiCurie, DeciCurie, Curie, DecaCurie, HectoCurie, KiloCurie, MegaCurie, GigaCurie, TeraCurie, PetaCurie, ExaCurie, ZettaCurie, YottaCurie, Rutherford)
}

trait MultiplicativeByRadioactivityUnit[R]{
  def *(unit: RadioactivityUnit): R
}

trait DivisibleByRadioactivityUnit[R]{
  def /(unit: RadioactivityUnit): R
}

trait RadioactivityPostfixOps[A]{
  import RadioactivityUnit._

  protected def radioactivityPostfixOps(unit: RadioactivityUnit): A


  def yBq : A = radioactivityPostfixOps(YoctoBecquerel)
  def zBq : A = radioactivityPostfixOps(ZeptoBecquerel)
  def aBq : A = radioactivityPostfixOps(AttoBecquerel)
  def fBq : A = radioactivityPostfixOps(FemtoBecquerel)
  def pBq : A = radioactivityPostfixOps(PicoBecquerel)
  def nBq : A = radioactivityPostfixOps(NanoBecquerel)
  def microBecquerel : A = radioactivityPostfixOps(MicroBecquerel)
  def microBq : A = radioactivityPostfixOps(MicroBecquerel)
  def μBq : A = radioactivityPostfixOps(MicroBecquerel)
  def mBq : A = radioactivityPostfixOps(MilliBecquerel)
  def cBq : A = radioactivityPostfixOps(CentiBecquerel)
  def dBq : A = radioactivityPostfixOps(DeciBecquerel)
  def Bq : A = radioactivityPostfixOps(Becquerel)
  def daBq : A = radioactivityPostfixOps(DecaBecquerel)
  def hBq : A = radioactivityPostfixOps(HectoBecquerel)
  def kBq : A = radioactivityPostfixOps(KiloBecquerel)
  def MBq : A = radioactivityPostfixOps(MegaBecquerel)
  def GBq : A = radioactivityPostfixOps(GigaBecquerel)
  def TBq : A = radioactivityPostfixOps(TeraBecquerel)
  def PBq : A = radioactivityPostfixOps(PetaBecquerel)
  def EBq : A = radioactivityPostfixOps(ExaBecquerel)
  def ZBq : A = radioactivityPostfixOps(ZettaBecquerel)
  def YBq : A = radioactivityPostfixOps(YottaBecquerel)
  def yCi : A = radioactivityPostfixOps(YoctoCurie)
  def zCi : A = radioactivityPostfixOps(ZeptoCurie)
  def aCi : A = radioactivityPostfixOps(AttoCurie)
  def fCi : A = radioactivityPostfixOps(FemtoCurie)
  def pCi : A = radioactivityPostfixOps(PicoCurie)
  def nCi : A = radioactivityPostfixOps(NanoCurie)
  def microCurie : A = radioactivityPostfixOps(MicroCurie)
  def microCi : A = radioactivityPostfixOps(MicroCurie)
  def μCi : A = radioactivityPostfixOps(MicroCurie)
  def mCi : A = radioactivityPostfixOps(MilliCurie)
  def cCi : A = radioactivityPostfixOps(CentiCurie)
  def dCi : A = radioactivityPostfixOps(DeciCurie)
  def Ci : A = radioactivityPostfixOps(Curie)
  def daCi : A = radioactivityPostfixOps(DecaCurie)
  def hCi : A = radioactivityPostfixOps(HectoCurie)
  def kCi : A = radioactivityPostfixOps(KiloCurie)
  def MCi : A = radioactivityPostfixOps(MegaCurie)
  def GCi : A = radioactivityPostfixOps(GigaCurie)
  def TCi : A = radioactivityPostfixOps(TeraCurie)
  def PCi : A = radioactivityPostfixOps(PetaCurie)
  def ECi : A = radioactivityPostfixOps(ExaCurie)
  def ZCi : A = radioactivityPostfixOps(ZettaCurie)
  def YCi : A = radioactivityPostfixOps(YottaCurie)
  def Rd : A = radioactivityPostfixOps(Rutherford)
}

trait RadioactivityDot[A]{
  import RadioactivityUnit._

  protected def radioactivityDot(unit: RadioactivityUnit): A

  def yBq(dot: Dot): A = radioactivityDot(YoctoBecquerel)
  def zBq(dot: Dot): A = radioactivityDot(ZeptoBecquerel)
  def aBq(dot: Dot): A = radioactivityDot(AttoBecquerel)
  def fBq(dot: Dot): A = radioactivityDot(FemtoBecquerel)
  def pBq(dot: Dot): A = radioactivityDot(PicoBecquerel)
  def nBq(dot: Dot): A = radioactivityDot(NanoBecquerel)
  def microBecquerel(dot: Dot): A = radioactivityDot(MicroBecquerel)
  def microBq(dot: Dot): A = radioactivityDot(MicroBecquerel)
  def μBq(dot: Dot): A = radioactivityDot(MicroBecquerel)
  def mBq(dot: Dot): A = radioactivityDot(MilliBecquerel)
  def cBq(dot: Dot): A = radioactivityDot(CentiBecquerel)
  def dBq(dot: Dot): A = radioactivityDot(DeciBecquerel)
  def Bq(dot: Dot): A = radioactivityDot(Becquerel)
  def daBq(dot: Dot): A = radioactivityDot(DecaBecquerel)
  def hBq(dot: Dot): A = radioactivityDot(HectoBecquerel)
  def kBq(dot: Dot): A = radioactivityDot(KiloBecquerel)
  def MBq(dot: Dot): A = radioactivityDot(MegaBecquerel)
  def GBq(dot: Dot): A = radioactivityDot(GigaBecquerel)
  def TBq(dot: Dot): A = radioactivityDot(TeraBecquerel)
  def PBq(dot: Dot): A = radioactivityDot(PetaBecquerel)
  def EBq(dot: Dot): A = radioactivityDot(ExaBecquerel)
  def ZBq(dot: Dot): A = radioactivityDot(ZettaBecquerel)
  def YBq(dot: Dot): A = radioactivityDot(YottaBecquerel)
  def yCi(dot: Dot): A = radioactivityDot(YoctoCurie)
  def zCi(dot: Dot): A = radioactivityDot(ZeptoCurie)
  def aCi(dot: Dot): A = radioactivityDot(AttoCurie)
  def fCi(dot: Dot): A = radioactivityDot(FemtoCurie)
  def pCi(dot: Dot): A = radioactivityDot(PicoCurie)
  def nCi(dot: Dot): A = radioactivityDot(NanoCurie)
  def microCurie(dot: Dot): A = radioactivityDot(MicroCurie)
  def microCi(dot: Dot): A = radioactivityDot(MicroCurie)
  def μCi(dot: Dot): A = radioactivityDot(MicroCurie)
  def mCi(dot: Dot): A = radioactivityDot(MilliCurie)
  def cCi(dot: Dot): A = radioactivityDot(CentiCurie)
  def dCi(dot: Dot): A = radioactivityDot(DeciCurie)
  def Ci(dot: Dot): A = radioactivityDot(Curie)
  def daCi(dot: Dot): A = radioactivityDot(DecaCurie)
  def hCi(dot: Dot): A = radioactivityDot(HectoCurie)
  def kCi(dot: Dot): A = radioactivityDot(KiloCurie)
  def MCi(dot: Dot): A = radioactivityDot(MegaCurie)
  def GCi(dot: Dot): A = radioactivityDot(GigaCurie)
  def TCi(dot: Dot): A = radioactivityDot(TeraCurie)
  def PCi(dot: Dot): A = radioactivityDot(PetaCurie)
  def ECi(dot: Dot): A = radioactivityDot(ExaCurie)
  def ZCi(dot: Dot): A = radioactivityDot(ZettaCurie)
  def YCi(dot: Dot): A = radioactivityDot(YottaCurie)
  def Rd(dot: Dot): A = radioactivityDot(Rutherford)
}

trait RadioactivityPer[A]{
  import RadioactivityUnit._

  protected def radioactivityPer(unit: RadioactivityUnit): A

  def yBq(per: Per): A = radioactivityPer(YoctoBecquerel)
  def zBq(per: Per): A = radioactivityPer(ZeptoBecquerel)
  def aBq(per: Per): A = radioactivityPer(AttoBecquerel)
  def fBq(per: Per): A = radioactivityPer(FemtoBecquerel)
  def pBq(per: Per): A = radioactivityPer(PicoBecquerel)
  def nBq(per: Per): A = radioactivityPer(NanoBecquerel)
  def microBecquerel(per: Per): A = radioactivityPer(MicroBecquerel)
  def microBq(per: Per): A = radioactivityPer(MicroBecquerel)
  def μBq(per: Per): A = radioactivityPer(MicroBecquerel)
  def mBq(per: Per): A = radioactivityPer(MilliBecquerel)
  def cBq(per: Per): A = radioactivityPer(CentiBecquerel)
  def dBq(per: Per): A = radioactivityPer(DeciBecquerel)
  def Bq(per: Per): A = radioactivityPer(Becquerel)
  def daBq(per: Per): A = radioactivityPer(DecaBecquerel)
  def hBq(per: Per): A = radioactivityPer(HectoBecquerel)
  def kBq(per: Per): A = radioactivityPer(KiloBecquerel)
  def MBq(per: Per): A = radioactivityPer(MegaBecquerel)
  def GBq(per: Per): A = radioactivityPer(GigaBecquerel)
  def TBq(per: Per): A = radioactivityPer(TeraBecquerel)
  def PBq(per: Per): A = radioactivityPer(PetaBecquerel)
  def EBq(per: Per): A = radioactivityPer(ExaBecquerel)
  def ZBq(per: Per): A = radioactivityPer(ZettaBecquerel)
  def YBq(per: Per): A = radioactivityPer(YottaBecquerel)
  def yCi(per: Per): A = radioactivityPer(YoctoCurie)
  def zCi(per: Per): A = radioactivityPer(ZeptoCurie)
  def aCi(per: Per): A = radioactivityPer(AttoCurie)
  def fCi(per: Per): A = radioactivityPer(FemtoCurie)
  def pCi(per: Per): A = radioactivityPer(PicoCurie)
  def nCi(per: Per): A = radioactivityPer(NanoCurie)
  def microCurie(per: Per): A = radioactivityPer(MicroCurie)
  def microCi(per: Per): A = radioactivityPer(MicroCurie)
  def μCi(per: Per): A = radioactivityPer(MicroCurie)
  def mCi(per: Per): A = radioactivityPer(MilliCurie)
  def cCi(per: Per): A = radioactivityPer(CentiCurie)
  def dCi(per: Per): A = radioactivityPer(DeciCurie)
  def Ci(per: Per): A = radioactivityPer(Curie)
  def daCi(per: Per): A = radioactivityPer(DecaCurie)
  def hCi(per: Per): A = radioactivityPer(HectoCurie)
  def kCi(per: Per): A = radioactivityPer(KiloCurie)
  def MCi(per: Per): A = radioactivityPer(MegaCurie)
  def GCi(per: Per): A = radioactivityPer(GigaCurie)
  def TCi(per: Per): A = radioactivityPer(TeraCurie)
  def PCi(per: Per): A = radioactivityPer(PetaCurie)
  def ECi(per: Per): A = radioactivityPer(ExaCurie)
  def ZCi(per: Per): A = radioactivityPer(ZettaCurie)
  def YCi(per: Per): A = radioactivityPer(YottaCurie)
  def Rd(per: Per): A = radioactivityPer(Rutherford)
}

trait PredefinedRadioactivityUnit extends RadioactivityPostfixOps[RadioactivityUnit]{
  override protected def radioactivityPostfixOps(unit: RadioactivityUnit) = unit
  
}

object PredefinedRadioactivityUnit extends PredefinedRadioactivityUnit
