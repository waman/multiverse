package org.waman.multiverse.thermal

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.angle.DegreePostfixOps

trait MultiplicativeByTemperatureUnit[R]{
  def *(unit: TemperatureUnit): R
}

trait DivisibleByTemperatureUnit[R]{
  def /(unit: TemperatureUnit): R
}

trait TemperaturePostfixOps[A]{
  import TemperatureUnit._

  protected def temperaturePostfixOps(unit: TemperatureUnit): A

  def yK : A = temperaturePostfixOps(YoctoKelvin)
  def zK : A = temperaturePostfixOps(ZeptoKelvin)
  def aK : A = temperaturePostfixOps(AttoKelvin)
  def fK : A = temperaturePostfixOps(FemtoKelvin)
  def pK : A = temperaturePostfixOps(PicoKelvin)
  def nK : A = temperaturePostfixOps(NanoKelvin)
  def microKelvin : A = temperaturePostfixOps(MicroKelvin)
  def microK : A = temperaturePostfixOps(MicroKelvin)
  def μK : A = temperaturePostfixOps(MicroKelvin)
  def mK : A = temperaturePostfixOps(MilliKelvin)
  def cK : A = temperaturePostfixOps(CentiKelvin)
  def dK : A = temperaturePostfixOps(DeciKelvin)
  def K : A = temperaturePostfixOps(Kelvin)
  def daK : A = temperaturePostfixOps(DecaKelvin)
  def hK : A = temperaturePostfixOps(HectoKelvin)
  def kK : A = temperaturePostfixOps(KiloKelvin)
  def MK : A = temperaturePostfixOps(MegaKelvin)
  def GK : A = temperaturePostfixOps(GigaKelvin)
  def TK : A = temperaturePostfixOps(TeraKelvin)
  def PK : A = temperaturePostfixOps(PetaKelvin)
  def EK : A = temperaturePostfixOps(ExaKelvin)
  def ZK : A = temperaturePostfixOps(ZettaKelvin)
  def YK : A = temperaturePostfixOps(YottaKelvin)
  def degC : A = temperaturePostfixOps(DegreeCelsius)
  def ℃ : A = temperaturePostfixOps(DegreeCelsius)
  def degF : A = temperaturePostfixOps(DegreeFahrenheit)
  def ℉ : A = temperaturePostfixOps(DegreeFahrenheit)
  def degDe : A = temperaturePostfixOps(DegreeDelisle)
  def degN : A = temperaturePostfixOps(DegreeNewton)
  def degR : A = temperaturePostfixOps(DegreeRankine)
  def degRe : A = temperaturePostfixOps(DegreeReaumur)
  def degRo : A = temperaturePostfixOps(DegreeRomer)
  def GM : A = temperaturePostfixOps(ReguloGasMark)
}

trait TemperatureDot[A]{
  import TemperatureUnit._

  protected def temperatureDot(unit: TemperatureUnit): A

  def yK(dot: Dot): A = temperatureDot(YoctoKelvin)
  def zK(dot: Dot): A = temperatureDot(ZeptoKelvin)
  def aK(dot: Dot): A = temperatureDot(AttoKelvin)
  def fK(dot: Dot): A = temperatureDot(FemtoKelvin)
  def pK(dot: Dot): A = temperatureDot(PicoKelvin)
  def nK(dot: Dot): A = temperatureDot(NanoKelvin)
  def microKelvin(dot: Dot): A = temperatureDot(MicroKelvin)
  def microK(dot: Dot): A = temperatureDot(MicroKelvin)
  def μK(dot: Dot): A = temperatureDot(MicroKelvin)
  def mK(dot: Dot): A = temperatureDot(MilliKelvin)
  def cK(dot: Dot): A = temperatureDot(CentiKelvin)
  def dK(dot: Dot): A = temperatureDot(DeciKelvin)
  def K(dot: Dot): A = temperatureDot(Kelvin)
  def daK(dot: Dot): A = temperatureDot(DecaKelvin)
  def hK(dot: Dot): A = temperatureDot(HectoKelvin)
  def kK(dot: Dot): A = temperatureDot(KiloKelvin)
  def MK(dot: Dot): A = temperatureDot(MegaKelvin)
  def GK(dot: Dot): A = temperatureDot(GigaKelvin)
  def TK(dot: Dot): A = temperatureDot(TeraKelvin)
  def PK(dot: Dot): A = temperatureDot(PetaKelvin)
  def EK(dot: Dot): A = temperatureDot(ExaKelvin)
  def ZK(dot: Dot): A = temperatureDot(ZettaKelvin)
  def YK(dot: Dot): A = temperatureDot(YottaKelvin)
  def degC(dot: Dot): A = temperatureDot(DegreeCelsius)
  def ℃(dot: Dot): A = temperatureDot(DegreeCelsius)
  def degF(dot: Dot): A = temperatureDot(DegreeFahrenheit)
  def ℉(dot: Dot): A = temperatureDot(DegreeFahrenheit)
  def degDe(dot: Dot): A = temperatureDot(DegreeDelisle)
  def degN(dot: Dot): A = temperatureDot(DegreeNewton)
  def degR(dot: Dot): A = temperatureDot(DegreeRankine)
  def degRe(dot: Dot): A = temperatureDot(DegreeReaumur)
  def degRo(dot: Dot): A = temperatureDot(DegreeRomer)
  def GM(dot: Dot): A = temperatureDot(ReguloGasMark)
}

trait TemperaturePer[A]{
  import TemperatureUnit._

  protected def temperaturePer(unit: TemperatureUnit): A

  def yK(per: Per): A = temperaturePer(YoctoKelvin)
  def zK(per: Per): A = temperaturePer(ZeptoKelvin)
  def aK(per: Per): A = temperaturePer(AttoKelvin)
  def fK(per: Per): A = temperaturePer(FemtoKelvin)
  def pK(per: Per): A = temperaturePer(PicoKelvin)
  def nK(per: Per): A = temperaturePer(NanoKelvin)
  def microKelvin(per: Per): A = temperaturePer(MicroKelvin)
  def microK(per: Per): A = temperaturePer(MicroKelvin)
  def μK(per: Per): A = temperaturePer(MicroKelvin)
  def mK(per: Per): A = temperaturePer(MilliKelvin)
  def cK(per: Per): A = temperaturePer(CentiKelvin)
  def dK(per: Per): A = temperaturePer(DeciKelvin)
  def K(per: Per): A = temperaturePer(Kelvin)
  def daK(per: Per): A = temperaturePer(DecaKelvin)
  def hK(per: Per): A = temperaturePer(HectoKelvin)
  def kK(per: Per): A = temperaturePer(KiloKelvin)
  def MK(per: Per): A = temperaturePer(MegaKelvin)
  def GK(per: Per): A = temperaturePer(GigaKelvin)
  def TK(per: Per): A = temperaturePer(TeraKelvin)
  def PK(per: Per): A = temperaturePer(PetaKelvin)
  def EK(per: Per): A = temperaturePer(ExaKelvin)
  def ZK(per: Per): A = temperaturePer(ZettaKelvin)
  def YK(per: Per): A = temperaturePer(YottaKelvin)
  def degC(per: Per): A = temperaturePer(DegreeCelsius)
  def ℃(per: Per): A = temperaturePer(DegreeCelsius)
  def degF(per: Per): A = temperaturePer(DegreeFahrenheit)
  def ℉(per: Per): A = temperaturePer(DegreeFahrenheit)
  def degDe(per: Per): A = temperaturePer(DegreeDelisle)
  def degN(per: Per): A = temperaturePer(DegreeNewton)
  def degR(per: Per): A = temperaturePer(DegreeRankine)
  def degRe(per: Per): A = temperaturePer(DegreeReaumur)
  def degRo(per: Per): A = temperaturePer(DegreeRomer)
  def GM(per: Per): A = temperaturePer(ReguloGasMark)
}

trait PredefinedTemperatureUnit extends TemperaturePostfixOps[TemperatureUnit]{
  override protected def temperaturePostfixOps(unit: TemperatureUnit) = unit
  
}

object PredefinedTemperatureUnit extends PredefinedTemperatureUnit
