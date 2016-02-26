package org.waman.multiverse.time

import org.waman.multiverse.{ConstantsDefined, PhysicalUnit, Quantity, UnitConverter}
import spire.implicits._
import spire.math.{Fractional, Real}

trait TimePostfixOps[A]{
  import TimeUnit._

  protected def timePostfixOps(timeUnit: TimeUnit): A

  def ys: A = timePostfixOps(YoctoSecond)
  def zs: A = timePostfixOps(ZeptoSecond)
  def as: A = timePostfixOps(AttoSecond)
  def fs: A = timePostfixOps(FemtoSecond)
  def ps: A = timePostfixOps(PicoSecond)
  def ns: A = timePostfixOps(NanoSecond)
  def μs: A = timePostfixOps(MicroSecond)
  def ms: A = timePostfixOps(MilliSecond)
  def cs: A = timePostfixOps(CentiSecond)
  def ds: A = timePostfixOps(DeciSecond)
  def s : A = timePostfixOps(Second)
  def das: A = timePostfixOps(DecaSecond)
  def hs: A = timePostfixOps(HectoSecond)
  def ks: A = timePostfixOps(KiloSecond)
  def Ms: A = timePostfixOps(MegaSecond)
  def Gs: A = timePostfixOps(GigaSecond)
  def Ts: A = timePostfixOps(TeraSecond)
  def Ps: A = timePostfixOps(PetaSecond)
  def Es: A = timePostfixOps(ExaSecond)
  def Zs: A = timePostfixOps(ZettaSecond)
  def Ys: A = timePostfixOps(YottaSecond)

  def minute: A = timePostfixOps(Minute)
  def h     : A = timePostfixOps(Hour)
  def d     : A = timePostfixOps(Day)
}

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends Quantity[A, TimeUnit]
    with TimePostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TimeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSecond) / real(evalUnit.unitInSecond)

  override protected def timePostfixOps(timeUnit: TimeUnit) = apply(timeUnit)
}

sealed abstract class TimeUnit(val symbol: String, val unitInSecond: Real)
  extends PhysicalUnit[TimeUnit]{

  def this(symbol: String, factor: Real, timeUnit: TimeUnit) =
    this(symbol, factor * timeUnit.unitInSecond)

  override def baseUnit = TimeUnit.Second
  override def valueInBaseUnit = unitInSecond
}

object TimeUnit extends ConstantsDefined[TimeUnit]{

  case object YoctoSecond extends TimeUnit("ys", r"1e-24")
  case object ZeptoSecond extends TimeUnit("zs", r"1e-21")
  case object AttoSecond  extends TimeUnit("as", r"1e-18")
  case object FemtoSecond extends TimeUnit("fs", r"1e-15")
  case object PicoSecond  extends TimeUnit("ps", r"1e-12")
  case object NanoSecond  extends TimeUnit("ns", r"1e-9")
  case object MicroSecond extends TimeUnit("μs", r"1e-6")
  case object MilliSecond extends TimeUnit("ms", r"1e-3")
  case object CentiSecond extends TimeUnit("cs", r"1e-2")
  case object DeciSecond  extends TimeUnit("ds", r"1e-1")
  case object Second      extends TimeUnit("s" , r"1")
  case object DecaSecond  extends TimeUnit("das", r"1e1")
  case object HectoSecond extends TimeUnit("hs", r"1e2")
  case object KiloSecond  extends TimeUnit("ks", r"1e3")
  case object MegaSecond  extends TimeUnit("Ms", r"1e6")
  case object GigaSecond  extends TimeUnit("Gs", r"1e9")
  case object TeraSecond  extends TimeUnit("Ts", r"1e12")
  case object PetaSecond  extends TimeUnit("Ps", r"1e15")
  case object ExaSecond   extends TimeUnit("Es", r"1e18")
  case object ZettaSecond extends TimeUnit("Zs", r"1e21")
  case object YottaSecond extends TimeUnit("Ys", r"1e24")

  case object Minute      extends TimeUnit("minute", r"60")
  case object Hour        extends TimeUnit("h"     , r"60", Minute)
  case object Day         extends TimeUnit("d"     , r"24", Hour)

  override lazy val values = Seq(
    YoctoSecond,
    ZeptoSecond,
    AttoSecond,
    FemtoSecond,
    PicoSecond,
    NanoSecond,
    MicroSecond,
    MilliSecond,
    CentiSecond,
    DeciSecond,
    Second,
    DecaSecond,
    HectoSecond,
    KiloSecond,
    MegaSecond,
    GigaSecond,
    TeraSecond,
    PetaSecond,
    ExaSecond,
    ZettaSecond,
    YottaSecond,

    Minute,
    Hour,
    Day
  )
}

trait PredefinedTimeUnit extends TimePostfixOps[TimeUnit]{
  override protected def timePostfixOps(timeUnit: TimeUnit) = timeUnit
}

object PredefinedTimeUnit extends PredefinedTimeUnit

trait TimeFactory[A] extends TimePostfixOps[Time[A]]{

  def apply(unit: TimeUnit): Time[A]

  override protected def timePostfixOps(timeUnit: TimeUnit) = apply(timeUnit)
}