package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

trait VelocityPostfixOps[A]{
  def `m/s` : A
  def `km/h`: A
}

abstract class Velocity[A: Fractional]
    extends VelocityPostfixOps[A]  // for style like "velocity.`m/s`" and "velocity `m/s`"
    with LengthPostfixOps[DivisibleByTime[A]]  // for style like "velocity.m/s" ( = "velocity.m./(s)")
    with LengthPer[TimePostfixOps[A]]  // for style like "velocity m/s" ( = "velocity.m(/).s")
    with UnitConverter[A]{

  val algebra = implicitly[Fractional[A]]

  def `m/s` : A
  def `km/h`: A = div(`m/s`, VelocityUnit.KilometrePerHour.inMetrePerSecond)

  // for style like "velocity (m/s)" ( = "velocity.apply(m/s)")
  def apply(unit: VelocityUnit): A = unit match {
    case u: PredefinedVelocityUnit => u.accept(this)
    case _ => div(`m/s`, unit.inMetrePerSecond)
  }

  // for style like "velocity.m/s"
  private def callMetre(metre: A) = new DivisibleByTime[A]{
    override def /(timeUnit: TimeUnit): A = times(metre, timeUnit.inSecond)
  }

  private def callMetre(metre: A, inMetre: Real): DivisibleByTime[A] = callMetre(div(metre, inMetre))

  override def nm = callMetre(`m/s`, LengthUnit.Nanometre.inMetre)
  override def µm = callMetre(`m/s`, LengthUnit.Micrometre.inMetre)
  override def mm = callMetre(`m/s`, LengthUnit.Millimetre.inMetre)
  override def cm = callMetre(`m/s`, LengthUnit.Centimetre.inMetre)
  override def m  = callMetre(`m/s`)
  override def km = callMetre(`m/s`, LengthUnit.Kilometre.inMetre)
  override def Mm = callMetre(`m/s`, LengthUnit.Megametre.inMetre)
  override def Gm = callMetre(`m/s`, LengthUnit.Gigametre.inMetre)
  override def Tm = callMetre(`m/s`, LengthUnit.Terametre.inMetre)

  // astronomy
  override def au = callMetre(`m/s`, LengthUnit.AstronomicalUnit.inMetre)
  override def ly = callMetre(`m/s`, LengthUnit.LightYear.inMetre)
  override def pc = callMetre(`m/s`, LengthUnit.Parsec.inMetre)

  // yard-pond
  override def in = callMetre(`m/s`, LengthUnit.Inch.inMetre)
  override def ft = callMetre(`m/s`, LengthUnit.Feet.inMetre)
  override def yd = callMetre(`m/s`, LengthUnit.Yard.inMetre)
  override def mi = callMetre(`m/s`, LengthUnit.Mile.inMetre)

  // for style like "velocity m/s"
  override def nm(per: Per): TimePostfixOps[A] = ???
  override def µm(per: Per): TimePostfixOps[A] = ???
  override def mm(per: Per): TimePostfixOps[A] = ???
  override def cm(per: Per): TimePostfixOps[A] = ???
  override def m (per: Per): TimePostfixOps[A] = ???
  override def km(per: Per): TimePostfixOps[A] = ???
  override def Mm(per: Per): TimePostfixOps[A] = ???
  override def Gm(per: Per): TimePostfixOps[A] = ???
  override def Tm(per: Per): TimePostfixOps[A] = ???

  // astronomy
  override def au(per: Per): TimePostfixOps[A] = ???
  override def ly(per: Per): TimePostfixOps[A] = ???
  override def pc(per: Per): TimePostfixOps[A] = ???

  // yard-pond
  override def in(per: Per): TimePostfixOps[A] = ???
  override def ft(per: Per): TimePostfixOps[A] = ???
  override def yd(per: Per): TimePostfixOps[A] = ???
  override def mi(per: Per): TimePostfixOps[A] = ???
}

case class MetrePerSecondVelocity[A: Fractional](value: A) extends Velocity[A]{
  override def `m/s`: A = value
}

trait VelocityUnit{
  val inMetrePerSecond: Real
}

trait PredefinedVelocityUnit extends VelocityUnit{
  def accept[A](v: Velocity[A]): A
  def accept[A](ui: VelocityUnitInterpreter[A]): Velocity[A]
}

object VelocityUnit{

  case object MetrePerSecond extends PredefinedVelocityUnit{
    override val inMetrePerSecond: Real = r"1"
    override def accept[A](v: Velocity[A]): A = v.`m/s`
    override def accept[A](ui: VelocityUnitInterpreter[A]): Velocity[A] = ui.`m/s`
  }

  case object KilometrePerHour extends PredefinedVelocityUnit{
    override val inMetrePerSecond: Real = r"1000" / r"3600"
    override def accept[A](v: Velocity[A]): A = v.`km/h`
    override def accept[A](ui: VelocityUnitInterpreter[A]): Velocity[A] = ui.`km/h`
  }

  def apply(value: Real): VelocityUnit = new VelocityUnit{
    override val inMetrePerSecond: Real = value
  }
}

trait VelocityUnitInterpreter[A]
    extends VelocityPostfixOps[Velocity[A]]
    with UnitConverter[A]{

  val value: A

  def apply(unit: VelocityUnit): Velocity[A] = unit match {
    case u: PredefinedVelocityUnit => u.accept(this)
    case _ => newVelocity(value, unit)
  }

  protected def newVelocity(value: A, unit: VelocityUnit): Velocity[A]
}
