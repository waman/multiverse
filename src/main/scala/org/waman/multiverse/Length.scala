package org.waman.multiverse

trait Length{
  def mm: Double = m * 1000.0
  def cm: Double = m * 100.0
  def m : Double
  def km: Double = m / 1000.0

  def /(timeUnit: TimeUnit): Velocity = {
    val timeInSecond: Double = timeUnit match{
      case _ if timeUnit == TimeUnit.ms => 0.001
      case _ if timeUnit == TimeUnit.s  => 1
      case _ if timeUnit == TimeUnit.min  => 60
      case _ if timeUnit == TimeUnit.h    => 3600
    }
    new UnitInterpreter(m / timeInSecond).`m/s`
  }
}

sealed abstract class LengthUnit(code: String)

object LengthUnit{
  case object mm extends LengthUnit("millimetre")
  case object cm extends LengthUnit("centimetre")
  case object m  extends LengthUnit("metre")
  case object km extends LengthUnit("kilometre")
}

trait LengthUnitInterpreter {

  val value: Double

  def mm: Length = MillimetreLength(value)
  def cm: Length = CentimetreLength(value)
  def m : Length = MetreLength(value)
  def km: Length = KilometreLength(value)

  def m(per: Per): MetrePer = new MetrePer(value)

  def apply(unit: LengthUnit): Length = unit match {
    case _ if unit == LengthUnit.mm => mm
    case _ if unit == LengthUnit.cm => cm
    case _ if unit == LengthUnit.m => m
    case _ if unit == LengthUnit.km => km
  }

  case class MillimetreLength(value: Double) extends Length {
    override def mm: Double = value
    override def m: Double = value / 1000.0
  }

  case class CentimetreLength(value: Double) extends Length {
    override def cm: Double = value
    override def m: Double = value / 100.0
  }

  case class MetreLength(value: Double) extends Length {
    override def m: Double = value
  }

  case class KilometreLength(value: Double) extends Length {
    override def m: Double = value * 1000
    override def km: Double = value
  }
}