package org.waman.multiverse.energy

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait PowerPostfixOps[A]{

  import PowerUnit._

  protected def powerPostfixOps(powerUnit: PowerUnit): A

  def W: A = powerPostfixOps(Watt)
}

class Power[A: Fractional](val value: A, val unit: PowerUnit)
  extends Quantity[A, PowerUnit]
    with PowerPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: PowerUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInWatt) / real(evalUnit.unitInWatt)


  override protected def powerPostfixOps(powerUnit: PowerUnit) = apply(powerUnit)
}

sealed abstract class PowerUnit(val symbols: Seq[String], val unitInWatt: Real)
  extends PhysicalUnit[PowerUnit]{

  override def baseUnit = PowerUnit.Watt
  override def valueInBaseUnit = unitInWatt
}

object PowerUnit extends ConstantsDefined[PowerUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)

  case object Watt extends PowerUnit("W", 1)

  override lazy val values = Seq(
    Watt
  )
}

trait PredefinedPowerUnit extends PowerPostfixOps[PowerUnit]{

  override protected def powerPostfixOps(powerUnit: PowerUnit) = powerUnit
}

object PredefinedPowerUnit extends PredefinedPowerUnit

trait PowerFactory[A]
  extends PowerPostfixOps[Power[A]]{

  def apply(unit: PowerUnit): Power[A]

  override protected def powerPostfixOps(powerUnit: PowerUnit) = apply(powerUnit)
}