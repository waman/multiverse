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

sealed abstract class PowerUnit(val symbol: String, val unitInWatt: Real)
  extends PhysicalUnit[PowerUnit]{

  def this(symbol: String, factor: Real, powerUnit: PowerUnit) =
    this(symbol, factor * powerUnit.unitInWatt)

  override val baseUnit = PowerUnit.Watt
  override val inBaseUnitAccessor = () => unitInWatt
}

object PowerUnit{

  case object Watt extends PowerUnit("W", 1)
}

trait PredefinedPowerUnit extends PowerPostfixOps[PowerUnit]{

  override protected def powerPostfixOps(powerUnit: PowerUnit) = powerUnit
}

object PredefinedPowerUnit extends PredefinedPowerUnit

trait PowerUnitInterpreter[A]
  extends PowerPostfixOps[Power[A]]{

  def apply(unit: PowerUnit): Power[A]

  override protected def powerPostfixOps(powerUnit: PowerUnit) = apply(powerUnit)
}