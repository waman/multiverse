package org.waman.multiverse.radiation

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait RadioactivityPostfixOps[A]{

  import RadioactivityUnit._

  protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit): A

  def Bq: A = radioactivityPostfixOps(Becquerel)
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

sealed abstract class RadioactivityUnit(val symbol: String, val unitInBecquerel: Real)
    extends PhysicalUnit[RadioactivityUnit]{

  override val baseUnit = RadioactivityUnit.Becquerel
  override val inBaseUnitAccessor = () => unitInBecquerel
}

object RadioactivityUnit{

  case object Becquerel extends RadioactivityUnit("Bq", 1)
}

trait PredefinedRadioactivityUnit extends RadioactivityPostfixOps[RadioactivityUnit]{

  override protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit) = radioactivityUnit
}

object PredefinedRadioactivityUnit extends PredefinedRadioactivityUnit

trait RadioactivityUnitInterpreter[A]
    extends RadioactivityPostfixOps[Radioactivity[A]]{

  def apply(unit: RadioactivityUnit): Radioactivity[A]

  override protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit) =
    apply(radioactivityUnit)
}