package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait ResistancePostfixOps[A]{

  import ResistanceUnit._

  protected def resistancePostfixOps(resistanceUnit: ResistanceUnit): A

  def Ω: A = resistancePostfixOps(Ohm)
}

class Resistance[A: Fractional](val value: A, val unit: ResistanceUnit)
  extends Quantity[A, ResistanceUnit]
    with ResistancePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ResistanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInOhm) / real(evalUnit.unitInOhm)

  override protected def resistancePostfixOps(resistanceUnit: ResistanceUnit) = apply(resistanceUnit)
}

sealed abstract class ResistanceUnit(val symbol: String, val unitInOhm: Real)
    extends PhysicalUnit[ResistanceUnit]{

  override def baseUnit = ResistanceUnit.Ohm
  override def valueInBaseUnit = unitInOhm
}

object ResistanceUnit extends ConstantsDefined[ResistanceUnit]{

  case object Ohm extends ResistanceUnit("Ω", 1)

  override lazy val values = Seq(
    Ohm
  )
}

trait PredefinedResistanceUnit extends ResistancePostfixOps[ResistanceUnit]{

  override protected def resistancePostfixOps(resistanceUnit: ResistanceUnit) = resistanceUnit
}

object PredefinedResistanceUnit extends PredefinedResistanceUnit

trait ResistanceFactory[A]
    extends ResistancePostfixOps[Resistance[A]]{

  def apply(unit: ResistanceUnit): Resistance[A]

  override protected def resistancePostfixOps(resistanceUnit: ResistanceUnit) =
    apply(resistanceUnit)
}