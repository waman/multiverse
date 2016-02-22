package org.waman.multiverse.luminous

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait IlluminancePostfixOps[A]{

  import IlluminanceUnit._

  protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit): A

  def lx: A = illuminancePostfixOps(Lux)
}

class Illuminance[A: Fractional](val value: A, val unit: IlluminanceUnit)
  extends Quantity[A, IlluminanceUnit]
    with IlluminancePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: IlluminanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInLux) / real(evalUnit.unitInLux)

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) = apply(illuminanceUnit)
}

sealed abstract class IlluminanceUnit(val symbol: String, val unitInLux: Real)
    extends PhysicalUnit[IlluminanceUnit]{

  override val baseUnit = IlluminanceUnit.Lux
  override val inBaseUnitAccessor = () => unitInLux
}

object IlluminanceUnit{

  case object Lux extends IlluminanceUnit("lx", 1)
}

trait PredefinedIlluminanceUnit extends IlluminancePostfixOps[IlluminanceUnit]{

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) = illuminanceUnit
}

object PredefinedIlluminanceUnit extends PredefinedIlluminanceUnit

trait IlluminanceUnitInterpreter[A]
    extends IlluminancePostfixOps[Illuminance[A]]{

  def apply(unit: IlluminanceUnit): Illuminance[A]

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) =
    apply(illuminanceUnit)
}