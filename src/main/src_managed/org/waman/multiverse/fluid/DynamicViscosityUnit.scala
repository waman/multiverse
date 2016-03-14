package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.time.TimeUnit
import spire.implicits._
import spire.math.Real

sealed trait DynamicViscosityUnit extends PhysicalUnit[DynamicViscosityUnit]{

  def unitInPascalSecond: Real

  override def baseUnit = PressureUnit.Pascal * TimeUnit.Second
  override def valueInBaseUnit = unitInPascalSecond
}

object DynamicViscosityUnit extends ConstantsDefined[DynamicViscosityUnit]{

  // intrinsic
  private[DynamicViscosityUnit]
  class IntrinsicDynamicViscosityUnit(name: String, val symbols: Seq[String], val unitInPascalSecond: Real)
      extends DynamicViscosityUnit{

    def this(name: String, symbols: Seq[String], unit: DynamicViscosityUnit) =
      this(name, symbols, unit.unitInPascalSecond)

    def this(name: String, symbols: Seq[String], factor: Real, unit: DynamicViscosityUnit) =
      this(name, symbols, factor * unit.unitInPascalSecond)
  }

  case object Poise extends IntrinsicDynamicViscosityUnit("Poise", Seq("P"), r"0.1")
    

  override lazy val values = Seq(Poise)

  // PressureUnit * TimeUnit -> DynamicViscosity
  private[DynamicViscosityUnit]
  class PressureDotTimeUnit(val firstUnit: PressureUnit, val secondUnit: TimeUnit)
      extends DynamicViscosityUnit with ProductUnit[DynamicViscosityUnit, PressureUnit, TimeUnit]{

    override lazy val unitInPascalSecond: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: PressureUnit, unit2: TimeUnit): DynamicViscosityUnit =
    new PressureDotTimeUnit(unit1, unit2)
}

trait DynamicViscosityPostfixOps[A]{
  import DynamicViscosityUnit._

  protected def dynamicViscosityPostfixOps(unit: DynamicViscosityUnit): A

  def P : A = dynamicViscosityPostfixOps(Poise)
}

trait DynamicViscosityDot[A]{
  import DynamicViscosityUnit._

  protected def dynamicViscosityDot(unit: DynamicViscosityUnit): A

  def P(dot: Dot): A = dynamicViscosityDot(Poise)
}

trait DynamicViscosityPer[A]{
  import DynamicViscosityUnit._

  protected def dynamicViscosityPer(unit: DynamicViscosityUnit): A

  def P(per: Per): A = dynamicViscosityPer(Poise)
}

trait PredefinedDynamicViscosityUnit extends DynamicViscosityPostfixOps[DynamicViscosityUnit]{
  override protected def dynamicViscosityPostfixOps(unit: DynamicViscosityUnit) = unit
  
}

object PredefinedDynamicViscosityUnit extends PredefinedDynamicViscosityUnit
