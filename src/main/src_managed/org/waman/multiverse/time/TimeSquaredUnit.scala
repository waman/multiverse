package org.waman.multiverse.time

import org.waman.multiverse._
import spire.implicits._
import spire.math.Real


sealed trait TimeSquaredUnit extends PhysicalUnit[TimeSquaredUnit]{

  def unitInSecondSquared: Real

  override def baseUnit = org.waman.multiverse.time.TimeSquaredUnit.SecondSquared
  override def valueInBaseUnit = unitInSecondSquared
}

object TimeSquaredUnit extends ConstantsDefined[TimeSquaredUnit]{

  // intrinsic
  private[TimeSquaredUnit]
  class IntrinsicTimeSquaredUnit(name: String, val symbols: Seq[String], val unitInSecondSquared: Real)
      extends TimeSquaredUnit{

    def this(name: String, symbols: Seq[String], unit: TimeSquaredUnit) =
      this(name, symbols, unit.unitInSecondSquared)

    def this(name: String, symbols: Seq[String], factor: Real, unit: TimeSquaredUnit) =
      this(name, symbols, factor * unit.unitInSecondSquared)
  }

  case object SecondSquared extends IntrinsicTimeSquaredUnit("SecondSquared", Seq("s2"), 1)
    

  override lazy val values = Seq(SecondSquared)

  // TimeUnit * TimeUnit -> TimeSquared
  private[TimeSquaredUnit]
  class TimeDotTimeUnit(val firstUnit: TimeUnit, val secondUnit: TimeUnit)
      extends TimeSquaredUnit with ProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit]{

    override lazy val unitInSecondSquared: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: TimeUnit, unit2: TimeUnit): TimeSquaredUnit =
    new TimeDotTimeUnit(unit1, unit2)
}

trait TimeSquaredPostfixOps[A]{
  import TimeSquaredUnit._

  protected def timeSquaredPostfixOps(unit: TimeSquaredUnit): A

  def s2 : A = timeSquaredPostfixOps(SecondSquared)
}

trait TimeSquaredDot[A]{
  import TimeSquaredUnit._

  protected def timeSquaredDot(unit: TimeSquaredUnit): A

  def s2(dot: Dot): A = timeSquaredDot(SecondSquared)
}

trait TimeSquaredPer[A]{
  import TimeSquaredUnit._

  protected def timeSquaredPer(unit: TimeSquaredUnit): A

  def s2(per: Per): A = timeSquaredPer(SecondSquared)
}

trait PredefinedTimeSquaredUnit extends TimeSquaredPostfixOps[TimeSquaredUnit]{
  override protected def timeSquaredPostfixOps(unit: TimeSquaredUnit) = unit
  
}

object PredefinedTimeSquaredUnit extends PredefinedTimeSquaredUnit
