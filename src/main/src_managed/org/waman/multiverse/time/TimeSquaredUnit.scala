package org.waman.multiverse.time

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._


sealed trait TimeSquaredUnit extends PhysicalUnit[TimeSquaredUnit]{

  override def getSIUnit = org.waman.multiverse.time.TimeSquaredUnit.SecondSquared
}

object TimeSquaredUnit extends ConstantsDefined[TimeSquaredUnit]{

  // intrinsic
  private[TimeSquaredUnit]
  class IntrinsicTimeSquaredUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends TimeSquaredUnit{

    def this(name: String, symbols: Seq[String], unit: TimeSquaredUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: TimeSquaredUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object SecondSquared extends IntrinsicTimeSquaredUnit("SecondSquared", Seq("s2"), 1)

  override lazy val values = Seq(SecondSquared)

  // TimeUnit * TimeUnit -> TimeSquared
  private[TimeSquaredUnit]
  class ProductTimeDotTimeUnit(val firstUnit: TimeUnit, val secondUnit: TimeUnit)
      extends TimeSquaredUnit with ProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: TimeUnit, unit2: TimeUnit): TimeSquaredUnit =
    new ProductTimeDotTimeUnit(unit1, unit2)
}

trait MultiplicativeByTimeSquaredUnit[R]{
  def *(unit: TimeSquaredUnit): R
}

trait DivisibleByTimeSquaredUnit[R]{
  def /(unit: TimeSquaredUnit): R
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
