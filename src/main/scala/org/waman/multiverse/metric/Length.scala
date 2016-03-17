package org.waman.multiverse.metric

import org.waman.multiverse._
import org.waman.multiverse.mechanics.{Acceleration, AccelerationUnit, Velocity, VelocityUnit}
import org.waman.multiverse.time._
import spire.implicits._
import spire.math.Fractional

/*
 * +++++ For Length +++++
 * 1.0.m --- LengthUnitInterpreter.m
 * 1.0 m --- LengthUnitInterpreter.m
 * 1.0 (m) ( = 1.0.apply(m)) --- LengthUnitInterpreter.apply(LengthUnit)
 *
 * length.m --- Length.m
 * length m --- Length.m
 * length (m) ( = length.apply(m)) --- Length.apply(LengthUnit)
 *
 * +++++ For Velocity +++++
 * 1.0.m/s --- Length#/(TimeUnit)
 * 1.0 m/s ( = 1.0.m(/).s) --- LengthUnitInterpreter.m(Per): TimePostfixOps, TimePostfixOps.s
 * 1.0 (m/s) ( = 1.0.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, LengthUnitInterpreter.apply(VelocityUnit)
 *
 * velocity.m/s ( = velocity.m./(s)) --- Velocity.m: DivisibleByTime, DivisibleByTime./(s)
 * velocity m/s ( = velocity.m(/).s) --- Velocity.m(Per): TimePostfixOps, TimePostfixOps.s
 * velocity (m/s) ( = velocity.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, Velocity.apply(VelocityUnit)
 */

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends Quantity[A, LengthUnit]
    with LengthPostfixOps[A]
    with MultiplicativeByLengthUnit[Area[A]]
    with DivisibleByTimeUnit[Velocity[A]]
    with DivisibleByTimeSquaredUnit[Acceleration[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  /**
    *  For style like <code>length.m</code> and <code>length m</code>
    *  where <code>length</code> is a Length object
    */
  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(lengthUnit)

  /** For style like <code>length (m)</code> where <code>length</code> is a Length object*/
  def apply(evalUnit: LengthUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetre) / real(evalUnit.unitInMetre)

  override def *(lengthUnit: LengthUnit): Area[A] = new Area(value, unit * lengthUnit)

  /** For style like <code>1.0.m/s</code> */
  override def /(timeUnit: TimeUnit): Velocity[A] = new Velocity(value, unit / timeUnit)

  override def /(timeSquaredUnit: TimeSquaredUnit): Acceleration[A] =
    new Acceleration[A](value, unit / timeSquaredUnit)
}

trait LengthFactory[A]
    extends LengthPostfixOps[Length[A]]
    with LengthDot[LengthPostfixOps[Area[A]]]
    with LengthPer[TimePostfixOps[Velocity[A]]  // for style like "1.0 m/s" ( = 1.0.m(/).s)
    with TimeSquaredPostfixOps[Acceleration[A]]]{

  // for style like "1.0 (m)" ( = "1.0.apply(m)")
  def apply(unit: LengthUnit): Length[A]

  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(lengthUnit)

  // Length * Length -> Area
  def apply(areaUnit: AreaUnit): Area[A]

  override protected def lengthDot(lengthUnit1: LengthUnit) = new LengthPostfixOps[Area[A]]{
    override protected def lengthPostfixOps(lengthUnit2: LengthUnit) = apply(lengthUnit1 * lengthUnit2)
  }

  // Length / Time -> Velocity
  // Length / TimeSquared -> Acceleration
  def apply(velocityUnit: VelocityUnit): Velocity[A]
  def apply(accelerationUnit: AccelerationUnit): Acceleration[A]

  // for style "1.0 m/s" ( = "1.0.m(/).s")
  override protected def lengthPer(lengthUnit: LengthUnit) =
    new TimePostfixOps[Velocity[A]] with TimeSquaredPostfixOps[Acceleration[A]]{
      override protected def timePostfixOps(timeUnit: TimeUnit) = apply(lengthUnit / timeUnit)
      override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit) =
        apply(lengthUnit / timeSquaredUnit)
    }
}