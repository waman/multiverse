package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

abstract class Angle[A: Fractional]{

  def deg: A = Angle.radianToDegree(rad)
  def rad: A

  def /(timeUnit: TimeUnit): AngularVelocity[A] = {
    val timeInSecond: Double = timeUnit match{
      case _ if timeUnit == TimeUnit.ms  => 0.001
      case _ if timeUnit == TimeUnit.s   => 1
      case _ if timeUnit == TimeUnit.min => 60
      case _ if timeUnit == TimeUnit.h   => 3600
    }
    new UnitInterpreter(deg / timeInSecond).`rad/s`
  }
}

object Angle{

  def pi[A: Fractional]: A = implicitly[Fractional[A]].fromReal(Real.pi)

  def radianToDegree[A: Fractional](rad: A): A = rad * 180 / pi

  def degreeToRadian[A: Fractional](deg: A): A = deg * pi / 180
}

sealed abstract class AngleUnit(code: String)

object AngleUnit{
  case object deg extends AngleUnit("degree")
  case object rad extends AngleUnit("radian")
}

trait AngleUnitInterpreter[A]{

  def deg: Angle[A]
  def rad: Angle[A]

  def apply(unit: AngleUnit): Angle[A] = unit match {
    case _ if unit == AngleUnit.deg => deg
    case _ if unit == AngleUnit.rad => rad
  }

  def rad(per: Per): AnglePer

  trait AnglePer{
    def s: AngularVelocity[A]
  }
}