package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

abstract class Angle[A: Fractional] extends UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  def deg: A = Angle.radianToDegree(rad)
  def rad: A

  def apply(unit: AngleUnit): A = unit.accept(this)

  def /(timeUnit: TimeUnit): AngularVelocity[A] =
    new UnitInterpreter(deg / real(timeUnit.inSecond)).`rad/s`
}

object Angle{

  def pi[A: Fractional]: A = implicitly[Fractional[A]].fromReal(Real.pi)

  def radianToDegree[A: Fractional](rad: A): A = rad * 180 / pi
  def degreeToRadian[A: Fractional](deg: A): A = deg * pi / 180
}

sealed abstract class AngleUnit(inRadian: Real = r"1"){
  def accept[A](unit: Angle[A]): A
  def accept[A](aui: AngleUnitInterpreter[A]): Angle[A]
}

object AngleUnit{
  case object Degree extends AngleUnit(Real.pi / r"180"){
    override def accept[A](a: Angle[A]): A = a.deg
    override def accept[A](aui: AngleUnitInterpreter[A]): Angle[A] = aui.deg
  }

  case object Radian extends AngleUnit(){
    override def accept[A](a: Angle[A]): A = a.rad
    override def accept[A](aui: AngleUnitInterpreter[A]): Angle[A] = aui.rad
  }
}

trait AngleUnitInterpreter[A]{

  def deg: Angle[A]
  def rad: Angle[A]

  def apply(unit: AngleUnit): Angle[A] = unit.accept(this)

  def rad(per: Per): AnglePer

  trait AnglePer{
    def s: AngularVelocity[A]
  }
}