package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AnglePostfixOps[A]{
  def deg: A
  def rad: A
}

abstract class Angle[A: Fractional] extends AnglePostfixOps[A] with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  def deg: A = Angle.radianToDegree(rad)
  def rad: A

  def apply(unit: AngleUnit): A = unit.accept(this)

  def /(timeUnit: TimeUnit): AngularVelocity[A] =
    new UnitInterpreter(rad / real(timeUnit.inSecond)).`rad/s`
}

object Angle{

  def pi[A: Fractional]: A = implicitly[Fractional[A]].fromReal(Real.pi)

  def radianToDegree[A: Fractional](rad: A): A = rad * 180 / pi
  def degreeToRadian[A: Fractional](deg: A): A = deg * pi / 180
}

abstract class AngleUnit(inRadian: Real = r"1"){
  def accept[A](unit: Angle[A]): A
  def accept[A](ui: AngleUnitInterpreter[A]): Angle[A]
}

object AngleUnit{

  case object Degree extends AngleUnit(Real.pi / r"180"){
    override def accept[A](a: Angle[A]): A = a.deg
    override def accept[A](ui: AngleUnitInterpreter[A]): Angle[A] = ui.deg
  }

  case object Radian extends AngleUnit(){
    override def accept[A](a: Angle[A]): A = a.rad
    override def accept[A](ui: AngleUnitInterpreter[A]): Angle[A] = ui.rad
  }
}

trait AngleUnitInterpreter[A] extends AnglePostfixOps[Angle[A]]{

  val value: A
  def apply(unit: AngleUnit): Angle[A] = unit.accept(this)

  def rad(per: Per): TimePostfixOps[AngularVelocity[A]]
}