package waman.multiverse.typeless

import spire.math.Real
import waman.multiverse._

import scala.annotation.tailrec

sealed trait TypelessLinearUnit extends LinearUnit[TypelessLinearUnit]{

  def ^(n: Int): TypelessLinearUnit
  def reciprocal: TypelessLinearUnit
  override def asTypeless: TypelessLinearUnit = this

  def multiply(second: TypelessLinearUnit): TypelessLinearUnit =
    second match {
      case Dimless => this
      case s: SimpleTypelessLinearUnit =>
        this.multiplyOption(s) match {
          case Some(x) => x
          case None => newSimpleProduct(s)
        }
      case p: TypelessProductUnit =>
        this.multiply(p.firstUnit).multiply(p.secondUnit)
      case q: TypelessQuotientUnit =>
        this.multiply(q.numeratorUnit).divide(q.denominatorUnit)
      case r: TypelessReciprocalUnit =>
        this.divide(r.baseUnit)
      case p: TypelessPowerUnit =>
        @tailrec
        def mul(acc: TypelessLinearUnit, n: Int): TypelessLinearUnit = n match {
          case 0 => acc
          case _ => mul(acc.multiply(p.baseUnit), n-1)
        }
        mul(this, p.power)
    }
  
  protected def newSimpleProduct(second: SimpleTypelessLinearUnit): TypelessLinearUnit

  /**
    * Return a Some of the reduced unit if the product is reducible, or None if not.
    * ex)
    *   (km/L).multiplyOption(L) => Some(km)
    *   (km/L).multiplyOption(km) => None
    *   km.multiplyOption(s) => None
    *   Dimless.multiplyOption(m) => Some(m)
    *   m.multiplyOption(Dimless) => Some(m)
    */
  private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit]

  def divide(deno: TypelessLinearUnit): TypelessLinearUnit =
    deno match {
      case Dimless => this
      case s: SimpleTypelessLinearUnit =>
        this.divideOption(s) match {
          case Some(x) => x
          case None => newSimpleQuotient(s)
        }
      case p: TypelessProductUnit =>
        this.divide(p.firstUnit).divide(p.secondUnit)
      case q: TypelessQuotientUnit =>
        this.divide(q.numeratorUnit).multiply(q.denominatorUnit)
      case r: TypelessReciprocalUnit =>
        this.multiply(r.baseUnit)
      case p: TypelessPowerUnit =>
        @tailrec
        def div(acc: TypelessLinearUnit, n: Int): TypelessLinearUnit = n match {
          case 0 => acc
          case _ => div(acc.divide(p.baseUnit), n-1)
        }
        div(this, p.power)
    }
  
  protected def newSimpleQuotient(deno: SimpleTypelessLinearUnit): TypelessLinearUnit

  /**
    * Return a Some of the reduced unit if the quotient is reducible, or None if not.
    * ex)
    *   (m*s).divideOption(m) => Some(s)
    *   (m*s).divideOption(s) => Some(m)
    *   m.divideOption(s) => None
    *   m.divideOption(m) => Some(Dimless)
    *   Dimless.divideOption(m) => None
    *   m.divideOption(Dimless) => Some(m)
    */
  private[typeless] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit]
}

// SimpleTypelessLinearUnit | TypelessProductUnit | TypelessPowerUnit
private[typeless] sealed trait FlatTypelessLinearUnit extends TypelessLinearUnit {

  override def ^(n: Int): TypelessLinearUnit = n match {
    case 0 => Dimless
    case 1 => this
    case -1 => this.reciprocal
    case _ if n < -1 => this.positivePower(-n).reciprocal
    case _ =>  this.positivePower(n)
  }

  private[typeless] def positivePower(n: Int): FlatTypelessLinearUnit

  override def reciprocal: TypelessLinearUnit = new TypelessReciprocalUnit(this)

  // In the None case, x^2 multiplyOption x => x^3
  override private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit): Option[FlatTypelessLinearUnit]

  override protected def newSimpleProduct(second: SimpleTypelessLinearUnit): TypelessLinearUnit =
    new TypelessProductUnit(this, second)

  override protected def newSimpleQuotient(deno: SimpleTypelessLinearUnit): TypelessLinearUnit =
    new TypelessQuotientUnit(this, deno)
}

/**
  * Typeless dimensionless unit
  */
object Dimless extends TypelessLinearUnit{
  override def interval: Real = Real.one
  override def name: String = "dimensionless unit"
  override def symbol: String = "[null]"
  override def aliases: Seq[String] = Nil
  override def getSIUnit: TypelessLinearUnit = this
  override val dimension: Map[DimensionSymbol, Int] = Map().withDefaultValue(0)

  override def ^(n: Int): TypelessLinearUnit = this
  override def reciprocal: TypelessLinearUnit = this

  override def multiply(second: TypelessLinearUnit): TypelessLinearUnit = second

  override private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    Some(second)

  override protected def newSimpleProduct(second: SimpleTypelessLinearUnit): TypelessLinearUnit = {
    require(requirement = false, "maybe unreached")
    second
  }

  override private[typeless] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    Some(new TypelessReciprocalUnit(deno))

  override protected def newSimpleQuotient(deno: SimpleTypelessLinearUnit): TypelessLinearUnit = {
    require(requirement = false, "maybe unreached")
    new TypelessReciprocalUnit(deno)
  }
}

class SimpleTypelessLinearUnit(val delegate: LinearUnit[_]) extends FlatTypelessLinearUnit{
  require(!delegate.isInstanceOf[ProductUnit[_, _, _]])
  require(!delegate.isInstanceOf[QuotientUnit[_, _, _]])
  require(!delegate.isInstanceOf[TypelessLinearUnit])

  override def interval: Real = delegate.interval
  override def name: String = delegate.name
  override def symbol: String = delegate.symbol
  override def aliases: Seq[String] = delegate.aliases
  override def getSIUnit: TypelessLinearUnit = delegate.getSIUnit.asInstanceOf[LinearUnit[_]].asTypeless
  override def dimension: Map[DimensionSymbol, Int] = delegate.dimension

  override private[typeless] def positivePower(n: Int): FlatTypelessLinearUnit =
    new TypelessPowerUnit(this, n)
  
  override private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit) =
    if (this == second) Some(new TypelessPowerUnit(this, 2))
    else None
  
  override private[typeless] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    if (this.delegate == deno) Some(Dimless)
    else None
}

class TypelessProductUnit(firstUnit: FlatTypelessLinearUnit, secondUnit: FlatTypelessLinearUnit)
  extends ProductUnit[TypelessLinearUnit, TypelessLinearUnit, TypelessLinearUnit](firstUnit, secondUnit)
    with FlatTypelessLinearUnit {

  override def getSIUnit: TypelessLinearUnit = this.firstUnit.getSIUnit.multiply(this.secondUnit.getSIUnit)
  override lazy val dimension: Map[DimensionSymbol, Int] = super.dimension.filter(_._2 != 0).withDefaultValue(0)

  override private[typeless] def positivePower(n: Int): FlatTypelessLinearUnit =
    new TypelessProductUnit(this.firstUnit.positivePower(n), this.secondUnit.positivePower(n))
  
  override private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit): Option[FlatTypelessLinearUnit] =
    this.secondUnit.multiplyOption(second) match {
      case Some(y) => Some(new TypelessProductUnit(this.firstUnit, y))
      case None => this.firstUnit.multiplyOption(second) match {
        case Some(x) => Some(new TypelessProductUnit(x, this.secondUnit))
        case None => None
      }
    }
 
  override private[typeless] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.secondUnit.divideOption(deno) match {
      case Some(Dimless) => Some(this.firstUnit)
      case Some(y: FlatTypelessLinearUnit) => Some(new TypelessProductUnit(this.firstUnit, y))
      case _ => this.firstUnit.divideOption(deno) match {
        case Some(Dimless) => Some(this.secondUnit)
        case Some(x: FlatTypelessLinearUnit) => Some(new TypelessProductUnit(x, this.secondUnit))
        case _ => None
      }
    }
}

class TypelessQuotientUnit(numeratorUnit: FlatTypelessLinearUnit, denominatorUnit: FlatTypelessLinearUnit)
  extends QuotientUnit[TypelessLinearUnit, TypelessLinearUnit, TypelessLinearUnit](numeratorUnit, denominatorUnit)
    with TypelessLinearUnit {

  override def getSIUnit: TypelessLinearUnit = this.numeratorUnit.getSIUnit.divide(this.denominatorUnit.getSIUnit)
  override lazy val dimension: Map[DimensionSymbol, Int] = super.dimension.filter(_._2 != 0).withDefaultValue(0)

  override def ^(n: Int): TypelessLinearUnit =
    if (n == 0)
      Dimless
    else if (n > 0)
      new TypelessQuotientUnit(this.numeratorUnit.positivePower(n), this.denominatorUnit.positivePower(n))
    else
      new TypelessQuotientUnit(this.denominatorUnit.positivePower(-n), this.numeratorUnit.positivePower(-n))

  override def reciprocal: TypelessLinearUnit =
    new TypelessQuotientUnit(this.denominatorUnit, this.numeratorUnit)

  override private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.denominatorUnit.divideOption(second) match {
      case Some(Dimless) => Some(this.numeratorUnit)
      case Some(y: FlatTypelessLinearUnit) => Some(new TypelessQuotientUnit(this.numeratorUnit, y))
      case _ =>
        this.numeratorUnit.multiplyOption(second) match {
          case Some(x) => Some(new TypelessQuotientUnit(x, this.denominatorUnit))
          case None => None
        }
    }

  // (x/y)*z => (x*z)/y
  override protected def newSimpleProduct(second: SimpleTypelessLinearUnit): TypelessLinearUnit =
    new TypelessQuotientUnit(
      new TypelessProductUnit(this.numeratorUnit, second),
      this.denominatorUnit)

  override private[typeless] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.numeratorUnit.divideOption(deno) match {
      case Some(Dimless) => Some(this.denominatorUnit.reciprocal)
      case Some(x: FlatTypelessLinearUnit) => Some(new TypelessQuotientUnit(x, this.denominatorUnit))
      case _ =>
        this.denominatorUnit.multiplyOption(deno) match {
          case Some(y) => Some(new TypelessQuotientUnit(this.numeratorUnit, y))
          case None => None
        }
    }

  // (x/y)/z => x/(y*z)
  override protected def newSimpleQuotient(deno: SimpleTypelessLinearUnit): TypelessLinearUnit =
    new TypelessQuotientUnit(
      this.numeratorUnit,
      new TypelessProductUnit(this.denominatorUnit, deno))
}

class TypelessReciprocalUnit(val baseUnit: FlatTypelessLinearUnit)
  extends LinearUnit[TypelessLinearUnit]
    with TypelessLinearUnit {

  override val name: String = s"one per ${baseUnit.name}"

  override val symbol: String = this.baseUnit match {
    case p: TypelessPowerUnit => p.baseUnit.symbol + toSuperscripts(-p.power)
    case _ => baseUnit.symbol + toSuperscripts(-1)
  }

  override val interval: Real = baseUnit.interval.reciprocal

  override def aliases: Seq[String] = this.baseUnit match {
    case p: TypelessPowerUnit =>
      val ps = toSuperscripts(-p.power)
      p.baseUnit.aliases.map(_ + ps)
    case _ =>
      val ps = toSuperscripts(-1)
      baseUnit.aliases.map(_ + ps)
  }

  override def getSIUnit: TypelessLinearUnit = this.baseUnit.getSIUnit.reciprocal

  override lazy val dimension: Map[DimensionSymbol, Int] =
    this.baseUnit.dimension.map(e => (e._1, -e._2)).withDefaultValue(0)

  override def ^(n: Int): TypelessLinearUnit =
    if (n == 0) Dimless
    else if (n > 0) this.baseUnit.positivePower(n).reciprocal
    else this.baseUnit.positivePower(-n)

  override def reciprocal: TypelessLinearUnit = this.baseUnit

  override private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.baseUnit.divideOption(second) match {
      case Some(Dimless) => Some(Dimless)
      case Some(x) => Some(x.reciprocal)
      case None => Some(new TypelessQuotientUnit(second, this.baseUnit))
    }

  override protected def newSimpleProduct(second: SimpleTypelessLinearUnit): TypelessLinearUnit = {
    require(requirement = false, "maybe unreached")
    new TypelessQuotientUnit(second, this.baseUnit)
  }

  override private [multiverse] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.baseUnit.multiplyOption(deno) match {
      case Some(x) => Some(x.reciprocal)
      case None => Some(new TypelessProductUnit(this.baseUnit, deno).reciprocal)
    }

  override protected def newSimpleQuotient(deno: SimpleTypelessLinearUnit): TypelessLinearUnit = {
    require(requirement = false, "maybe unreached")
    new TypelessProductUnit(this.baseUnit, deno).reciprocal
  }
}

/**
  * Note that a base unit can be a SimpleTypelessLinearUnit like m^3 (other than (kg*m)^3)
  */
class TypelessPowerUnit(val baseUnit: SimpleTypelessLinearUnit, val power: Int)
  extends LinearUnit[TypelessLinearUnit]
    with FlatTypelessLinearUnit with LiteralComposite {

  require(power > 1)

  override val name: String =  power match {
    case 2 => "square " + baseUnit.name
    case 3 => "cubic " + baseUnit.name
    case _ => s"${power}th power of ${baseUnit.name}"
  }

  override val symbol: String = baseUnit.symbol + toSuperscripts(power)
  override val interval: Real = baseUnit.interval**power

  override def aliases: Seq[String] = {
    val ps = toSuperscripts(power)
    baseUnit.aliases.map(_ + ps)
  }

  override def getSIUnit: TypelessLinearUnit = this.baseUnit.getSIUnit ^ power

  override lazy val dimension: Map[DimensionSymbol, Int] =
    this.baseUnit.dimension.map(e => (e._1, e._2 * power)).withDefaultValue(0)

  override private[typeless] def positivePower(n: Int): FlatTypelessLinearUnit =
    new TypelessPowerUnit(this.baseUnit, this.power*n)

  override private[typeless] def multiplyOption(second: SimpleTypelessLinearUnit): Option[FlatTypelessLinearUnit] =
    if (this.baseUnit == second) Some(new TypelessPowerUnit(this.baseUnit, power+1))
    else None

  override private[typeless] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    if (this.baseUnit == deno)
      if (this.power == 2) Some(this.baseUnit)
      else Some(new TypelessPowerUnit(this.baseUnit, power-1))
    else None
}