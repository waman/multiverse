package waman.multiverse

import spire.math.Real

sealed trait TypelessLinearUnit extends LinearUnit[TypelessLinearUnit]{

  def multiply(second: TypelessLinearUnit): TypelessLinearUnit =
    second match {
      case Dimless => this
      case s: SimpleTypelessLinearUnit =>
        this.multiplyOption(s) match {
          case Some(x) => x
          case None => new TypelessProductUnit(this, s)
        }
      case p: TypelessProductUnit =>
        this.multiply(p.firstUnit).multiply(p.secondUnit)
      case q: TypelessQuotientUnit =>
        this.multiply(q.numeratorUnit).divide(q.denominatorUnit)
    }

  /**
    * Return a Some of the reduced unit if the product is reducible, or None if not.
    * ex)
    *   (km/L).multiplyOption(L) => Some(km)
    *   (km/L).multiplyOption(km) => None
    *   km.multiplyOption(s) => None
    *   Dimless.multiplyOption(m) => Some(m)
    *   m.multiplyOption(Dimless) => Some(m)
    */
  private[multiverse] def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit]

  def divide(deno: TypelessLinearUnit): TypelessLinearUnit =
    deno match {
      case Dimless => this
      case s: SimpleTypelessLinearUnit =>
        this.divideOption(s) match {
          case Some(x) => x
          case None => new TypelessQuotientUnit(this, s)
        }
      case p: TypelessProductUnit =>
        this.divide(p.firstUnit).divide(p.secondUnit)
      case q: TypelessQuotientUnit =>
        this.divide(q.numeratorUnit).multiply(q.denominatorUnit)
    }

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
  private[multiverse] def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit]
}

object TypelessLinearUnit {
  def apply(delegate: LinearUnit[_]): TypelessLinearUnit =
    delegate match {
      case u: TypelessLinearUnit => u
      case p: ProductUnit[_, _, _] =>
        new TypelessProductUnit(TypelessLinearUnit(p.firstUnit), TypelessLinearUnit(p.secondUnit))
      case q: QuotientUnit[_, _, _] =>
        new TypelessQuotientUnit(TypelessLinearUnit(q.numeratorUnit), TypelessLinearUnit(q.denominatorUnit))
      case _ => new SimpleTypelessLinearUnit(delegate)
    }
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

  override def multiply(second: TypelessLinearUnit): TypelessLinearUnit = second

  private[multiverse]
  override def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    Some(TypelessLinearUnit(second))

  private[multiverse]
  override def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] = None
}

class SimpleTypelessLinearUnit(val delegate: LinearUnit[_]) extends TypelessLinearUnit{
  require(!delegate.isInstanceOf[ProductUnit[_, _, _]])
  require(!delegate.isInstanceOf[QuotientUnit[_, _, _]])

  override def interval: Real = delegate.interval
  override def name: String = delegate.name
  override def symbol: String = delegate.symbol
  override def aliases: Seq[String] = delegate.aliases
  override def getSIUnit: TypelessLinearUnit = TypelessLinearUnit(delegate.getSIUnit.asInstanceOf[LinearUnit[_]])
  override def dimension: Map[DimensionSymbol, Int] = delegate.dimension

  private[multiverse]
  override def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] = None

 private[multiverse]
  override def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
  if (this.delegate == deno) Some(Dimless)
    else None
}

class TypelessProductUnit(firstUnit: TypelessLinearUnit, secondUnit: TypelessLinearUnit)
  extends ProductUnit[TypelessLinearUnit, TypelessLinearUnit, TypelessLinearUnit](firstUnit, secondUnit)
    with TypelessLinearUnit {

  override def getSIUnit: TypelessLinearUnit = this.firstUnit.getSIUnit * this.secondUnit.getSIUnit
  override lazy val dimension: Map[DimensionSymbol, Int] = super.dimension.filter(_._2 != 0).withDefaultValue(0)

  private[multiverse]
  override def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.secondUnit.multiplyOption(second) match {
      case Some(Dimless) => Some(TypelessLinearUnit(this.firstUnit))
      case Some(y) => Some(new TypelessProductUnit(this.firstUnit, y))
      case None => this.firstUnit.multiplyOption(second) match {
        case Some(Dimless) => Some(TypelessLinearUnit(this.secondUnit))
        case Some(x) => Some(new TypelessProductUnit(x, this.secondUnit))
        case None => None
      }
    }

  private[multiverse]
  override def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.secondUnit.divideOption(deno) match {
      case Some(Dimless) => Some(TypelessLinearUnit(this.firstUnit))
      case Some(y) => Some(new TypelessProductUnit(this.firstUnit, y))
      case None => this.firstUnit.divideOption(deno) match {
        case Some(Dimless) => Some(TypelessLinearUnit(this.secondUnit))
        case Some(x) => Some(new TypelessProductUnit(x, this.secondUnit))
        case None => None
      }
    }
}

class TypelessQuotientUnit(numeratorUnit: TypelessLinearUnit, denominatorUnit: TypelessLinearUnit)
  extends QuotientUnit[TypelessLinearUnit, TypelessLinearUnit, TypelessLinearUnit](numeratorUnit, denominatorUnit)
    with TypelessLinearUnit {

  override protected def newName: String = this.numeratorUnit match {
    case Dimless => "one per " + this.denominatorUnit.name
    case _ => super.newName
  }

  override protected def newSymbol: String = this.numeratorUnit match {
    case Dimless => "1/" + LiteralComposite.mkSymbol(this.denominatorUnit)
    case _ => super.newSymbol
  }

  override def getSIUnit: TypelessLinearUnit = this.numeratorUnit.getSIUnit / this.denominatorUnit.getSIUnit
  override lazy val dimension: Map[DimensionSymbol, Int] = super.dimension.filter(_._2 != 0).withDefaultValue(0)

  private[multiverse]
  override def multiplyOption(second: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.denominatorUnit.divideOption(second) match {
      case Some(Dimless) => Some(TypelessLinearUnit(this.numeratorUnit))
      case Some(y) => Some(new TypelessQuotientUnit(this.numeratorUnit, y))
      case None =>
        this.numeratorUnit.multiplyOption(second) match {
          case Some(x) => Some(new TypelessQuotientUnit(x, this.denominatorUnit))
          case None =>
            // (x/y)*z => (x*z)/y
            Some(
              new TypelessQuotientUnit(
                new TypelessProductUnit(this.numeratorUnit, second),
                this.denominatorUnit))
        }
    }

  private[multiverse]
  override def divideOption(deno: SimpleTypelessLinearUnit): Option[TypelessLinearUnit] =
    this.numeratorUnit.divideOption(deno) match {
      case Some(x) => Some(new TypelessQuotientUnit(x, this.denominatorUnit))
      case None =>
        this.denominatorUnit.multiplyOption(deno) match {
          case Some(Dimless) => Some(TypelessLinearUnit(this.numeratorUnit))
          case Some(y) => Some(new TypelessQuotientUnit(this.numeratorUnit, y))
          case None =>
            // (x/y)/z => x/(y*z)
            Some(
              new TypelessQuotientUnit(
                this.numeratorUnit,
                new TypelessProductUnit(this.denominatorUnit, deno)))
        }
    }
}