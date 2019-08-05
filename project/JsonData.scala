case class Constant(name: String, value: String)

case class ContextJson(name: String, symbol: String)

case class UnitCategory(SIUnit: String, units: Array[UnitJson])

// ex)
//{"name":"electronvolt", "symbol":"eV", "intervalInSIUnit":"Constants.ElementaryCharge",
//   scalePrefixes":true, "notExact":true},
case class UnitJson(
  name: String,
  symbol: String,
  aliases: Array[String],
  intervalInSIUnit: String,
  baseUnit: String,
  scalePrefixes: Boolean,
  excludePrefixes: Array[String],
  notExact: Boolean,
  attributes: Array[Attribute]
){
  def extractInterval: String =
      if(this.intervalInSIUnit != null)
        this.intervalInSIUnit
      else
        this.attributes.find(_.default) match {
          case Some(a) => a.intervalInSIUnit
          case None => throw new RuntimeException(
            "Neither an intervalInSIUnit element or a default attribute exists: " + this.name)
        }

  def extractBaseUnit: String =
    if(this.baseUnit != null)
      this.baseUnit
    else if(this.intervalInSIUnit != null)
      null
    else
      this.attributes.find(_.default) match {
        case Some(a) => a.baseUnit
        case None => throw new RuntimeException(
          "Neither a baseUnit element or a default attribute exists: " + this.name)
      }

  def canonicalizeAndExpandScalePrefixes(): Seq[CanonicalizedUnitJson] = {
    val interval = makeIntervalExpression(extractInterval, extractBaseUnit)
    val aliases = nonNull(this.aliases)

    if(scalePrefixes){
      val nm = this.symbol +: aliases

      CanonicalizedUnitJson(this.name, this.symbol, aliases, interval, this.notExact, Nil) +:
        GenerationUtil.scalePrefixes.map{ p =>
          val al = (p.prefix +: p.aliases).flatMap(ps => nm.map(ns => ps + ns)).tail
          CanonicalizedUnitJson(p.name + this.name, p.prefix + this.symbol, nonNull(al),
            s"""$interval * r"${p.scale}"""", this.notExact, Nil)
        }
    }else{
      val atts = if(this.attributes == null) Nil else this.attributes.toList

      val u = CanonicalizedUnitJson(
        this.name, this.symbol, nonNull(this.aliases), interval, this.notExact, atts)

      val us = atts.map{ a =>
        CanonicalizedUnitJson(
          s"${u.name}(${a.name})", s"${u.symbol}(${a.name})", aliases.map(al => s"$al(${a.name})"),
          makeIntervalExpression(a.intervalInSIUnit, a.baseUnit), a.notExact, Nil)
      }

      u +: us
    }
  }

  private def makeIntervalExpression(interval: String, baseUnit: String): String = {
    val i =
      if(interval.startsWith("Constants."))
        interval
      else
        s"""r"$interval""""

    val bu =
      if(baseUnit == null) ""
      else s""" * $baseUnit.intervalInSIUnit"""

    i + bu
  }

  private def  nonNull(a: Seq[String]): Seq[String] = if(a == null) Nil else a.toList
}

case class Attribute(name: String, intervalInSIUnit: String, baseUnit: String, notExact: Boolean, default: Boolean)

case class CanonicalizedUnitJson(
   name: String,
   symbol: String,
   aliases: Seq[String],
   intervalInSIUnit: String,
   notExact: Boolean,
   attributes: Seq[Attribute])

case class ScalePrefix(name: String, prefix: String, aliases: Seq[String], scale: String)

object GenerationUtil{

  val scalePrefixes: Seq[ScalePrefix] = Seq(
    ScalePrefix("yocto", "y", Nil, "1e-24"),
    ScalePrefix("zepto", "z", Nil, "1e-21"),
    ScalePrefix("atto" , "a", Nil, "1e-18"),
    ScalePrefix("femto", "f", Nil, "1e-15"),
    ScalePrefix("pico" , "p", Nil, "1e-12"),
    ScalePrefix("nano" , "n", Nil, "1e-9"),
    ScalePrefix("micro", "μ", Seq("mc"), "1e-6"),
    ScalePrefix("milli", "m", Nil, "1e-3"),
    ScalePrefix("centi", "c", Nil, "1e-2"),
    ScalePrefix("deci" , "d", Nil, "1e-1"),

    ScalePrefix("deca", "da", Nil, "1e1"),
    ScalePrefix("hecto", "h", Nil, "1e2"),
    ScalePrefix("kilo" , "k", Nil, "1e3"),
    ScalePrefix("mega" , "M", Nil, "1e6"),
    ScalePrefix("giga" , "G", Nil, "1e9"),
    ScalePrefix("tera" , "T", Nil, "1e12"),
    ScalePrefix("peta" , "P", Nil, "1e15"),
    ScalePrefix("exa"  , "E", Nil, "1e18"),
    ScalePrefix("zetta", "Z", Nil, "1e21"),
    ScalePrefix("yotta", "Y", Nil, "1e24")
  )
}