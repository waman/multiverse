import java.nio.file.Path

case class Constant(name: String, value: String)

case class ScalePrefix(name: String, prefix: String, scale: String)

case class ContextJson(name: String, symbol: String)

case class UnitCategory(SIUnit: String, units: Array[UnitJson])

//{"name":"ElectronVolt", "symbol":"eV", "intervalInSIUnit":" * ElementaryCharge.unitValueInSIUnit",
//  "scalePrefixes":true, "notExact":true},
case class UnitJson(
  name: String,
  symbol: String,
  aliases: Array[String],
  intervalInSIUnit: String,
  baseUnit: String,
  scalePrefixes: Boolean,
  excludePrefixes: Array[String],
  notExact: Boolean
){
  def canonicalizeAndExpandScalePrefixes(): Seq[CanonicalizedUnitJson] = {
    val interval =
      if(this.intervalInSIUnit.startsWith("Constants.")) this.intervalInSIUnit
      else s"""r"${this.intervalInSIUnit}""""

    val factor =
      if(this.baseUnit == null) ""
      else s""" * ${this.baseUnit}.intervalInSIUnit"""

    if(scalePrefixes){
      CanonicalizedUnitJson(this.name, this.symbol, Array(), interval, this.notExact) +:
        GenerationUtil.scalePrefixes.map{ p =>
          assert(this.aliases == null || this.aliases.isEmpty)
          CanonicalizedUnitJson(p.name + this.name, p.prefix + this.symbol, Array(),
            s"""$interval * r"${p.scale}"$factor""", this.notExact)
        }
    }else{
      val aliases: Array[String] = if(this.aliases == null) Array() else this.aliases
      Seq(CanonicalizedUnitJson(this.name, this.symbol, aliases, s"""$interval$factor""", this.notExact))
    }
  }
}

case class CanonicalizedUnitJson(
   name: String,
   symbol: String,
   aliases: Array[String],
   intervalInSIUnit: String,
   notExact: Boolean)

case class UnitSource(src: Path, packageName: String, unitName: String, unitJson: UnitJson)

object GenerationUtil{

  val scalePrefixes: Seq[ScalePrefix] = Seq(
    ScalePrefix("yocto", "y", "1e-24"),
    ScalePrefix("zepto", "z", "1e-21"),
    ScalePrefix("atto" , "a", "1e-18"),
    ScalePrefix("femto", "f", "1e-15"),
    ScalePrefix("pico" , "p", "1e-12"),
    ScalePrefix("nano" , "n", "1e-9"),
    ScalePrefix("micro", "μ", "1e-6"),
    ScalePrefix("milli", "m", "1e-3"),
    ScalePrefix("centi", "c", "1e-2"),
    ScalePrefix("deci" , "d", "1e-1"),

    ScalePrefix("deca" , "da", "1e1"),
    ScalePrefix("hecto", "h", "1e2"),
    ScalePrefix("kilo" , "k", "1e3"),
    ScalePrefix("mega" , "M", "1e6"),
    ScalePrefix("giga" , "G", "1e9"),
    ScalePrefix("tera" , "T", "1e12"),
    ScalePrefix("peta" , "P", "1e15"),
    ScalePrefix("exa"  , "E", "1e18"),
    ScalePrefix("zetta", "Z", "1e21"),
    ScalePrefix("yotta", "Y", "1e24")
  )
}