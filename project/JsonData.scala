case class Scale(name: String, prefix: String, scale: String)

case class ContextJson(name: String, symbol: String)

case class UnitJson
(
  imports: Array[String],
  baseUnit: String,
  baseUnitAccessor: String,
  multiplicatives: Array[Array[String]],
  divisibles: Array[Array[String]],
  products: Array[Array[String]],
  quotients: Array[Array[String]],
  square: String,
  cubic: String,
  context: String,
  constants: Array[Constant]
){
  def getImports: Array[String] = if(imports != null) imports else Array[String]()

  def hasSquare: Boolean = square != null
  def hasCubic: Boolean = cubic != null

  def getMultiplicatives: Array[Array[String]] = if(multiplicatives != null) multiplicatives else Array()
  def getDivisibles     : Array[Array[String]] = if(divisibles != null) divisibles else Array()

  def getProducts: Array[Array[String]] = if(products != null) products else Array()
  def getQuotients: Array[Array[String]] = if(quotients != null) quotients else Array()

  def contextFullClassName: String = if(context != null) context else ""
  def contextClassName: String =
    if(context != null)contextFullClassName.substring(contextFullClassName.lastIndexOf(".") + 1)
    else ""

  def hasConstants: Boolean = constants != null && constants.length > 0
  def getCanonicalConstants: Array[CanonicalConstant] =
    if(constants != null)constants.flatMap(_.generateCanonicalConstants)
    else Array[CanonicalConstant]()
}

case class Constant
(
  name: String,
  symbol: String,
  symbols: Array[String],
  args: String,
  scalePrefixes: Boolean,
  excludePrefixes: Array[String],
  notExact: Boolean,
  mixed: String
){

  def generateCanonicalConstants: Array[CanonicalConstant] =
    if(symbols != null) {
      Array(CanonicalConstant(name, symbols, args, notExact, mixed))
    }else {
      val excludes: Array[String] = if (excludePrefixes != null) excludePrefixes else Array[String]()
      GenerationUtil.scalePrefixes.filterNot(sp => excludes.contains(sp.prefix)).map { sp =>
        val ss: Array[String] =
          if (sp.name == "Micro") {
            if (symbol.length == 1 && symbol.head.isLower)
              Array("micro" + name, sp.prefix + symbol)
            // MicroMetre => Array("microMetre", "μm")
            else
              Array("micro" + name, "micro" + headToUpper(symbol), sp.prefix + symbol)
            // MicroMetre => Array("microGauss", "microG", "μG")
          } else {
            Array(sp.prefix + symbol)
          }
        CanonicalConstant(sp.name + name, ss, s"""r"${sp.scale}"$args""", notExact, mixed)
      }.toArray
    }

  private def headToUpper(s: String) = s.charAt(0).toUpper.toString + s.substring(1)
}

case class CanonicalConstant
(
  name: String,
  symbols: Array[String],
  args: String,
  notExact: Boolean,
  mixed: String
){

  def symbolSeqString: String = symbols.map(quote).mkString("Seq(", ", ", ")")
  def isNotExact: Boolean = notExact
  def hasMixed: Boolean = mixed != null

  private def quote(s: String) = "\"" + s + "\""
}

object GenerationUtil{

  val scalePrefixes: Seq[Scale] = Seq(
    Scale("Yocto", "y", "1e-24"),
    Scale("Zepto", "z", "1e-21"),
    Scale("Atto" , "a", "1e-18"),
    Scale("Femto", "f", "1e-15"),
    Scale("Pico" , "p", "1e-12"),
    Scale("Nano" , "n", "1e-9"),
    Scale("Micro", "μ", "1e-6"),
    Scale("Milli", "m", "1e-3"),
    Scale("Centi", "c", "1e-2"),
    Scale("Deci" , "d", "1e-1"),
    Scale(""     , "" , "1"),
    Scale("Deca" , "da", "1e1"),
    Scale("Hecto", "h", "1e2"),
    Scale("Kilo" , "k", "1e3"),
    Scale("Mega" , "M", "1e6"),
    Scale("Giga" , "G", "1e9"),
    Scale("Tera" , "T", "1e12"),
    Scale("Peta" , "P", "1e15"),
    Scale("Exa"  , "E", "1e18"),
    Scale("Zetta", "Z", "1e21"),
    Scale("Yotta", "Y", "1e24")
  )

  // String
  def headToLower(s: String) = s.charAt(0).toLower.toString + s.substring(1)
}