package waman.multiverse

sealed abstract class DimensionSymbol(val unit: String)

object DimensionSymbol{
  final case object M extends DimensionSymbol("kg")
  final case object L extends DimensionSymbol("m")
  final case object T extends DimensionSymbol("s")
  final case object I extends DimensionSymbol("A")
  final case object Θ extends DimensionSymbol("K")
  final case object N extends DimensionSymbol("mol")
  final case object J extends DimensionSymbol("cd")

  def values: Seq[DimensionSymbol] = Seq(M, L,  T, I, Θ, N, J)

  def valueOf(s: String): Option[DimensionSymbol] = s match {
    case "M" => Some(M)
    case "L" => Some(L)
    case "T" => Some(T)
    case "I" => Some(I)
    case "Θ" => Some(Θ)
    case "N" => Some(N)
    case "J" => Some(J)
    case _ => None
  }

  private val uppers = "⁰¹²³⁴⁵⁶⁷⁸⁹⁻"

  private def toUppers(c: Char): Char = c match {
    case '-' => uppers.charAt(10)
    case n => uppers.charAt(n - '0')
  }

  def toStringWithUnit(d: Map[DimensionSymbol, Int]): String = toString(d, _.unit, " ")

  def toStringWithSymbol(d: Map[DimensionSymbol, Int]): String = toString(d, _.toString, "")

  private def toString(d: Map[DimensionSymbol, Int], f: DimensionSymbol => String, sep: String): String =
    if (d.isEmpty) "-"
    else
      values.map(s => (s, d(s))).filter(_._2 != 0).map{
        case (s, 1) => f(s)
        case (s, n) => f(s) + n.toString.map(toUppers).mkString("")
      }.mkString(sep)
}