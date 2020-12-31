package org.waman.multiverse

trait UnitInfo[U <: PhysicalUnit[U]] {
  def dimension: Map[DimensionSymbol, Int]

  def getSIUnit: U

  def getUnits: Seq[U]
}
