package org.waman.multiverse.unit.basic

import spire.math.Real
import org.waman.multiverse.ProductUnit
import org.waman.multiverse.unit.mechanics.TimeSquaredUnit

private[basic] trait TimeUnitCanSquare { self: TimeUnit =>

  def squared: TimeSquaredUnit =
     new TimeSquaredUnit{
       override val name: String = self.name + " squared"
       override val symbol: String = self.symbol + "²"
       override val interval: Real = self.interval**2
       override def aliases: Seq[String] = self.symbols.map(_+".squared")
     }

     def *(timeUnit: TimeUnit): TimeSquaredUnit =
       if(this == timeUnit)
        this.squared
       else
        new ProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit](self, timeUnit) with TimeSquaredUnit
}
