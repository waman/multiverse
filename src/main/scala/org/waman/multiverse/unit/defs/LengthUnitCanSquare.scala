package org.waman.multiverse.unit.defs

import org.waman.multiverse.ProductUnit

private[defs] trait LengthUnitCanSquare { self: LengthUnit with LengthUnitCanCubic =>

  def squared: AreaUnit =
    new LengthPoweredAreaUnit(this, self.symbols.map(_+".squared")){
      override def *(lengthUnit: LengthUnit): VolumeUnit = {
        if (lengthUnit == self) self.cubic
        else super.*(lengthUnit)
      }
    }

  def *(lengthUnit: LengthUnit): AreaUnit =
    if(this == lengthUnit)
      this.squared
    else
      new ProductUnit[AreaUnit, LengthUnit, LengthUnit](self, lengthUnit) with AreaUnit
}

private[defs] trait LengthUnitCanCubic { self: LengthUnit =>

  def cubic: VolumeUnit = new LengthPoweredVolumeUnit(self, self.symbols.map(_+".cubic"))
}
