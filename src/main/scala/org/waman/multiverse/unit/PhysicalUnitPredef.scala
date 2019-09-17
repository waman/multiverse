package org.waman.multiverse.unit

import org.waman.multiverse.PhysicalUnit
import scala.reflect.runtime.{universe => ru}

trait PhysicalUnitPredef[U <: PhysicalUnit[U]]{

  protected def getUnitsType: ru.Type

  def getUnits: Seq[U] = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    getUnitsType.decls.filter(_.isModule).map(_.asModule)
      .map(m.reflectModule(_).instance.asInstanceOf[U]).toSeq
  }
}
