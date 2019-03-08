package org.waman.multiverse.predef

import org.waman.multiverse.PhysicalUnit

import scala.reflect.runtime.{universe => ru}

trait PhysicalUnitPredefProxy {

  protected def getUnitsType: ru.Type

  def getUnits: Seq[PhysicalUnit[_]] = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val puType = ru.typeOf[PhysicalUnit[_]]

    getUnitsType.decls.filter(_.isMethod).map(_.asMethod)
      .filter(m => m.isPublic && m.paramLists.isEmpty && m.returnType <:< puType)
      .map(im.reflectMethod(_)().asInstanceOf[PhysicalUnit[_]]).toSeq
  }
}
