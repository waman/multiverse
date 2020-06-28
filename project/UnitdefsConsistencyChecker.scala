object UnitdefsConsistencyChecker {

  def test(jsons: JsonResources): Unit = {
    jsons.unitsystems.foreach{
      case u if u.id == "MKS" || u.id == "CGS" =>
        checkNecessaryEntriesExist(u, jsons, "mechanical", isNecessaryMechanicalUnit)
      case u if u.id == "MKSA" =>
        checkNecessaryEntriesExist(u, jsons, "electromagnetic", isNecessaryElectromagneticUnit)
      case _ =>
    }
  }

  def checkNecessaryEntriesExist(us: UnitSystemJson, jsons: JsonResources, unitKind: String,
                                 cond: LinearUnitDefinitionJson => Boolean): Unit = {
    val emUnits = jsons.linearUnitDefs.filter(cond).map(_.id)
    val entries = us.unitsystemInfo._evalEntries.map(_.quantity)
    emUnits.foreach{ emu =>
      if (!entries.contains(emu))
        throw new RuntimeException(s"""${us.id} must contain the $unitKind unit "$emu".""")
    }
  }

  private val auxiliaryMechanicalUnits = List("AreaFrequency", "TimePerLength", "TimeSquaredPerLength")

  private def isNecessaryMechanicalUnit(ud: LinearUnitDefinitionJson): Boolean = {
    if (auxiliaryMechanicalUnits.contains(ud.id))
      return false

    val dim = ud.unitCategory.dimension
    !auxiliaryMechanicalUnits.contains(ud.id) && dim.I == 0 && dim.J == 0 && dim.Θ == 0 && dim.N == 0
  }

  private val auxiliaryElectromagneticUnits = List("ElectricalConductance")

  private def isNecessaryElectromagneticUnit(ud: LinearUnitDefinitionJson): Boolean =
    !auxiliaryElectromagneticUnits.contains(ud.id) && ud.unitCategory.dimension.I != 0
}
