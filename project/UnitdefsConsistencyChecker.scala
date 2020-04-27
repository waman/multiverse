object UnitdefsConsistencyChecker {

  def test(jsons: JsonResources): Unit = {
    jsons.unitsystems.foreach{
      case u if u.id == "MKS" || u.id == "CGS" =>
        checkNecessaryEntriesExist(u, jsons, "mechanical", isMechanicalUnit)
      case u if u.id == "MKSA" =>
        checkNecessaryEntriesExist(u, jsons, "electromagnetic", isElectromagneticUnit)
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

  private def isMechanicalUnit(ud: LinearUnitDefinitionJson): Boolean = {
    val dim = ud.unitCategory.dimension
    dim.I == 0 && dim.J == 0 && dim.Θ == 0 && dim.N == 0
  }

  private def isElectromagneticUnit(ud: LinearUnitDefinitionJson): Boolean =
    ud.unitCategory.dimension.I != 0
}
