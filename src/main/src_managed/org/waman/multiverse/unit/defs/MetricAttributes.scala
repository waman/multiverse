package org.waman.multiverse.unit.defs

sealed trait square_linkAttribute
sealed trait square_footAttribute
sealed trait square_chainAttribute
sealed trait square_rodAttribute
sealed trait square_mileAttribute
sealed trait acreAttribute
sealed trait sectionAttribute
sealed trait xunitAttribute
sealed trait linkAttribute
sealed trait footAttribute
sealed trait rodAttribute
sealed trait chainAttribute
sealed trait mileAttribute
sealed trait cableAttribute
sealed trait leagueAttribute
sealed trait furlongAttribute
sealed trait nautical_mileAttribute
sealed trait minimAttribute
sealed trait fluid_ounceAttribute
sealed trait gillAttribute
sealed trait cupAttribute
sealed trait pottleAttribute
sealed trait gallonAttribute
sealed trait bushelAttribute
sealed trait hogsheadAttribute
sealed trait fluid_dramAttribute
sealed trait pintAttribute
sealed trait quartAttribute
sealed trait peckAttribute
sealed trait barrelAttribute

object MetricAttributes{
  final object MoKα1 extends xunitAttribute
  final object Adm extends nautical_mileAttribute
  final object US extends square_linkAttribute with square_footAttribute with square_chainAttribute with square_rodAttribute with square_mileAttribute with acreAttribute with sectionAttribute with linkAttribute with footAttribute with rodAttribute with chainAttribute with mileAttribute with cableAttribute with leagueAttribute with furlongAttribute with minimAttribute with fluid_ounceAttribute with gillAttribute with cupAttribute with pottleAttribute with gallonAttribute with bushelAttribute with hogsheadAttribute with fluid_dramAttribute
  final object US_fl extends pintAttribute with quartAttribute with gallonAttribute with barrelAttribute
  final object US_dry extends pintAttribute with quartAttribute with gallonAttribute with peckAttribute with barrelAttribute
  final object CuKα1 extends xunitAttribute
  final object metric extends cupAttribute
  final object imp extends cableAttribute with minimAttribute with fluid_ounceAttribute with gillAttribute with pintAttribute with quartAttribute with pottleAttribute with gallonAttribute with peckAttribute with bushelAttribute with barrelAttribute with hogsheadAttribute with fluid_dramAttribute
  final object US_lvl extends bushelAttribute
}