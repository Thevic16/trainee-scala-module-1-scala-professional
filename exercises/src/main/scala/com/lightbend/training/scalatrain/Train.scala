package com.lightbend.training.scalatrain

case class Station(name: String)

sealed abstract class TrainInfo{
  def number:Int
}

case class InterCityExpress(number:Int, hasWifi: Boolean = false) extends TrainInfo
case class RegionalExpress(number:Int) extends TrainInfo
case class BavarianRegional(number:Int) extends TrainInfo


case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]){
  require(schedule.size >=2, "Invalid schedule, must have a least 2 elements")

  val stations: Seq[Station] = schedule.map(_._2)

  def timeAt(station: Station): Option[Time] = {
    schedule.find {
      case (_, needle) => needle == station
    }.map{
      case (time, _) => time
    }
  }
}
