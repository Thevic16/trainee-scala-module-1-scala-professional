package com.lightbend.training.scalatrain

case class JourneyPlanner(trains: Set[Train]) {
  val stations: Set[Station] = trains.flatMap(_.stations)

  def trainsAt(station: Station): Set[Train] = trains.filter(_.stations.contains(station))

  def stopsAt(station: Station): Set[(Time, Train)] =
    for {
      train <- trains
      // timeAdnStation <- train.schedule if timeAdnStation._2 == station
      //(time, candidateStation) <- train.schedule if candidateStation == station
      time <- train.timeAt(station)
    } yield time -> train

  /*
  def isShortTrip(from : Station, to: Station): Boolean = {
    trains.exists(train =>
      train.stations.dropWhile(station =>
        station != from).drop(1).take(2).contains(to))
  }
   */

  def isShortTrip(from: Station, to: Station): Boolean ={
    trains.exists(train => train.stations.dropWhile(station => station != from) match {
        case `from` +: `to` +: _ => true
        case `from` +: _ +: `to` +: _ => true
        case _ => false
      })
  }

}
