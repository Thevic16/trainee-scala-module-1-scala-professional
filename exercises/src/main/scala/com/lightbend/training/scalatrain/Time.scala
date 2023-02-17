package com.lightbend.training.scalatrain
import play.api.libs.json._

import scala.util.{Try, Success, Failure}


case class Time(hours: Int = 0, minutes: Int = 0) extends Ordered[Time]{
  require(hours >=0 && hours < 24, "Invalid hours, must be >= 0 and < 24")
  require(minutes >=0 && minutes < 60, "Invalid minutes, must be >= 0 and < 60")

  val asMinutes: Int = hours * 60 + minutes

  def minus(that: Time): Int = asMinutes - that.asMinutes

  def -(that: Time): Int = minus(that)

  override lazy val toString: String = f"$hours%02d:$minutes%02d"

  override def compare(that: Time): Int = asMinutes - that.asMinutes


  def toJson: JsValue = {
    Json.obj(
      "hours" -> hours,
      "minutes" -> minutes
    )
  }

}

object Time{
  def fromMinutes(minutes: Int): Time = {
    val hours = cal_hours(minutes)
    val time: Time = Time(hours = hours, minutes = cal_minutes(minutes, hours))
    time
  }

  def cal_hours(minutes: Int): Int = {
    minutes / 60
  }

  def cal_minutes(minutes: Int, hours: Int): Int = {
    minutes - hours * 60
  }

  def fromJson(json: JsValue): Option[Time] = {
    val parseHours: Option[Int] = Try((json \ "hours").as[Int]) match {
      case Success(hours) => Some(hours)
      case Failure(_) => None
    }

    val parseMinutes: Int = Try((json \ "minutes").as[Int]) match {
      case Success(minutes) => minutes
      case Failure(_) => 0
    }

    if (parseHours.isDefined)
      Some(Time(parseHours.getOrElse(0), parseMinutes))
    else
      None

  }

}



