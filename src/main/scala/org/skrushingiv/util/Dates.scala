package org.skrushingiv.util

import org.joda.time.{ DateTime, Duration }
import scala.concurrent.duration.FiniteDuration

/**
 * This module provides a convenient syntax for creating Streams of DateTimes.
 * 
 * For syntactical sugar, it allows the use of `scala.concurrent.duration` postfix operators
 * for durations as well as joda-time durations.
 * 
 * Example:
 * 
 *     val every5Days = Dates.from(DateTime.now, 5 days)
 *     val untilChristmas = Dates.between(DateTime.now, DateTime.now.withMonthOfYear(12).withDayOfMonth(31))
 * 
 */
object Dates {

  /**
   * Creates a lazily evaluated infinite stream of datetimes separated by a step duration in milliseconds.
   * 
   * @param step duration between dates in milliseconds; defaults to 86400000 milliseconds (1 day)
   */
  def from(start: DateTime, step: Long = 86400000L): Stream[DateTime] = start #:: from(start.plus(step), step)

  /**
   * Creates a lazily evaluated stream of datetimes separated by a step duration.
   * 
   * This variation allows the use of Joda-Time Duration objects.
   */
  def from(start: DateTime, step: Duration): Stream[DateTime] = from(start, step.getMillis)

  /**
   * Creates a lazily evaluated stream of datetimes separated by a step duration.
   * 
   * This variation allows the use of scala.concurrent.duration postfix operators such as `3 minutes` or `2 days`
   */
  def from(start: DateTime, step: FiniteDuration): Stream[DateTime] = from(start, step.toMillis)

  /**
   * Creates a lazily evaluated stream of datetimes between start and end dates (inclusive) separated by a step
   * duration in milliseconds.
   * 
   * @param stepMillis defaults to 86400000 milliseconds, 1 day
   */
  def between(start: DateTime, end: DateTime, stepMillis: Long = 86400000L): Stream[DateTime] =
    from(start, stepMillis).takeWhile(!_.isAfter(end))

  /**
   * Creates a lazily evaluated stream of datetimes between start and end dates (inclusive) separated by a step duration.
   * 
   * This variation allows the use of Joda-Time Duration objects.
   */
  def between(start: DateTime, end: DateTime, step: Duration): Stream[DateTime] =
    between(start, end, step.getMillis)

  /**
   * Creates a lazily evaluated stream of datetimes between start and end dates (inclusive) separated by a step duration.
   * 
   * This variation allows the use of scala.concurrent.duration postfix operators such as `3 minutes` or `2 days`
   */
  def between(start: DateTime, end: DateTime, step: FiniteDuration): Stream[DateTime] =
    between(start, end, step.toMillis)

}
