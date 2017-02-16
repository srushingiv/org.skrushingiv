package org.skrushingiv.repository.mongo

import java.time._

import reactivemongo.bson._
import scala.language.postfixOps

/**
 * 
 */
trait BasicBsonHandlers {

  implicit def mapHandler[B <: BSONValue, T](implicit r: BSONReader[B, T], w: BSONWriter[T, B], evidence: BSONReader[_ <: BSONValue, B]) =
    new BSONDocumentReader[Map[String, T]] with BSONDocumentWriter[Map[String, T]] with BSONHandler[BSONDocument, Map[String, T]] {
      def write(m: Map[String, T]) = BSONDocument(m.toStream.map { t ⇒ t._1 → w.write(t._2) })

      // assume that all values in the map are B
      def read(bson: BSONDocument) = bson.elements.map { case BSONElement(k, v) ⇒ k → r.read(v.seeAsTry[B].get) } toMap
    }

  implicit val zonedDateTimeFormatter = new BSONHandler[BSONLong, ZonedDateTime] {
    override def read(bson: BSONLong): ZonedDateTime = {
      val instant = Instant.ofEpochMilli(bson.value.toLong)
      ZonedDateTime.ofInstant(instant, ZoneId.of("UTC")).withFixedOffsetZone()
    }

    override def write(t: ZonedDateTime): BSONLong = {
      BSONLong(t.toInstant.toEpochMilli)
    }
  }

}