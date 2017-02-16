package org.skrushingiv.repository
package mongo

import reactivemongo.bson._
import reactivemongo.bson.Producer._

object DSL {

  def $and(objs: BSONDocument*) =
    nameOptionValue2Producer("$and", objs.headOption.map(_ ⇒ BSONArray(objs.map(valueProducer(_)): _*)))

  def $or(objs: BSONDocument*) =
    nameOptionValue2Producer("$or", objs.headOption.map(_ ⇒ BSONArray(objs.map(valueProducer(_)): _*)))

  def $gte[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer("$gte", value)
  def $gt[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer("$gt", value)
  def $lt[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer("$lt", value)
  def $lte[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer("$lte", value)

  def $in[T](values: Seq[T])(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer("$in", values)
  def $in[T](values: Option[Seq[T]])(implicit writer: BSONWriter[T, _ <: BSONValue]) =
    values.map(objs ⇒ element2Producer("$in", BSONArray(objs.map(valueProducer(_)): _*)))

  implicit class StringHelper(val s: String) extends AnyVal {
    def $gte[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer(s, document(DSL.$gte(value)))
    def $gt[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer(s, document(DSL.$gt(value)))
    def $lt[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer(s, document(DSL.$lt(value)))
    def $lte[T](value: T)(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer(s, document(DSL.$lte(value)))
    def $in[T](values: Seq[T])(implicit writer: BSONWriter[T, _ <: BSONValue]) = element2Producer(s, document(DSL.$in(values)))
    def $in[T](values: Option[Seq[T]])(implicit writer: BSONWriter[T, _ <: BSONValue]) = nameOptionValue2Producer(s, DSL.$in(values).map(document(_)))
  }

}
