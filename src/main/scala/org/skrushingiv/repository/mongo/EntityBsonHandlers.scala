package org.skrushingiv.repository
package mongo

import reactivemongo.bson._

trait EntityBsonHandlers {

  // we can't make this implicit because then it would get in the way of the default BSONStringHandler

  object BSONObjectIDHandler extends BSONHandler[BSONObjectID, String] {
    def write(id: String): BSONObjectID = BSONObjectID.parse(id).get
    def read(bson: BSONObjectID): String = bson.stringify
  }

  def idFormat[E <: Entity](implicit evidence: E#Id <:< String): BSONHandler[BSONObjectID, EntityId[E]] =
    taggedIdHandler[E, BSONObjectID](BSONObjectIDHandler.asInstanceOf, BSONObjectIDHandler.asInstanceOf)

  def taggedIdHandler[E <: Entity, V <: BSONValue](implicit r: BSONReader[V, E#Id], w: BSONWriter[E#Id, V]) =
    new BSONHandler[V, EntityId[E]] {
      def write(id: EntityId[E]): V = w.write(id)
      def read(bson: V): EntityId[E] = r.read(bson).asInstanceOf[EntityId[E]]
    }

}
