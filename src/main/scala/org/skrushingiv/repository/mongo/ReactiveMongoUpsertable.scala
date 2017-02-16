package org.skrushingiv.repository
package mongo

import query._
import reactivemongo.bson._
import scala.concurrent.Future
import scala.util.{Success, Failure}

/**
 * This mixin provides default functionality for upsertable behavior in mongo.
 */
trait ReactiveMongoUpsertable extends ReactiveMongoRepositoryT with Upsertable {

  override protected def upsert(query: InternalQuery, entity: UpsertMagnet): Future[UpsertResult] = {
    val e: Entity = entity match {
      case Data(data) ⇒ data
      case Pair(pair) ⇒ pair._2
    }
    val doc = entityHandler.asInstanceOf[BSONDocumentWriter[Entity]].write(e)
    for {
      c ← collection
      v ← c.update(query, doc, upsert = true)
      newEnt ← (entity, v.upserted.size) match {
        case (Pair(ent), 0) ⇒ Future.successful(ent)
        case (Data(data), 0) ⇒ this.find(query, Some(Pagination(1,1)), None).map(_.toSeq.head)
        case _               ⇒ v.upserted(0)._id.seeAsTry[EntityId[Entity]](idReader) match {
          case Success(id) => Future.successful(id -> e)
          case Failure(e) => Future.failed(e)
        }
      }
      action = if (v.n > v.nModified) Upsertable.Inserted else Upsertable.Updated
    } yield action -> newEnt
  }

}
