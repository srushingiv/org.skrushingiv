package org.skrushingiv.repository

import scala.concurrent.Future
import scala.language.implicitConversions

/**
 * This mixin provides support for "upsert" behavior in repositories.
 * 
 * Ideal usage is that the a generic repository will incorporate this mixin,
 * use the `upsert` method to build storage-system-agnostic behavior, and then
 * implement the `upsert` method in the concrete storage-system-specific
 * implementation.
 */
trait Upsertable { self: Repository =>
  import Upsertable._

  type UpsertResult = (UpsertAction, SavableEntity)

  sealed trait UpsertMagnet
  case class Data(entity: Entity) extends UpsertMagnet
  case class Pair(entity: SavableEntity) extends UpsertMagnet

  object UpsertMagnet {
    implicit def dataMagnet(entity: Entity) = Data(entity)
    implicit def pairMagnet(entity: SavableEntity) = Pair(entity)
  }

  protected def upsert(query: InternalQuery, entity: UpsertMagnet): Future[UpsertResult]

}

object Upsertable {
  sealed trait UpsertAction
  case object Inserted extends UpsertAction
  case object Updated extends UpsertAction
}
