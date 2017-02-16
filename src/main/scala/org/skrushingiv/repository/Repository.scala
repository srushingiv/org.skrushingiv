package org.skrushingiv
package repository

import query._
import shapeless.tag
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

/**
 * The basic Repository knows how to find, count, insert, update and delete domain
 * entities from persistent storage.
 */
sealed trait BaseRepository { self: Repository =>
  import Repository._

  // -------- abstract functionality which implementations must provide --------

  /**
   * The domain entity type for which this repository is pertinent.
   */
  type Entity <: repository.Entity

  /**
   * A domain entity and its associated id.
   */
  final type SavableEntity = IdEntityPair[Entity]

  /**
   * A domain query type that implementations of this repository will know how to
   * internalize.
   * 
   * The intent, here, is that implementations of this pattern will define domain-specific
   * Algebraic Data Types (ADTs, sealed traits implemented by several case classes)
   * in order to support varied, complex, and/or composable queries.
   */
  type Query

  /**
   * The underlying database's interface for querying for objects.
   * In Mongo, this would translate to a BSONDocument.
   * 
   * Generic repository definitions should not provide this type, but
   * should leave it to the concrete storage-system-specific implementation
   * to define.
   */
  protected type InternalQuery

  /**
   * Internalizes a domain query into one the storage medium understands.
   * 
   * Generic repository definitions should not provide this function, but
   * should leave it to the concrete storage-system-specific implementation
   * to define.
   */
  protected implicit def internalize(q: Query): InternalQuery

  /**
   * Repositories should provide an execution context so that processing does not halt other threads.
   */
  protected implicit def ec: ExecutionContext

  /**
   * The generator for this repository's entity IDs.
   */
  protected implicit def idGenerator: IdGenerator[Entity#Id]

  /**
   * Finds matching objects in persistent storage, and returns them in an Iterator.
   * Use Iterator in order to avoid excessive memory consumption on large result
   * sets.
   */
  protected def find(query: InternalQuery, page: Option[Pagination], sort: Option[Sortable]): Future[Iterator[SavableEntity]]

  /**
   * Removes matching objects from persistent storage.
   */
  protected def remove(query: InternalQuery): Future[Unit]

  /**
   * Counts matching objects in persistent storage.
   */
  protected def count(query: InternalQuery): Future[Long]

  /**
   * Perists entity changes.
   */
  def save(entity: SavableEntity): Future[Unit]

  /**
   * Inserts entities into persistent storage and returns them paired with new IDs
   */
  def insert(entity: Seq[Entity]): Future[Seq[SavableEntity]]

  def findAll: Future[Iterator[SavableEntity]]

  def removeAll: Future[Unit]

  def countAll: Future[Long]
}

trait Repository extends BaseRepository {
  import Repository._

  // -------- derived functionality --------

  /**
   * Inserts a single entity into persistent storage
   */
  def insert(entity: Entity): Future[SavableEntity] = insert(Seq(entity)).map(_.head)

  /**
   * Finds objects in storage that match the domain query's criteria
   */
  def findBy(q: Query) = q match {
    case PageableQuery(page, sort) => find(q, Some(page), sort)
    case _ => find(q, None, None)
  }

  /**
   * Finds the first object in storage that matches the domain query's criteria
   */
  def findOneBy(q: Query) = findBy(q) map { i =>
    if (i.hasNext) Some(i.next) else None
  }

  /**
   * Removes objects in storage that match the domain query's criteria
   */
  def removeBy(q: Query) = remove(q)

  /**
   * Counts objects in storage that match the domain query's criteria
   */
  def countBy(q: Query) = count(q)

  /**
   * Usable in external contexts to lift raw IDs into tagged and validated IDs appropriate 
   * for the repository implementation.
   */
  final def liftId(id: Entity#Id): EntityId[Entity] = Id.lift[Entity](id)

}

object Repository {

  /**
   * A re-usable means of generating and validating IDs for Repositories.
   * 
   * ID Generators can be used across multiple repositories that share an ID
   * space. For example, in Mongo, all object IDs are unique, and therefore
   * any Entity Repository that uses the Mongo object IDs as the primary key
   * can use the same instance of an IdGenerator.
   * 
   * @tparam T The raw ID type.
   */
  trait IdGenerator[T] {

    def create: T

    def validate(rawId: T): T = rawId

  }

  object passthroughIdGenerator extends IdGenerator[Any] {
    override def create: Any = ???
    override def validate(rawId: Any): Any = rawId
  }

  /**
   * A convenience module for encapsulating ID creation and validation.
   */
  object Id {
    def apply[E <: Entity](implicit gen: IdGenerator[E#Id]): EntityId[E] =
      tag[E].apply[E#Id](gen.create)
  
    def lift[E <: Entity](rawId: E#Id)(implicit gen: IdGenerator[E#Id]): EntityId[E] =
      tag[E].apply[E#Id](gen.validate(rawId))
  }

}
