package org.skrushingiv.repository
package mongo

import query._
import Repository._
import reactivemongo.api._
import reactivemongo.api.collections.GenericQueryBuilder
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.UpdateWriteResult
import reactivemongo.bson._
import shapeless.{ :: , HNil }
import shapeless.tag._

import scala.collection.generic.CanBuildFrom
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.language.higherKinds
import scala.language.implicitConversions

case class Employee(name: String, manager: EntityId[Employee]) extends Entity {
  type Id = String
}
object Employee {
  //implicit val gen = LabelledGeneric[Employee]
}

case class EmployeeQuery(
  id: Option[EntityId[Employee]] = None,
  mgrId: Option[EntityId[Employee]] = None
) {
  def withId(id: EntityId[Employee]) = copy(id = Some(id))
  def withManagerId(mgrId: EntityId[Employee]) = copy(mgrId = Some(mgrId))
}

object EmployeeQuery extends EmployeeQuery(None, None) {
  //implicit val gen = LabelledGeneric[EmployeeQuery]
}

object test {
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val mc: MongoConnection = ???
  implicit val empIdHandler = idFormat[Employee]
  implicit val eh: BSONDocumentHandler[Employee] = shapeless.cachedImplicit
  implicit val qh: BSONDocumentHandler[EmployeeQuery] = shapeless.cachedImplicit
  
  val employeeRepo = new ReactiveMongoRepository[Employee, BSONObjectID]("db","emp")
  {
    //implicit val queryHandler: BSONFormats[EmployeeQuery] = shapeless.cachedImplicit
    override type Query = EmployeeQuery
    override protected implicit def internalize(q: EmployeeQuery): InternalQuery = BSON.write(q)
  }
}



/**
 * Mongo Implementation of [[org.skrushingiv.repository.Repository]]
 *
 * Create Entity
 * {{{
 *      case class Employee(name: String, manager: EntityId[Employee]) extends Entity {
 *        type Id = String
 *      }
 * }}}
 * 
 * Create Query ADT
 * {{{
 *      sealed trait EmployeeQuery
 *      
 *      case object EmployeeEmptyQuery extends EmployeeQuery
 * 
 *      case class EmployeeIdQuery(id: EntityId[Employee]) extends EmployeeQuery
 * 
 *      case class EmployeeManagerQuery(mgrId: EntityId[Employee]) extends EmployeeQuery
 *      
 *      // NOTE: This example is not composable, but could be made to be.
 * }}}
 * 
 * ... or Query prototype
 * {{{
 *      case class EmployeeQuery(
 *        id: Option[EntityId[Employee]] = None,
 *        mgrId: Option[EntityId[Employee]] = None
 *      ) {
 *        def withId(id: EntityId[Employee]) = copy(id = Some(id))
 *        def withManagerId(mgrId: EntityId[Employee]) = copy(mgrId = Some(mgrId))
 *      }
 *      
 *      object EmployeeQuery extends EmployeeQuery(None, None)
 * }}}
 *
 * Create a concrete implementation of [[org.skrushingiv.repository.mongo.ReactiveMongoRepository]]
 *
 * {{{
 *      implicit val empIdHandler = idFormat[Employee]
 *      implicit val eh: BSONDocumentHandler[Employee] = shapeless.cachedImplicit
 *      implicit val qh: BSONDocumentHandler[EmployeeQuery] = shapeless.cachedImplicit
 *      
 *      val employeeRepo = new ReactiveMongoRepository[Employee, BSONObjectID]("db","emp")
 *      {
 *        override type Query = EmployeeQuery
 *        override protected implicit def internalize(q: EmployeeQuery): InternalQuery = BSON.write(q)
 *      }
 * }}}
 *
 *
 * @param dbName Mongo Database Name
 * @param collectionName Mongo Collection Name
 * @param mongoConnection Mongo Connection
 * @param ec ExecutionContext
 * @param entityFormats Ensures the E#Id for a Mongo Entity is {{{ String @@ Of[E] }}}
 * @tparam E Entity type
 * @tparam I Persisted Entity ID BSON value type
 *
 * @see test com.zco.esports.core.repository.ReactiveMongoRepoSpec
 */
abstract class ReactiveMongoRepository[E <: Entity, I <: BSONValue](val dbName: String, val collectionName: String)(implicit
    protected val mongoConnection: MongoConnection,
    protected val ec: ExecutionContext,
    protected val entityHandler: BSONDocumentHandler[E],
    protected val idWriter: BSONWriter[EntityId[E], I],
    protected val idReader: BSONReader[I, EntityId[E]],
    protected val idGenerator: IdGenerator[E#Id] = ReactiveMongoRepository.defaultGenerator[E#Id].asInstanceOf
) extends ReactiveMongoRepositoryT {
  override protected type IdBsonType = I
  override type Entity = E

  override lazy val collection =
    mongoConnection.database(dbName).map(_.collection(collectionName))
}



private[mongo] trait ReactiveMongoRepositoryT extends Repository with AllQueriesFromInternal {
  protected type IdBsonType <: BSONValue
  protected val mongoConnection: MongoConnection
  protected implicit val ec: ExecutionContext
  protected implicit val entityHandler: BSONDocumentHandler[Entity]
  protected implicit val idWriter: BSONWriter[EntityId[Entity], IdBsonType]
  protected implicit val idReader: BSONReader[IdBsonType, EntityId[Entity]]
  protected implicit val idGenerator: IdGenerator[Entity#Id]

  import ReactiveMongoRepository._

  override protected type InternalQuery = BSONDocument
  override val allQuery: InternalQuery = BSONDocument.empty

  def collection: Future[BSONCollection]

  protected implicit val idFormats: BSONDocumentHandler[EntityId[Entity]] = idDocHandler[Entity, IdBsonType]
  protected implicit val savableEntityFormats: BSONDocumentHandler[SavableEntity] = pairHandler[Entity, IdBsonType]

  /**
   * This method will be called *before* every insert. As such, implementations
   * should ensure that all generated entities have IDs created for them.
   */
  def toSavable(data: Entity): SavableEntity = Id[Entity](idGenerator) -> data

  override protected def find(query: InternalQuery, page: Option[Pagination], sort: Option[Sortable]): Future[Iterator[SavableEntity]] = for {
    c ← collection
    v ← c.find(query).sort(sort).paginate[SavableEntity, Iterator](page)
  } yield v

  override protected def remove(q: InternalQuery): Future[Unit] = for {
    c ← collection
    _ ← c.remove(q)
  } yield ()

  override def save(entity: SavableEntity): Future[Unit] = for {
    c ← collection
    _ ← c.update(idFormats.write(entity.id), entity)
  } yield ()

  @inline def insertOne(entityData: Entity): Future[SavableEntity] = {
    val entity = toSavable(entityData)
    for {
      c ← collection
      _ ← c.insert(entity)
    } yield entity
  }

  override def insert(eds: Seq[Entity]): Future[Seq[SavableEntity]] = 
    if (eds.size < 1) Future.successful(Seq.empty)
    else if (eds.size == 1) insertOne(eds(0)).map(Seq(_))
    else {
      val entities = eds.map(toSavable)
      // can't use for comprehension here, because the compiler can't deal with the path-dependent type
      collection flatMap { c ⇒
        val bulkDocs = entities.map(implicitly[c.ImplicitlyDocumentProducer](_))
        Failover2(mongoConnection, FailoverStrategy()) { () ⇒
          c.bulkInsert(ordered = false)(bulkDocs: _*)
        }.future
      } map (_ ⇒ entities)
    }

  override def count(query: InternalQuery): Future[Long] =
    collection.flatMap {
      // Concern: what happens if there are more than MAXINT records in the collection?
      _.count(if (query.isEmpty) None else Some(query)) map { i => i:Long }
    }

}

object ReactiveMongoRepository {

  // provided for default String id generation where each Id must parse as a BSONObjectID
  implicit val objectIdGenerator = new IdGenerator[String] {
    override def create: String = BSONObjectID.generate.stringify
    override def validate(rawId: String): String = BSONObjectID.parse(rawId).get.stringify //Boom if parse fails
  }

  // provided for determining a default idGenerator if no implicit is available during creation of a repository.
  implicit def passthroughGenerator[T]: IdGenerator[T] = Repository.passthroughIdGenerator.asInstanceOf[IdGenerator[T]]
  
  def defaultGenerator[T] = implicitly[IdGenerator[T]]

  implicit class EnrichedQueryBuilder(val qb: GenericQueryBuilder[BSONSerializationPack.type]) extends AnyVal {
    def skip(pagination: Option[Pagination]) = pagination match {
      case Some(p) => qb.options(QueryOpts(skipN = (p.page - 1) * p.limit))
      case None => qb
    }

    def paginate[T: BSONDocumentReader, M[_]](pagination: Option[Pagination])(implicit cbf: CanBuildFrom[M[_], T, M[T]], ec: ExecutionContext) = {
      val limit = pagination.map(_.limit) getOrElse -1
      qb.skip(pagination)
        .cursor[T]()
        .collect[M](limit, Cursor.ContOnError[M[T]]())
    }

    def sort(sortable: Option[Sortable])(implicit sortWriter: BSONDocumentWriter[Sortable]) = sortable match {
      case Some(s) => qb.sort(qb.pack.serialize(s, sortWriter))
      case None => qb
    }
  }

  def idDocHandler[E <: Entity, V <: BSONValue](implicit r: BSONReader[V, EntityId[E]], w: BSONWriter[EntityId[E], V]) =
    new BSONDocumentReader[EntityId[E]] with BSONDocumentWriter[EntityId[E]] with BSONHandler[BSONDocument, EntityId[E]] {
      override def write(id: EntityId[E]): BSONDocument = BSONDocument("_id" → w.write(id))
      override def read(doc: BSONDocument): EntityId[E] = doc.getAsTry[EntityId[E]]("_id").get
    }

  def pairHandler[E <: Entity, V <: BSONValue](implicit
      f: BSONDocumentHandler[E],
      idR: BSONReader[V, EntityId[E]],
      idW: BSONWriter[EntityId[E], V]) =
    new BSONDocumentReader[IdEntityPair[E]] with BSONDocumentWriter[IdEntityPair[E]] with BSONHandler[BSONDocument, IdEntityPair[E]] {
      override def write(pair: IdEntityPair[E]): BSONDocument = BSONDocument("_id" → idW.write(pair.id)) ++ f.write(pair._2)
      override def read(doc: BSONDocument): IdEntityPair[E] = doc.getAsTry[EntityId[E]]("_id").get -> f.read(doc)
    }
}

