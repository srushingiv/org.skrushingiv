package org.skrushingiv.repository
package mongo

import query._
import reactivemongo.api.MongoConnection
import reactivemongo.bson._
import reactivemongo.api.BSONSerializationPack
import reactivemongo.api.commands.bson.BSONAggregationFramework._
import scala.concurrent.{ExecutionContext, Future}

case class AggregateTotal(total: Long)
object AggregateTotal {
  implicit val reader = Macros.reader[AggregateTotal]
}

trait AggregateQuerySupport extends ReactiveMongoRepositoryT {
  import AggregateQuerySupport._

  def requiresAggregation(q: Query): Boolean

  def buildPipeline(q: Query)(tail: Pipeline): Pipeline

  def pipeline(q: Query, shouldPage: Boolean, tail: Pipeline = Nil) = buildPipeline(q) {
    q match {
      case PageableQuery(Pagination(page, limit), sort) if shouldPage ⇒ {
        val $sort: List[PipelineOperator] = sort match {
          case Some(Sortable(field, Asc))  ⇒ Sort(Ascending(field)) :: tail
          case Some(Sortable(field, Desc)) ⇒ Sort(Descending(field)) :: tail
          case _                                   ⇒ tail
        }
        val $limit = if (limit < 0) $sort else (Limit(limit) :: $sort)
        if (page > 1 && limit > 0) (Skip(limit * (page - 1)) :: $limit) else $limit
      }
      case _ ⇒ tail
    }
  }

  def aggregate[T: BSONDocumentReader](pipeline: Pipeline) = for {
    c ← collection
    h :: t = pipeline
    v ← c.aggregate(h, t)
  } yield v.firstBatch.map(_.as[T]).iterator // TODO: there's an implied additional results here

  abstract override def countBy(q: Query): Future[Long] = {
    if (requiresAggregation(q))
      aggregate[AggregateTotal](pipeline(q, false, TotalPipeline)).map { _.toStream.headOption.map(_.total).getOrElse(0L) }
    else super.countBy(q)
  }

  abstract override def findBy(q: Query): Future[Iterator[SavableEntity]] = {
    if (requiresAggregation(q)) aggregate[SavableEntity](pipeline(q, true))
    else super.findBy(q)
  }

}

object AggregateQuerySupport {
  type Pipeline = List[PipelineOperator]
  val TotalPipeline: Pipeline = List(Group(BSONNull)("total" → SumAll))
}
