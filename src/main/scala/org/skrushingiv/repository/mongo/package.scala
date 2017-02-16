package org.skrushingiv.repository

import query._
import reactivemongo.bson._
import reactivemongo.api.{Cursor, QueryOpts}
import reactivemongo.api.collections.bson.BSONCollection
import scala.concurrent.{Future, ExecutionContext}

package object mongo
extends DefaultBSONHandlers
with BasicBsonHandlers
with EntityBsonHandlers
with GenericBsonHandlers {

  type BSONDocumentHandler[T] = BSONDocumentReader[T] with BSONDocumentWriter[T] with BSONHandler[BSONDocument, T]
  
  protected def format[C](r: BSONDocument ⇒ C, w: C ⇒ BSONDocument): BSONDocumentHandler[C] =
    new BSONDocumentReader[C] with BSONDocumentWriter[C] with BSONHandler[BSONDocument, C] {
      override def write(t: C): BSONDocument = w(t)
      override def read(bson: BSONDocument): C = r(bson)
    }

  protected def handler[B <: BSONValue, C](r: B ⇒ C, w: C ⇒ B): BSONHandler[B, C] =
    new BSONHandler[B, C] {
      override def write(t: C): B = w(t)
      override def read(bson: B): C = r(bson)
    }

  implicit object SortableWriter extends BSONDocumentWriter[Sortable] {
    override def write(sortable: Sortable): BSONDocument = BSONDocument(sortable.fieldName → (if (sortable.dir == Asc) 1 else -1))
  }

  implicit object SortableOptionWriter extends BSONDocumentWriter[Option[Sortable]] {
    override def write(o: Option[Sortable]) = o map SortableWriter.write getOrElse BSONDocument.empty
  }

  def paginateAndSort[Q](query: Q): (QueryOpts, Int, BSONDocument) = query match {
    case PageableQuery(p, sortable) ⇒
      (QueryOpts(skipN = (p.page - 1) * p.limit), if (p.limit == 0) Int.MaxValue else p.limit, BSON.writeDocument(sortable))
    case _ ⇒ (QueryOpts(), Int.MaxValue, BSONDocument.empty)
  }

  implicit class EnrichedFutureCollection(val c: Future[BSONCollection]) extends AnyVal {
    def find[Q: BSONDocumentWriter, E: BSONDocumentReader](query: Q, opts: (QueryOpts, Int, BSONDocument))(implicit ec: ExecutionContext) =
      c.flatMap { _
        .find(query)
        .options(opts._1)
        .sort(opts._3)
        .cursor[E]()
        .collect[Seq](opts._2, Cursor.ContOnError[Seq[E]]())
      }
  }

  implicit class EnrichedBSONHandler[B <: BSONValue, T1](val h: BSONHandler[B, T1]) extends AnyVal {
    def xmapDoc(r: BSONDocument ⇒ B, w: (B, T1) => BSONDocument) =
      format(bson => h.read(r(bson)), (t: T1) => w(h.write(t), t))

    def xmapDoc(r: BSONDocument ⇒ B, w: B ⇒ BSONDocument) =
      format(bson => h.read(r(bson)), (t: T1) => w(h.write(t)))

    def xmapBSON[C <: BSONValue](r: C => B, w: B => C) =
      handler[C, T1](bson => h.read(r(bson)), (t: T1) => w(h.write(t)))

    def xmap[X](r: T1 => X, w: X => T1) =
      handler[B, X](bson => r(h.read(bson)), (x: X) => h.write(w(x)))

  }

}
