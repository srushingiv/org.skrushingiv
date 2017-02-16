package org.skrushingiv.repository.query

trait PageableQuery {

  def pagination: Pagination
  def sortable: Option[Sortable]

}

object PageableQuery {
  def unapply(q: PageableQuery): Option[(Pagination, Option[Sortable])] = Some((q.pagination, q.sortable))
}
